package spatial.index

import data.Point
import data.Point.Embedding
import utils.Distances.euclideanDistanceSquared

import scala.collection.mutable.ArrayBuffer
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.concurrent.duration._

/** Parallel hyper-quadtree (generalized quadtree for d dimensions). Key characteristics:
  *   - Points stored in one global (reordered) array; each node references a contiguous slice (start, length).
  *   - Integer (counting) sort on child indices from 0 up to (2 to the power d) minus 1 groups points contiguously.
  *   - Child subtrees built in parallel above a configurable size threshold.
  *   - Path compression: keeps subdividing a region mapping all points to one child until either (a) at least two
  *     non-empty children, (b) leaf capacity reached, or (c) bounds are degenerate.
  *   - Mutates the provided points array for spatial locality.
  */
class ParQuadTree(
    val points: Array[Point],
    leafCapacity: Int = 32,
    parallelThreshold: Int = 2048
)(implicit ec: ExecutionContext)
    extends Serializable
    with SpatialTree[ParQuadTree] {

  require(points.nonEmpty, "Cannot build ParallelQuadTree with empty points")

  private val dimensions = points.head.dim

  private val numChildren = 1 << dimensions // 2^d

  case class Node(
      minBounds: Embedding,
      maxBounds: Embedding,
      start: Int,
      length: Int,
      isLeaf: Boolean,
      children: Array[Node] // size numChildren or empty for leaf
  )

  @volatile private var root: Node = _

  def build(): ParQuadTree = {
    // Compute global bounds once
    val (gMin, gMax) = computeBounds(0, points.length)
    root = buildNode(gMin, gMax, 0, points.length)
    this
  }

  // SpatialTree requirement
  override def kNearestNeighbors(queryPoint: Embedding, k: Int): Array[(Point, Float)] = {
    if (k <= 0 || root == null) return Array.empty
    val pq = scala.collection.mutable.PriorityQueue.empty[(Double, Point)](Ordering.by[(Double, Point), Double](_._1))
    knn(root, queryPoint, k, pq)
    val dequeued: Seq[(Double, Point)] = pq.dequeueAll.reverse
    dequeued.iterator.map { case (d2: Double, p: Point) => (p, math.sqrt(d2).toFloat) }.toArray
  }

  override def rangeQuery(anchorPoint: Embedding, range: Float): Array[Point] = {
    if (root == null) return Array.empty
    val rangeSq   = range.toDouble * range.toDouble
    val out       = ArrayBuffer.empty[Point]
    val searchMin = Array.ofDim[Float](dimensions)
    val searchMax = Array.ofDim[Float](dimensions)
    var d         = 0
    while (d < dimensions) { searchMin(d) = anchorPoint(d) - range; searchMax(d) = anchorPoint(d) + range; d += 1 }
    rangeRec(root, anchorPoint, rangeSq, searchMin, searchMax, out)
    out.toArray
  }

  override def rangeQuery(anchorPoint: Point, range: Float): Array[Point] = rangeQuery(anchorPoint.vector, range)

  /** Returns total number of nodes (for diagnostics). */
  def totalNodes: Int = {
    def count(n: Node): Int =
      if (n == null) 0 else 1 + (if (n.isLeaf) 0 else n.children.filter(_ != null).map(count).sum)
    if (root == null) 0 else count(root)
  }

  private def buildNode(minB: Embedding, maxB: Embedding, start: Int, length: Int): Node = {
    if (length <= leafCapacity || isDegenerate(minB, maxB)) {
      return Node(minB, maxB, start, length, isLeaf = true, children = Array.empty)
    }

    var curMin                         = minB
    var curMax                         = maxB
    var childCounts: Array[Int]        = null
    var childIndexPerPoint: Array[Int] = null
    var nonEmptyChildren               = 0
    var continue                       = true
    var attempt                        = 0

    while (continue) {
      val centers = midPoints(curMin, curMax)
      childIndexPerPoint = new Array[Int](length)
      childCounts = Array.fill(numChildren)(0)
      var i = 0
      while (i < length) {
        val idx = childIndex(points(start + i).vector, centers)
        childIndexPerPoint(i) = idx
        childCounts(idx) += 1
        i                += 1
      }
      nonEmptyChildren = childCounts.count(_ > 0)
      val allInOne  = nonEmptyChildren == 1
      val belowLeaf = length <= leafCapacity
      val deg       = isDegenerate(curMin, curMax)
      if (allInOne && !belowLeaf && !deg) {
        val onlyChild    = childCounts.indexWhere(_ > 0)
        val (nMin, nMax) = childBounds(onlyChild, curMin, curMax, centers)
        val shrinks      = !boundsEqual(curMin, nMin) || !boundsEqual(curMax, nMax)
        if (shrinks) { curMin = nMin; curMax = nMax }
        else continue = false
      } else continue = false
      attempt += 1
      if (attempt > 64) continue = false
    }

    if (nonEmptyChildren <= 1) {
      return Node(curMin, curMax, start, length, isLeaf = true, children = Array.empty)
    }

    // Counting sort to group children contiguously
    val prefix = new Array[Int](numChildren)
    var c      = 1
    while (c < numChildren) { prefix(c) = prefix(c - 1) + childCounts(c - 1); c += 1 }
    val temp = new Array[Point](length)
    var i    = 0
    while (i < length) {
      val idx = childIndexPerPoint(i)
      val pos = prefix(idx)
      temp(pos) = points(start + i)
      prefix(idx) += 1
      i           += 1
    }
    System.arraycopy(temp, 0, points, start, length)

    // Rebuild prefix as starting offsets
    var accum = 0; c = 0
    while (c < numChildren) { val cnt = childCounts(c); prefix(c) = accum; accum += cnt; c += 1 }

    val childrenArr  = Array.fill[Node](numChildren)(null)
    val centersFinal = midPoints(curMin, curMax)

    val buildFutures = ArrayBuffer.empty[Future[(Int, Node)]]
    c = 0
    while (c < numChildren) {
      val cnt = childCounts(c)
      if (cnt > 0) {
        val childStart        = start + prefix(c)
        val (cMin, cMax)      = childBounds(c, curMin, curMax, centersFinal)
        val buildSequentially = cnt <= leafCapacity || cnt < parallelThreshold
        if (buildSequentially) {
          childrenArr(c) = buildNode(cMin, cMax, childStart, cnt)
        } else {
          buildFutures += Future((c, buildNode(cMin, cMax, childStart, cnt)))
        }
      }
      c += 1
    }

    if (buildFutures.nonEmpty) {
      val built = Await.result(Future.sequence(buildFutures.toSeq), Duration.Inf)
      built.foreach { case (idx, node) => childrenArr(idx) = node }
    }

    Node(curMin, curMax, start, length, isLeaf = false, children = childrenArr)
  }

  private def computeBounds(start: Int, length: Int): (Embedding, Embedding) = {
    val minB = Array.fill(dimensions)(Float.MaxValue)
    val maxB = Array.fill(dimensions)(Float.MinValue)
    var i    = 0
    while (i < length) {
      val p = points(start + i)
      var d = 0
      while (d < dimensions) {
        val v = p.vector(d)
        if (v < minB(d)) minB(d) = v
        if (v > maxB(d)) maxB(d) = v
        d += 1
      }
      i += 1
    }
    (minB, maxB)
  }

  private def midPoints(minB: Embedding, maxB: Embedding): Embedding = {
    val c = new Embedding(dimensions); var d = 0
    while (d < dimensions) { c(d) = (minB(d) + maxB(d)) * 0.5f; d += 1 }; c
  }

  private def childIndex(vec: Embedding, center: Embedding): Int = {
    var idx = 0; var d = 0
    while (d < dimensions) { if (vec(d) > center(d)) idx |= (1 << d); d += 1 }; idx
  }

  private def childBounds(
      index: Int,
      parentMin: Embedding,
      parentMax: Embedding,
      center: Embedding
  ): (Embedding, Embedding) = {
    val minB = new Embedding(dimensions); val maxB = new Embedding(dimensions); var d = 0
    while (d < dimensions) {
      if ((index & (1 << d)) == 0) { minB(d) = parentMin(d); maxB(d) = center(d) }
      else { minB(d) = center(d); maxB(d) = parentMax(d) }
      d += 1
    }
    (minB, maxB)
  }

  private def boundsEqual(a: Embedding, b: Embedding): Boolean = {
    var d = 0; while (d < dimensions) { if (a(d) != b(d)) return false; d += 1 }; true
  }

  private def isDegenerate(minB: Embedding, maxB: Embedding): Boolean = {
    var d = 0; while (d < dimensions) { if (minB(d) != maxB(d)) return false; d += 1 }; true
  }

  private def boxIntersects(aMin: Embedding, aMax: Embedding, bMin: Embedding, bMax: Embedding): Boolean = {
    var d = 0
    while (d < dimensions) {
      if (aMin(d) > bMax(d) || aMax(d) < bMin(d)) return false
      d += 1
    }
    true
  }

  private def boxDistSq(p: Embedding, minB: Embedding, maxB: Embedding): Double = {
    var sum = 0.0
    var d   = 0
    while (d < dimensions) {
      val v    = p(d)
      var diff = 0.0
      if (v < minB(d)) diff = v - minB(d) else if (v > maxB(d)) diff = v - maxB(d)
      sum += diff * diff
      d   += 1
    }
    sum
  }

  private def knn(
      node: Node,
      query: Embedding,
      k: Int,
      pq: scala.collection.mutable.PriorityQueue[(Double, Point)]
  ): Unit = {
    if (node == null) return
    val maxDistSq = if (pq.size == k) pq.head._1 else Double.MaxValue
    if (boxDistSq(query, node.minBounds, node.maxBounds) > maxDistSq) return
    if (node.isLeaf) {
      var i = 0
      while (i < node.length) {
        val p  = points(node.start + i)
        val d2 = euclideanDistanceSquared(query, p.vector)
        if (pq.size < k) pq.enqueue((d2, p)) else if (d2 < pq.head._1) { pq.dequeue(); pq.enqueue((d2, p)) }
        i += 1
      }
    } else {
      val ordered = node.children.filter(_ != null).sortBy(n => boxDistSq(query, n.minBounds, n.maxBounds))
      var i       = 0
      while (i < ordered.length) { knn(ordered(i), query, k, pq); i += 1 }
    }
  }

  private def rangeRec(
      node: Node,
      query: Embedding,
      rangeSq: Double,
      searchMin: Embedding,
      searchMax: Embedding,
      out: ArrayBuffer[Point]
  ): Unit = {
    if (node == null) return
    if (!boxIntersects(node.minBounds, node.maxBounds, searchMin, searchMax)) return
    if (node.isLeaf) {
      var i = 0
      while (i < node.length) {
        val p = points(node.start + i)
        if (euclideanDistanceSquared(query, p.vector) <= rangeSq) out += p
        i                                                             += 1
      }
    } else {
      var i = 0
      while (i < numChildren) {
        val ch = node.children(i); if (ch != null) rangeRec(ch, query, rangeSq, searchMin, searchMax, out); i += 1
      }
    }
  }

}
