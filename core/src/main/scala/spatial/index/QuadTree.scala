package spatial.index

import data.Point
import data.Point.Embedding
import utils.Distances.euclideanDistanceSquared

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

class QuadTreeNode(
    val minBounds: Embedding,
    val maxBounds: Embedding,
    numChildren: Int
) {

  val children: Array[QuadTreeNode] = Array.fill(numChildren)(null)

  val elements: ArrayBuffer[Point] = ArrayBuffer.empty[Point]

  var isLeaf: Boolean = true

  var size: Int = 0

}

class QuadTree(
    val points: ArrayBuffer[Point],
    leafCapacity: Int = 32,
    maxDepth: Int = 20,
    minPointsForSplit: Int = 2
) extends Serializable
    with SpatialTree[QuadTree] {

  def this(points: Array[Point]) = this(ArrayBuffer.from(points))

  private val dimensions = if (points.isEmpty) 0 else points.head.dim

  private val numChildren = 1 << dimensions // 2^N

  private lazy val root: QuadTreeNode = buildTree()

  def build(): QuadTree = {
    if (points.isEmpty) {
      throw new IllegalArgumentException("Cannot build QuadTree with empty points")
    }
    buildTree()
    this
  }

  private def buildTree(): QuadTreeNode = {
    if (points.isEmpty) {
      return null
    }

    // Optimize bounds calculation - single pass instead of multiple map operations
    val minBounds = Array.fill(dimensions)(Float.MaxValue)
    val maxBounds = Array.fill(dimensions)(Float.MinValue)

    var i = 0
    while (i < points.length) {
      val point = points(i)
      var d     = 0
      while (d < dimensions) {
        val coord = point.vector(d)
        if (coord < minBounds(d)) minBounds(d) = coord
        if (coord > maxBounds(d)) maxBounds(d) = coord
        d += 1
      }
      i += 1
    }

    val rootNode = new QuadTreeNode(minBounds, maxBounds, numChildren)
    build(rootNode, points.to(ArrayBuffer), 0)
    rootNode
  }

  def build(node: QuadTreeNode, nodePoints: ArrayBuffer[Point], depth: Int): Unit = {
    node.size = nodePoints.size

    if (nodePoints.size <= leafCapacity || depth >= maxDepth || nodePoints.size < minPointsForSplit) {
      node.isLeaf = true
      node.elements ++= nodePoints
      return
    }

    node.isLeaf = false
    val center = Array.ofDim[Float](dimensions)
    for (i <- 0 until dimensions)
      center(i) = (node.minBounds(i) + node.maxBounds(i)) / 2.0f

    val childPoints = Array.fill(numChildren)(new ArrayBuffer[Point]())

    // Partition points into the 2^N children.
    for (p <- nodePoints) {
      val childIndex = getChildIndex(p.vector, center) // use vector-based getChildIndex
      childPoints(childIndex) += p
    }

    // Create and recursively build child nodes.
    for (i <- 0 until numChildren) {
      if (childPoints(i).nonEmpty) {
        val (childMin, childMax) = calculateChildBounds(i, node.minBounds, node.maxBounds, center)
        node.children(i) = new QuadTreeNode(childMin, childMax, numChildren)
        build(node.children(i), childPoints(i), depth + 1)
      }
    }
  }

  private def getChildIndex(pointVector: Embedding, center: Embedding): Int = {
    var index = 0
    for (d <- 0 until dimensions) {
      if (pointVector(d) > center(d)) {
        index |= (1 << d)
      }
    }
    index
  }

  private def calculateChildBounds(
      index: Int,
      parentMin: Embedding,
      parentMax: Embedding,
      center: Embedding
  ): (Embedding, Embedding) = {
    val minB = Array.ofDim[Float](dimensions)
    val maxB = Array.ofDim[Float](dimensions)
    for (d <- 0 until dimensions) {
      if ((index & (1 << d)) == 0) { // The d-th bit is 0
        minB(d) = parentMin(d)
        maxB(d) = center(d)
      } else { // The d-th bit is 1
        minB(d) = center(d)
        maxB(d) = parentMax(d)
      }
    }
    (minB, maxB)
  }

  override def rangeQuery(queryPoint: Point, radius: Float): Array[Point] = {
    val (results: ArrayBuffer[Point], radiusSq: Double, searchBoxMin: Array[Float], searchBoxMax: Array[Float]) =
      computeSearchBoxes(queryPoint, radius)

    searchRecursive(root, queryPoint, radiusSq, searchBoxMin, searchBoxMax, results)
    results.toArray
  }

  private def computeSearchBoxes(queryPoint: Point, radius: Float) = {
    val results  = new ArrayBuffer[Point]()
    val radiusSq = radius.toDouble * radius.toDouble

    // Pre-allocate search box arrays for better performance
    val searchBoxMin = Array.ofDim[Float](dimensions)
    val searchBoxMax = Array.ofDim[Float](dimensions)
    var i            = 0
    while (i < dimensions) {
      searchBoxMin(i) = queryPoint.vector(i) - radius
      searchBoxMax(i) = queryPoint.vector(i) + radius
      i += 1
    }
    (results, radiusSq, searchBoxMin, searchBoxMax)
  }

  def rangeQuery(queryVector: Embedding, radius: Float): Array[Point] =
    rangeQuery(Point(queryVector, -1L), radius)

  def hasNeighborInRange(queryVector: Embedding, radius: Float): Boolean = {
    val (results: ArrayBuffer[Point], radiusSq: Double, searchBoxMin: Array[Float], searchBoxMax: Array[Float]) =
      computeSearchBoxes(Point(queryVector, -1L), radius)

    searchRecursive(
      root,
      Point(queryVector, -1L),
      radiusSq,
      searchBoxMin,
      searchBoxMax,
      ArrayBuffer.empty[Point],
      earlyTerminate = true
    )
  }

  private def searchRecursive(
      node: QuadTreeNode,
      queryPoint: Point,
      radiusSq: Double,
      searchBoxMin: Embedding,
      searchBoxMax: Embedding,
      results: ArrayBuffer[Point],
      earlyTerminate: Boolean = false
  ): Boolean = {
    if (node == null || !boxIntersects(node.minBounds, node.maxBounds, searchBoxMin, searchBoxMax)) {
      return false
    }

    if (node.isLeaf) {
      var i            = 0
      val elementsSize = node.elements.length
      while (i < elementsSize) {
        val p = node.elements(i)
        if (euclideanDistanceSquared(queryPoint.vector, p.vector) <= radiusSq) {
          results += p
          // Early termination check
          if (earlyTerminate) {
            return true
          }
        }
        i += 1
      }
    } else {
      var i = 0
      while (i < numChildren) {
        val child = node.children(i)
        if (child != null) {
          val found = searchRecursive(child, queryPoint, radiusSq, searchBoxMin, searchBoxMax, results, earlyTerminate)
          if (found && earlyTerminate) {
            return true
          }
        }
        i += 1
      }
    }
    false
  }

  override def kNearestNeighbors(queryPoint: Embedding, k: Int): Array[(Point, Float)] = {
    val pq = mutable.PriorityQueue.empty[(Double, Point)](Ordering.by[(Double, Point), Double](_._1))
    knnRecursive(root, queryPoint, k, pq)
    val dequeued: Seq[(Double, Point)] = pq.dequeueAll.reverse
    dequeued.map { case (distSq, point) =>
      (point, math.sqrt(distSq).toFloat)
    }.toArray
  }

  private def knnRecursive(
      node: QuadTreeNode,
      queryPoint: Embedding,
      k: Int,
      pq: mutable.PriorityQueue[(Double, Point)]
  ): Unit = {
    if (node == null) return

    val maxDistSq = if (pq.size == k) pq.head._1 else Double.MaxValue
    if (boxDistSq(queryPoint, node.minBounds, node.maxBounds) > maxDistSq) {
      return
    }

    if (node.isLeaf) {
      for (p <- node.elements) {
        val dSq = euclideanDistanceSquared(queryPoint, p.vector)
        if (pq.size < k) {
          pq.enqueue((dSq, p))
        } else if (dSq < pq.head._1) {
          pq.dequeue()
          pq.enqueue((dSq, p))
        }
      }
    } else {
      val sortedChildren =
        node.children.filter(_ != null).sortBy(c => boxDistSq(queryPoint, c.minBounds, c.maxBounds))
      for (child <- sortedChildren)
        knnRecursive(child, queryPoint, k, pq)
    }
  }

  private def boxIntersects(min1: Embedding, max1: Embedding, min2: Embedding, max2: Embedding): Boolean = {
    var i = 0
    while (i < dimensions) {
      if (min1(i) > max2(i) || max1(i) < min2(i)) {
        return false
      }
      i += 1
    }
    true
  }

  private def boxDistSq(p: Embedding, minBounds: Embedding, maxBounds: Embedding): Double = {
    var sum = 0.0
    var i   = 0
    while (i < dimensions) {
      var d = 0.0
      if (p(i) < minBounds(i)) d = p(i) - minBounds(i)
      else if (p(i) > maxBounds(i)) d = p(i) - maxBounds(i)
      sum += d * d
      i   += 1
    }
    sum
  }

}
