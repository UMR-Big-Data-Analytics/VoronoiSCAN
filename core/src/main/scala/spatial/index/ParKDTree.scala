package spatial.index

import data.Point
import data.Point.Embedding
import utils.Distances.euclideanDistanceSquared

import java.util.concurrent.ConcurrentHashMap
import scala.collection.mutable.ArrayBuffer
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.concurrent.duration._

/** A KD-Tree specialized for Point centers enabling efficient range queries on existing (non-empty) cells. Construction
  * is recursive; child subtrees are built in parallel once the subproblem is large enough. Sorting at each level is
  * also optionally parallelized.
  *
  * Neighbor (range) queries are read-only and their results are cached per (cell, radius) on the first query to avoid
  * repeated computation throughout algorithms that repeatedly need neighboring cells (e.g. iterative clustering
  * refinement steps).
  */
class ParKDTree(
    inputPoints: Array[Point],
    parallelBuildThreshold: Int = 2048, // size above which to build children in parallel
    parallelSortThreshold: Int = 4096,  // size above which to parallelize the level sort
    cacheEnabled: Boolean = true
)(implicit ec: ExecutionContext)
    extends Serializable {

  require(inputPoints.nonEmpty, "Points array must not be empty")

  private val dimension: Int = inputPoints.head.vector.length

  private val pts: Array[Point] = inputPoints

  @volatile private var root: Option[CellKDNode] = None

  // (pointId, radius) -> neighbors (excluding the point itself)
  private val neighborCache = new ConcurrentHashMap[(Long, Float), Array[Point]]()

  /** Build (or rebuild) the tree. */
  def build(): ParKDTree = {
    root = buildNode(pts, 0)
    this
  }

  /** Returns cached neighboring cells (within radius) for a given cell, computing once. */
  def neighboringPoints(point: Point, radius: Float): Array[Point] = {
    val key = (point.id, radius)
    if (!cacheEnabled) return rangeQuery(point.vector, radius).filterNot(_.id == point.id)
    val cached = neighborCache.get(key)
    if (cached != null) return cached
    val computed = rangeQuery(point.vector, radius).filterNot(_.id == point.id)
    neighborCache.putIfAbsent(key, computed)
    neighborCache.get(key)
  }

  // Backwards compatible alias (name kept from cell-oriented design)
  def neighboringCells(point: Point, radius: Float): Array[Point] = neighboringPoints(point, radius)

  /** Range query around a cell center (includes the cell itself). */
  def rangeQuery(anchorPoint: Point, radius: Float): Array[Point] = rangeQuery(anchorPoint.vector, radius)

  /** Range query around arbitrary point. */
  def rangeQuery(point: Embedding, radius: Float): Array[Point] = {
    val rSq   = radius.toDouble * radius.toDouble
    val buf   = ArrayBuffer.empty[Point]
    val stack = ArrayBuffer.empty[(CellKDNode, Int)]
    root.foreach(r => stack += ((r, 0)))
    while (stack.nonEmpty) {
      val (node, depth) = stack.remove(stack.length - 1)
      val axis          = depth % dimension
      val distSq        = euclideanDistanceSquared(point, node.point.vector)
      if (distSq <= rSq) buf += node.point
      val splitVal = node.point.vector(axis)
      val queryVal = point(axis)
      val diff     = radius.toDouble
      if (queryVal - diff <= splitVal) node.left.foreach(ch => stack += ((ch, depth + 1)))
      if (queryVal + diff > splitVal) node.right.foreach(ch => stack += ((ch, depth + 1)))
    }
    buf.toArray
  }

  /** Clears the neighbor cache (e.g., if tree rebuilt with different cells). */
  def clearCache(): Unit = neighborCache.clear()

  /** Current number of cached neighbor sets. */
  def cacheSize: Int = neighborCache.size()

  /** Depth of tree (for diagnostics). */
  def treeDepth: Int = {
    def depthRec(n: Option[CellKDNode]): Int = n match {
      case None       => 0
      case Some(node) => 1 + math.max(depthRec(node.left), depthRec(node.right))
    }
    depthRec(root)
  }

  private def buildNode(local: Array[Point], depth: Int): Option[CellKDNode] = {
    if (local.isEmpty) return None
    if (local.length == 1) return Some(CellKDNode(local(0), None, None))
    val axis       = depth % dimension
    val sorted     = sortLevel(local, axis)
    val medianIdx  = sorted.length / 2
    val median     = sorted(medianIdx)
    val leftSlice  = java.util.Arrays.copyOfRange(sorted, 0, medianIdx)
    val rightSlice = java.util.Arrays.copyOfRange(sorted, medianIdx + 1, sorted.length)
    // Build children (possibly parallel)
    if (sorted.length >= parallelBuildThreshold) {
      val fLeft = Future(buildNode(leftSlice, depth + 1))
      val right = buildNode(rightSlice, depth + 1) // build one side in current thread to balance
      val left  = Await.result(fLeft, Duration.Inf)
      Some(CellKDNode(median, left, right))
    } else {
      val left  = buildNode(leftSlice, depth + 1)
      val right = buildNode(rightSlice, depth + 1)
      Some(CellKDNode(median, left, right))
    }
  }

  private def sortLevel(arr: Array[Point], axis: Int): Array[Point] = {
    if (arr.length < parallelSortThreshold) arr.sortBy(_.vector(axis))
    else {
      // Split into chunks, sort in parallel, then merge k-way
      val cores        = math.min(Runtime.getRuntime.availableProcessors(), math.max(1, arr.length / parallelSortThreshold))
      val chunkSize    = math.max(1, (arr.length + cores - 1) / cores)
      val slices       = arr.grouped(chunkSize).toArray
      val futures      = slices.map(sl => Future(sl.sortBy(_.vector(axis))))
      val sortedSlices = Await.result(Future.sequence(futures.toSeq), Duration.Inf)
      kWayMerge(sortedSlices.toArray, axis)
    }
  }

  private def kWayMerge(slices: Array[Array[Point]], axis: Int): Array[Point] = {
    case class Cursor(i: Int, j: Int)
    implicit val ord: Ordering[Cursor] = Ordering.by[Cursor, Float](c => slices(c.i)(c.j).vector(axis)).reverse
    val pq                             = scala.collection.mutable.PriorityQueue.empty[Cursor]
    var total                          = 0
    var si                             = 0
    while (si < slices.length) {
      if (slices(si).nonEmpty) {
        pq.enqueue(Cursor(si, 0)); total += slices(si).length
      };
      si += 1
    }
    val out = new Array[Point](total)
    var k   = 0
    while (pq.nonEmpty) {
      val cur = pq.dequeue()
      val p   = slices(cur.i)(cur.j)
      out(k) = p; k += 1
      val nextJ = cur.j + 1
      if (nextJ < slices(cur.i).length) pq.enqueue(Cursor(cur.i, nextJ))
    }
    out
  }

}

object ParKDTree {

  /** Convenience constructor using global ExecutionContext. */
  def apply(
      points: Array[Point],
      parallelBuildThreshold: Int = 2048,
      parallelSortThreshold: Int = 4096,
      cacheEnabled: Boolean = true
  ): ParKDTree =
    new ParKDTree(points, parallelBuildThreshold, parallelSortThreshold, cacheEnabled)(
      scala.concurrent.ExecutionContext.global
    ).build()

}

case class CellKDNode(var point: Point, var left: Option[CellKDNode], var right: Option[CellKDNode])
