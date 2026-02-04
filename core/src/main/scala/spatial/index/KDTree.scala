package spatial.index

import data.Point
import data.Point.Embedding
import utils.Distances.euclideanDistanceSquared

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer

class KDTree(initialPoints: ArrayBuffer[Point], depth: Int = 10, val rebalanceThreshold: Int = 100)
  extends Serializable
    with SpatialTree[KDTree] {

  def this(initialPoints: Array[Point], depth: Int, rebalanceThreshold: Int) =
    this(ArrayBuffer.from(initialPoints), depth, rebalanceThreshold)

  def this(initialPoints: Array[Point]) = this(ArrayBuffer.from(initialPoints), 10, 100)

  require(initialPoints.nonEmpty, "Points collection must not be empty")

  private lazy val k = initialPoints.head.vector.length

  private val pointsBuffer = ArrayBuffer.from(initialPoints)

  private var insertions: Int = 0

  private var deletions: Int = 0

  var root: Option[KDNode] = None

  def build(): KDTree = {
    root = buildTreeOptimized(pointsBuffer.toArray, 0)
    insertions = 0
    deletions = 0
    this
  }

  def insert(point: Point): Unit = {
    pointsBuffer += point
    root = insertRec(root, point, 0)
    insertions += 1
    if (insertions >= rebalanceThreshold) {
      rebalance()
    }
  }

  def delete(point: Point): Unit = {
    val index = pointsBuffer.indexWhere(_.id == point.id)
    if (index >= 0) {
      pointsBuffer.remove(index)
      root = deleteRec(root, point, 0)
      deletions += 1
      if (deletions >= rebalanceThreshold) {
        rebalance()
      }
    }
  }

  override def rangeQuery(anchorPoint: Embedding, range: Float): Array[Point] = {
    if (root.isEmpty) return Array.empty

    val rangeSq = range.toDouble * range.toDouble
    val resultBuffer = ArrayBuffer.empty[Point]

    val nodeStack = ArrayBuffer.empty[(KDNode, Int)]
    nodeStack.addOne((root.get, 0))

    while (nodeStack.nonEmpty) {
      val (node, depth) = nodeStack.remove(nodeStack.length - 1)
      val axis          = depth % k

      val dSq = euclideanDistanceSquared(anchorPoint, node.pointWithId.vector)
      if (dSq <= rangeSq) resultBuffer += node.pointWithId

      val splitVal = node.pointWithId.vector(axis)
      val queryVal = anchorPoint(axis)

      val rangeDiff = range.toDouble
      if (queryVal - rangeDiff <= splitVal && node.left.isDefined) {
        nodeStack.addOne((node.left.get, depth + 1))
      }
      if (queryVal + rangeDiff > splitVal && node.right.isDefined) {
        nodeStack.addOne((node.right.get, depth + 1))
      }
    }

    resultBuffer.toArray
  }

  override def rangeCount(anchorPoint: Embedding, range: Float): Int = {
    if (root.isEmpty) return 0
    val rangeSq   = range.toDouble * range.toDouble
    var count     = 0
    val nodeStack = ArrayBuffer.empty[(KDNode, Int)]
    nodeStack.addOne((root.get, 0))
    while (nodeStack.nonEmpty) {
      val (node, depth) = nodeStack.remove(nodeStack.length - 1)
      val axis          = depth % k
      val dSq           = euclideanDistanceSquared(anchorPoint, node.pointWithId.vector)
      if (dSq <= rangeSq) count += 1
      val splitVal  = node.pointWithId.vector(axis)
      val queryVal  = anchorPoint(axis)
      val rangeDiff = range.toDouble
      if (queryVal - rangeDiff <= splitVal && node.left.isDefined) {
        nodeStack.addOne((node.left.get, depth + 1))
      }
      if (queryVal + rangeDiff > splitVal && node.right.isDefined) {
        nodeStack.addOne((node.right.get, depth + 1))
      }
    }
    count
  }

  private def buildTreeOptimized(points: Array[Point], depth: Int): Option[KDNode] = {
    if (points.isEmpty) return None
    if (points.length == 1) return Some(KDNode(points(0), None, None))

    val axis = depth % k

    val medianIndex = points.length / 2
    introSelectInPlace(points, medianIndex, axis, 0, points.length - 1, depth)

    val median      = points(medianIndex)
    val leftPoints  = points.slice(0, medianIndex)
    val rightPoints = points.slice(medianIndex + 1, points.length)

    Some(
      KDNode(
        pointWithId = median,
        left = buildTreeOptimized(leftPoints, depth + 1),
        right = buildTreeOptimized(rightPoints, depth + 1)
      )
    )
  }

  private def introSelectInPlace(
                                  points: Array[Point],
                                  targetIndex: Int,
                                  axis: Int,
                                  low: Int,
                                  high: Int,
                                  depthLimit: Int
                                ): Unit = {

    @tailrec
    def introSelectRec(l: Int, h: Int, target: Int, depth: Int): Unit = {
      if (l < h) {
        // Switch to heap-select for deep recursion to guarantee O(n) performance
        if (depth <= 0) {
          heapSelectInPlace(points, l, h, target, axis)
        } else {
          // Use median-of-three pivot selection for better partitioning
          val pivotIndex    = medianOfThree(points, l, h, axis)
          val newPivotIndex = partitionArray(points, l, h, pivotIndex, axis)

          if (newPivotIndex == target) {} else if (newPivotIndex > target) {
            introSelectRec(l, newPivotIndex - 1, target, depth - 1)
          } else {
            introSelectRec(newPivotIndex + 1, h, target, depth - 1)
          }
        }
      }
    }

    val maxDepth = 2 * (32 - Integer.numberOfLeadingZeros(high - low))
    introSelectRec(low, high, targetIndex, maxDepth)
  }

  private def medianOfThree(points: Array[Point], low: Int, high: Int, axis: Int): Int = {
    val mid = low + (high - low) / 2
    if (points(mid).vector(axis) < points(low).vector(axis)) swap(points, low, mid)
    if (points(high).vector(axis) < points(low).vector(axis)) swap(points, low, high)
    if (points(high).vector(axis) < points(mid).vector(axis)) swap(points, mid, high)
    mid
  }

  private def heapSelectInPlace(points: Array[Point], low: Int, high: Int, target: Int, axis: Int): Unit = {
    // Simple heap-based selection for guaranteed O(n) worst case
    val n = high - low + 1
    var i = n / 2 - 1

    // Build heap
    while (i >= 0) {
      heapify(points, low, high, low + i, axis)
      i -= 1
    }

    // Extract elements until we reach target
    var end = high
    while (end > low + target - low) {
      swap(points, low, end)
      end -= 1
      heapify(points, low, end, low, axis)
    }
  }

  @tailrec
  private def heapify(points: Array[Point], low: Int, high: Int, i: Int, axis: Int): Unit = {
    val largest = {
      val left  = 2 * (i - low) + 1 + low
      val right = 2 * (i - low) + 2 + low
      var max   = i

      if (left <= high && points(left).vector(axis) > points(max).vector(axis))
        max = left

      if (right <= high && points(right).vector(axis) > points(max).vector(axis))
        max = right

      max
    }

    if (largest != i) {
      swap(points, i, largest)
      heapify(points, low, high, largest, axis)
    }
  }

  private def partitionArray(points: Array[Point], low: Int, high: Int, pivotIndex: Int, axis: Int): Int = {
    val pivotValue = points(pivotIndex).vector(axis)
    // Move pivot to end
    swap(points, pivotIndex, high)
    var storeIndex = low

    var i = low
    while (i < high) {
      if (points(i).vector(axis) < pivotValue) {
        swap(points, i, storeIndex)
        storeIndex += 1
      }
      i += 1
    }
    // Move pivot to its final place
    swap(points, storeIndex, high)
    storeIndex
  }

  private def swap(arr: Array[Point], i: Int, j: Int): Unit = {
    val temp = arr(i)
    arr(i) = arr(j)
    arr(j) = temp
  }

  private def insertRec(node: Option[KDNode], pointWithId: Point, depth: Int): Option[KDNode] =
    node match {
      case None => Some(KDNode(pointWithId, None, None))
      case Some(n) =>
        val axis = depth % k
        if (pointWithId.vector(axis) <= n.pointWithId.vector(axis)) {
          n.left = insertRec(n.left, pointWithId, depth + 1)
        } else {
          n.right = insertRec(n.right, pointWithId, depth + 1)
        }
        Some(n)
    }

  private def deleteRec(node: Option[KDNode], point: Point, depth: Int): Option[KDNode] =
    node match {
      case None => None
      case Some(n) =>
        val axis = depth % k
        if (n.pointWithId.id == point.id) {
          if (n.right.isDefined) {
            val minNode = findMin(n.right, axis, depth + 1)
            n.pointWithId = minNode.pointWithId
            n.right = deleteRec(n.right, minNode.pointWithId, depth + 1)
          } else if (n.left.isDefined) {
            val minNode = findMin(n.left, axis, depth + 1)
            n.pointWithId = minNode.pointWithId
            n.right = deleteRec(n.left, minNode.pointWithId, depth + 1)
            n.left = None
          } else {
            return None
          }
        } else if (point.vector(axis) < n.pointWithId.vector(axis)) {
          n.left = deleteRec(n.left, point, depth + 1)
        } else {
          n.right = deleteRec(n.right, point, depth + 1)
        }
        Some(n)
    }

  private def findMin(node: Option[KDNode], axis: Int, depth: Int): KDNode =
    node match {
      case None => throw new NoSuchElementException
      case Some(n) =>
        val currentAxis = depth % k
        if (currentAxis == axis) {
          if (n.left.isEmpty) n else findMin(n.left, axis, depth + 1)
        } else {
          val candidates = ArrayBuffer(n)
          if (n.left.isDefined) candidates  += findMin(n.left, axis, depth + 1)
          if (n.right.isDefined) candidates += findMin(n.right, axis, depth + 1)
          candidates.minBy(_.pointWithId.vector(axis))
        }
    }

  private def rebalance(): Unit = {
    root = buildTreeOptimized(pointsBuffer.toArray, 0)
    insertions = 0
    deletions = 0
  }

  def kNearestNeighbors(queryPoint: Embedding, k: Int): Array[(Point, Float)] = {
    if (root.isEmpty) return Array.empty
    if (k <= 0) return Array.empty

    // Use a max-heap to maintain k nearest neighbors
    val maxHeap = scala.collection.mutable.PriorityQueue.empty[(Point, Double)](
      Ordering.by(_._2)
    )

    def searchRec(node: KDNode, depth: Int): Unit = {
      val axis          = depth % this.k
      val currentDistSq = euclideanDistanceSquared(queryPoint, node.pointWithId.vector)

      if (maxHeap.size < k) {
        maxHeap.enqueue((node.pointWithId, currentDistSq))
      } else if (currentDistSq < maxHeap.head._2) {
        maxHeap.dequeue()
        maxHeap.enqueue((node.pointWithId, currentDistSq))
      }

      val queryAxisValue = queryPoint(axis)
      val nodeAxisValue  = node.pointWithId.vector(axis)
      val goLeft         = queryAxisValue < nodeAxisValue

      val (firstChild, secondChild) = if (goLeft) (node.left, node.right) else (node.right, node.left)

      // Search the closer side first
      if (firstChild.isDefined) {
        searchRec(firstChild.get, depth + 1)
      }

      // Check if we need to search the other side
      val axisDist        = queryAxisValue - nodeAxisValue
      val axisDiffSq      = axisDist * axisDist
      val searchThreshold = if (maxHeap.size < k) Double.MaxValue else maxHeap.head._2

      if (secondChild.isDefined && axisDiffSq < searchThreshold) {
        searchRec(secondChild.get, depth + 1)
      }
    }

    searchRec(root.get, 0)
    maxHeap.toArray.map { case (point, distSq) => (point, math.sqrt(distSq).toFloat) }.reverse
  }

  def points: Array[Point] = pointsBuffer.toArray

  def insertionCount: Int = insertions

  def deletionCount: Int = deletions

}

case class KDNode(var pointWithId: Point, var left: Option[KDNode], var right: Option[KDNode])
