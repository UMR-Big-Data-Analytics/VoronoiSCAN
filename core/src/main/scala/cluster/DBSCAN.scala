package cluster

import data.Point
import spatial.index.KDTree

import scala.collection.mutable

class DBSCAN(val epsilon: Float, val minPts: Int) extends BaseDBSCAN {

  private val corePoints: mutable.Map[Int, mutable.Set[Point]] = mutable.Map()

  private val borderPoints: mutable.Map[Int, mutable.Set[Point]] = mutable.Map()

  private var kdtree: KDTree = _

  private var numClusters: Int = _

  def getKdTree: KDTree = kdtree

  override def fit(points: Array[Point]): (Array[Int], Array[Point]) = {
    if (points.isEmpty) {
      return (Array.empty[Int], Array.empty[Point])
    }
    kdtree = new KDTree(points).build()
    val labels    = Array.fill(points.length)(-1)
    var clusterId = 0

    // Pre-compute core point status
    val isCorePoint = Array.ofDim[Boolean](points.length)
    var i = 0
    while (i < points.length) {
      val neighbors = kdtree.rangeQuery(points(i).vector, epsilon)
      isCorePoint(i) = neighbors.length >= minPts
      i += 1
    }

    // Build a fast id->index map for O(1) lookups
    val idToIndex = points.indices.map(i => points(i).id -> i).toMap

    // Perform clustering
    i = 0
    while (i < points.length) {
      if (labels(i) == -1 && isCorePoint(i)) {
        clusterId += 1
        corePoints(clusterId) = mutable.Set(points(i))
        borderPoints(clusterId) = mutable.Set()
        expandCluster(points, labels, i, isCorePoint, idToIndex, clusterId)
      }
      i += 1
    }

    numClusters = clusterId
    (labels, points)
  }

  def getNumClusters: Int = numClusters

  def getCorePoints: Array[Array[Point]] = getPointsArray(corePoints)

  override def getPointsArray(pointsMap: mutable.Map[Int, mutable.Set[Point]]): Array[Array[Point]] =
    pointsMap.toArray.sortBy(_._1).map(_._2.toArray)

  def getBorderPoints: Array[Array[Point]] = getPointsArray(borderPoints)

  private def expandCluster(
                             pointsWithIds: Array[Point],
                             labels: Array[Int],
                             seedIdx: Int,
                             isCorePoint: Array[Boolean],
                             idToIndex: Map[Long, Int],
                             clusterId: Int
  ): Unit = {
    val queue = mutable.Queue[Int]()
    labels(seedIdx) = clusterId
    queue.enqueue(seedIdx)

    while (queue.nonEmpty) {
      val idx = queue.dequeue()
      val neighbors = kdtree.rangeQuery(pointsWithIds(idx).vector, epsilon)
      var j = 0
      while (j < neighbors.length) {
        val nId = neighbors(j).id
        val nIdx = idToIndex(nId)
        if (labels(nIdx) == -1) {
          labels(nIdx) = clusterId
          if (isCorePoint(nIdx)) {
            corePoints(clusterId) += pointsWithIds(nIdx)
            queue.enqueue(nIdx)
          } else {
            borderPoints(clusterId) += pointsWithIds(nIdx)
          }
        }
        j += 1
      }
    }
  }

}
