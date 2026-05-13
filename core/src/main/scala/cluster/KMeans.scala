package cluster

import data.Point
import spatial.index.KDTree
import utils.ArrayOps.DoubleArrayOps
import utils.Distances.euclideanDistanceSquared

import scala.collection.mutable.ArrayBuffer
import scala.util.Random

class KMeans(k: Int, maxIterations: Int = 1000, tolerance: Double = 1e-4, seed: Int = 42) {

  private var centroids: Array[Array[Double]] = _

  private var lastAssignments: Array[Int] = _

  def fit(data: Array[Array[Double]]): Array[Int] = {
    require(k <= data.length, "k cannot be larger than number of data points")

    val rand = new Random(seed)
    centroids = initializeCentroidsKMeansPlusPlus(data, rand)
    lastAssignments = Array.fill(data.length)(-1)

    var oldCentroids = Array.empty[Array[Double]]
    var iterations   = 0

    while (iterations < maxIterations && !hasConverged(oldCentroids, centroids)) {
      oldCentroids = centroids.map(_.clone())

      val centroidTree = buildCentroidTree()
      val assignments  = assignWithKDTree(data, centroidTree)
      updateCentroids(data, assignments)

      iterations += 1
    }

    lastAssignments
  }

  private def initializeCentroidsKMeansPlusPlus(data: Array[Array[Double]], rand: Random): Array[Array[Double]] = {
    val selected = ArrayBuffer[Array[Double]]()
    selected += data(rand.nextInt(data.length)).clone()

    while (selected.length < k) {
      val minDistances = data.map { point =>
        selected.iterator.map(centroid => euclideanDistanceSquared(point, centroid)).min
      }
      val totalDistance = minDistances.sum

      val nextCentroid =
        if (totalDistance <= 0.0 || totalDistance.isNaN) {
          data(rand.nextInt(data.length))
        } else {
          val threshold  = rand.nextDouble() * totalDistance
          var idx        = 0
          var cumulative = minDistances(idx)

          while (idx < data.length - 1 && cumulative < threshold) {
            idx        += 1
            cumulative += minDistances(idx)
          }
          data(idx)
        }

      selected += nextCentroid.clone()
    }

    selected.toArray
  }

  private def buildCentroidTree(): KDTree = {
    val centroidPoints = ArrayBuffer.from(
      centroids.zipWithIndex.map { case (c, i) =>
        Point(c.map(_.toFloat), i.toLong)
      }
    )
    new KDTree(centroidPoints).build()
  }

  private def assignWithKDTree(data: Array[Array[Double]], centroidTree: KDTree): Array[Int] = {
    for (i <- data.indices) {
      val query        = data(i).map(_.toFloat)
      val (nearest, _) = centroidTree.nearestNeighbor(query)
      lastAssignments(i) = nearest.id.toInt
    }
    lastAssignments
  }

  private def updateCentroids(data: Array[Array[Double]], assignments: Array[Int]): Unit = {
    val newCentroids = Array.fill(k)(Array.fill[Double](data.head.length)(0))
    val counts       = Array.fill(k)(0)

    for (i <- data.indices) {
      val cluster = assignments(i)
      newCentroids(cluster) += data(i)
      counts(cluster)       += 1
    }

    for (i <- 0 until k)
      if (counts(i) > 0) newCentroids(i) /= counts(i).toDouble

    centroids = newCentroids
  }

  private def hasConverged(old: Array[Array[Double]], current: Array[Array[Double]]): Boolean = {
    if (old.isEmpty) return false
    old.zip(current).forall { case (o, c) => euclideanDistanceSquared(o, c) < tolerance }
  }

  def predict(point: Array[Double]): Int =
    centroids.indices.minBy(i => euclideanDistanceSquared(point, centroids(i)))

  def getCentroids: Array[Array[Double]] = centroids

}
