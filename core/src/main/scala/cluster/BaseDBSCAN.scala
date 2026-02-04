package cluster

import data.Point

import scala.collection.mutable

trait BaseDBSCAN extends Serializable {

  def fit(points: Array[Point]): (Array[Int], Array[Point])

  def getNumClusters: Int

  def getCorePoints: Array[Array[Point]]

  def getBorderPoints: Array[Array[Point]]

  protected def getPointsArray(pointsMap: mutable.Map[Int, mutable.Set[Point]]): Array[Array[Point]] = {
    if (pointsMap.isEmpty) {
      return Array.empty
    }
    val maxClusterId = pointsMap.keys.max
    Array(maxClusterId).map(i => pointsMap.getOrElse(i, mutable.Set()).toArray)
  }

}
