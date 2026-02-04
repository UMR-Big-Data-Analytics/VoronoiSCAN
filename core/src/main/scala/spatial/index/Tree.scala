package spatial.index

import data.Point
import data.Point.Embedding

trait TreeRoot {

  def root: Embedding

}

trait Tree[T <: Tree[T]] {

  def build(): T

}

trait NearestNeighbor {

  def nearestNeighbor(queryPoint: Embedding): (Point, Float) = {
    kNearestNeighbors(queryPoint, 1) match {
      case Array((point, distance)) => (point, distance)
      case _                        => throw new NoSuchElementException("No points in the KDTree")
    }
  }

  def nearestNeighbor(queryPoint: Point): (Point, Float) =
    nearestNeighbor(queryPoint.vector)

  def kNearestNeighbors(queryPoint: Embedding, k: Int): Array[(Point, Float)]

  def kNearestNeighbors(queryPoint: Point, k: Int): Array[(Point, Float)] =
    kNearestNeighbors(queryPoint.vector, k)

}

trait RangeQuery {

  def rangeCount(anchorPoint: Embedding, range: Float): Int =
    rangeQuery(anchorPoint, range).length

  def rangeCount(anchorPoint: Point, range: Float): Int =
    rangeCount(anchorPoint.vector, range)

  def rangeQuery(anchorPoint: Embedding, range: Float): Array[Point]

  def rangeQuery(anchorPoint: Point, range: Float): Array[Point] =
    rangeQuery(anchorPoint.vector, range)

}

trait PointAccessor {

  def getPoints: Iterable[Point]

}

trait TreeInitialized {

  def isBuilt: Boolean

}

trait SpatialTree[T <: SpatialTree[T]] extends Tree[T] with NearestNeighbor with RangeQuery
