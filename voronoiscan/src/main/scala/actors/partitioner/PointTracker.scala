package actors.partitioner

import scala.collection.mutable

final class PointTracker {

  private val points: mutable.Map[Long, Int] = mutable.Map.empty

  def initPoints(ids: mutable.Set[Long]): Unit =
    ids.foreach(id => points.update(id, 0))

  def receivePoints(ids: mutable.Set[Long], numReceives: Int): mutable.Set[Long] = {
    val completedPoints = mutable.Set.empty[Long]
    ids.foreach { id =>
      val currentCount = points.getOrElse(id, 0)
      val newCount     = currentCount + 1
      if (newCount == numReceives) {
        completedPoints.add(id)
        points.remove(id)
      } else {
        points.update(id, newCount)
      }
    }
    completedPoints
  }

}
