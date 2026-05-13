package assignment

import data.Point

import scala.collection.mutable

class VectorAssignment(nCells: Int) extends Serializable {

  private val cellAssignments = Array.fill(nCells)(mutable.Map[Long, Point]())

  def assign(point: Point, cellIndex: Int): Unit =
    cellAssignments(cellIndex) += (point.id -> point)

  def get(cellIndex: Int): mutable.Map[Long, Point] =
    cellAssignments(cellIndex)

  def containsPoint(cellIdx: Int, point: Point): Boolean =
    cellAssignments(cellIdx).contains(point.id)

  override def toString: String =
    cellAssignments.zipWithIndex.map { case (points, cellIndex) =>
      s"Cell $cellIndex: ${points.mkString(", ")}"
    }
      .mkString("\n")

  override def equals(obj: Any): Boolean = obj match {
    case other: VectorAssignment => deepEquals(other)
    case _                       => false
  }

  private def deepEquals(other: VectorAssignment): Boolean =
    cellAssignments.zip(other.cellAssignments).forall { case (thisMap, otherMap) =>
      thisMap.size == otherMap.size && thisMap.forall { case (idx, point) =>
        otherMap.contains(idx) && otherMap(idx) == point
      }
    }

}
