package data

import data.Point.{DType, Embedding}

object Point {

  type DType = Float

  type Embedding = Array[DType]

}

case class Point(vector: Embedding, id: Long) {

  var distanceToCenter: Double = _

  def this(id: Long, point: Embedding) =
    this(point, id)

  def dim: Int = vector.length

  def apply(index: Int): DType =
    vector(index)

  override def equals(obj: Any): Boolean =
    obj match {
      case that: Point => id == that.id
      case _ => false
    }

  override def hashCode(): Int =
    id.hashCode()

  override def toString: String =
    s"Point(id=$id, vector=${vector.mkString(", ")}, distanceToCenter=$distanceToCenter)"

}
