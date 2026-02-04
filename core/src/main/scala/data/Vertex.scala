package data

import com.fasterxml.jackson.annotation.JsonValue

class Vertex[T](val v: T) extends Serializable {

  @JsonValue
  def value: T = v

  def apply[R](f: T => R): R = f(value)

  override def equals(obj: Any): Boolean = obj match {
    case that: Vertex[_] =>
      that.v match {
        case v: T => this.value == v
        case _ => false
      }
    case _ => false
  }

  override def hashCode(): Int = value.hashCode()

}
