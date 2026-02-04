package data

import com.fasterxml.jackson.core.JsonGenerator
import com.fasterxml.jackson.databind.SerializerProvider
import com.fasterxml.jackson.databind.ser.std.StdKeySerializers

case class Edge[T, V <: Vertex[T]](left: V, right: V) extends Serializable {

  override def equals(obj: Any): Boolean = obj match {
    case Edge(l, r) => (l == left && r == right) || (l == right && r == left)
    case _          => false
  }

  override def hashCode(): Int = {
    val l = left.hashCode()
    val r = right.hashCode()
    if (l < r) {
      l * 31 + r
    } else {
      r * 31 + l
    }
  }

}

class EdgeKeySerializer extends StdKeySerializers.StringKeySerializer {

  override def serialize(value: Any, gen: JsonGenerator, provider: SerializerProvider): Unit = {
    val edge = value.asInstanceOf[Edge[_, _ <: Vertex[_]]]
    val left = edge.left.value
    val right = edge.right.value
    gen.writeFieldName(s"$left-$right")
  }

}
