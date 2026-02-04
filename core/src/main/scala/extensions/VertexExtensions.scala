package extensions

import data.Vertex

object VertexExtensions {

  implicit class IntToVertex(value: Int) {

    def toVertex: Vertex[Int] = new Vertex(value)

  }

  implicit class LongToVertex(value: Long) {

    def toVertex: Vertex[Long] = new Vertex(value)

  }

  implicit class FloatToVertex(value: Float) {

    def toVertex: Vertex[Float] = new Vertex(value)

  }

  implicit class DoubleToVertex(value: Double) {

    def toVertex: Vertex[Double] = new Vertex(value)

  }

  implicit class CharToVertex(value: Char) {

    def toVertex: Vertex[Char] = new Vertex(value)

  }

  implicit class ByteToVertex(value: Byte) {

    def toVertex: Vertex[Byte] = new Vertex(value)

  }

  implicit class ShortToVertex(value: Short) {

    def toVertex: Vertex[Short] = new Vertex(value)

  }

  implicit class BooleanToVertex(value: Boolean) {

    def toVertex: Vertex[Boolean] = new Vertex(value)

  }

  implicit class TupleToVertex(value: (Int, Int)) {

    def toVertex: Vertex[(Int, Int)] = new Vertex(value)

  }

  implicit class StringToVertex(value: String) {

    def toVertex: Vertex[String] = new Vertex(value)

  }

}
