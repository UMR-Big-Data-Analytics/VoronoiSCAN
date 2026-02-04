package data

import com.fasterxml.jackson.databind.{DeserializationContext, KeyDeserializer}

class EdgeKeyDeserializer extends KeyDeserializer {

  override def deserializeKey(key: String, ctxt: DeserializationContext): AnyRef = {
    val parts = key.split("-")
    val left  = new Vertex(parts(0).toInt)
    val right = new Vertex(parts(1).toInt)
    Edge[Int, Vertex[Int]](left, right)
  }

}
