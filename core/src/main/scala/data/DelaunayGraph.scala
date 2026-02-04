package data

import scala.collection.mutable

case class DelaunayGraph(vertices: Seq[Point], edges: Set[(Int, Int)]) extends Serializable {

  val adjacencyList = mutable.Map.empty[Int, mutable.Set[Int]]

  edges.foreach { case (v1, v2) =>
    adjacencyList.getOrElseUpdate(v1, mutable.Set.empty) += v2
    adjacencyList.getOrElseUpdate(v2, mutable.Set.empty) += v1
  }

  def getEdges: Seq[(Int, Int)] =
    edges.toSeq

  override def toString: String = {
    val sb = new StringBuilder("Delaunay Graph:\n")
    vertices.zipWithIndex.foreach { case (point, idx) =>
      sb.append(s"Vertex $idx: $point\n")
    }

    sb.append("Edges:\n")
    edges.foreach { case (v1, v2) =>
      sb.append(s"$v1 - $v2\n")
    }
    sb.toString()
  }

}
