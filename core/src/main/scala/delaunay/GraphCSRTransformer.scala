package delaunay

import data.DelaunayGraph

object GraphCSRTransformer {

  case class CSR(xadj: Array[Int], adjncy: Array[Int])

  def toCSR(graph: DelaunayGraph): CSR = {
    val n         = graph.vertices.size
    val adjacency = Array.fill(n)(List.empty[Int])
    for ((v, neighbors) <- graph.adjacencyList)
      adjacency(v) = neighbors.toList.sorted
    val xadj         = new Array[Int](n + 1)
    val adjncyBuffer = scala.collection.mutable.ArrayBuffer[Int]()
    for (i <- 0 until n) {
      xadj(i) = adjncyBuffer.size
      adjncyBuffer ++= adjacency(i)
    }
    xadj(n) = adjncyBuffer.size
    CSR(xadj, adjncyBuffer.toArray)
  }

}
