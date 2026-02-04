package nodedistribution

import data.DelaunayGraph

object SingleNodeDistributor extends NodeDistributor {

  override def assignCellsToNodes(graph: DelaunayGraph, numNodes: Int): Map[Int, Int] = {
    require(numNodes == 1, "SingleNodeDistributor can only be used with numNodes = 1")
    graph.adjacencyList.keys.map(vertexId => vertexId -> 0).toMap
  }

}
