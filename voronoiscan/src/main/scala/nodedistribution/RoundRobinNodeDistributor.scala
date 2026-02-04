package nodedistribution

import data.DelaunayGraph

object RoundRobinNodeDistributor extends NodeDistributor {

  override def assignCellsToNodes(graph: DelaunayGraph, numNodes: Int): Map[Int, Int] = {
    require(numNodes > 1, "Number of nodes must be greater than 1")
    val points = graph.adjacencyList.keys.toArray
    require(numNodes <= points.length, "Number of nodes must not exceed the number of points")

    // Distribute points in a round-robin fashion and create a map
    points.zipWithIndex.map { case (pointId, index) =>
      pointId -> (index % numNodes)
    }.toMap
  }

}
