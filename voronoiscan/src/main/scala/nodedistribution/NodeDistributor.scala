package nodedistribution

import data.DelaunayGraph

trait NodeDistributor {

  def assignCellsToNodes(graph: DelaunayGraph, numNodes: Int): Map[Int, Int]

}
