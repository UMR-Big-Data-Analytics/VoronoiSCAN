package nodedistribution

import data.DelaunayGraph
import delaunay.GraphCSRTransformer
import kahip.KahipAdapter

import scala.jdk.CollectionConverters._

object KaHIPNodeDistributor extends NodeDistributor {

  override def assignCellsToNodes(graph: DelaunayGraph, numNodes: Int): Map[Int, Int] = {
    val csrGraph = GraphCSRTransformer.toCSR(graph)
    val xadj     = csrGraph.xadj                // Starting indices for each vertex's adjacency list
    val adjncy   = csrGraph.adjncy              // Adjacency list
    val adjcwgt  = Array.fill(adjncy.length)(1) // Edge weights (all 1 for unweighted)

    KahipAdapter
      .kahipPartition(
        graph.vertices.size,                // Number of vertices
        Array.fill(graph.vertices.size)(1), // Vertex weights (all 1 for unweighted)
        xadj,
        adjncy, // Adjacency list
        adjcwgt,
        numNodes, // Number of partitions
        0.03,     // Maximum allowed imbalance
        false,    // suppress_output
        42        // random seed);
      )
      .partitionMap()
      .asScala
      .map { case (k, v) => k.toInt -> v.toInt }
      .toMap
  }

}
