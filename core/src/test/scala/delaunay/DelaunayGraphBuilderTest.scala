package delaunay

import data.Point
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class DelaunayGraphBuilderTest extends AnyFlatSpec with Matchers {

  "QHullWrapper" should "compute the Delaunay triangulation correctly" in {
    val points = Array(
      Point(Array(0.5327700708364619f, 0.8433048356466775f), 1),
      Point(Array(0.6568729954984174f, 0.3186475059556044f), 2),
      Point(Array(0.3046879742682438f, 0.8629357481546722f), 3),
      Point(Array(0.8442221419460387f, 0.2693438403479881f), 4)
    )

    val graph = DelaunayGraphBuilder.buildDelaunayGraph(points)

    // Check the number of vertices
    graph.vertices.length shouldEqual points.length

    // Check the adjacency list
    val expectedAdjacencyList = Map(0 -> Set(1, 2, 3), 1 -> Set(0, 2, 3), 2 -> Set(0, 1), 3 -> Set(0, 1))

    graph.adjacencyList shouldEqual expectedAdjacencyList

    // Check the edges
    val expectedEdges = Seq((1, 2), (0, 1), (1, 3), (0, 3), (0, 2))

    graph.getEdges should contain theSameElementsAs expectedEdges
  }

}
