package components

import data.{Edge, Vertex}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ConnectedComponentsTest extends AnyFlatSpec with Matchers {

  "CommunityDetection" should "detect communities in a simple graph" in {
    val graph: Set[Edge[Int, Vertex[Int]]] = Set(
      (1, 2),
      (2, 3),
      (3, 4),
      (4, 5), // First community
      (6, 7),
      (7, 8),
      (8, 9),
      (9, 10) // Second community
    ).map { case (a, b) => Edge(new Vertex(a), new Vertex(b)) }

    val communityDetection = new ConnectedComponents[Int]()
    val communities        = communityDetection.findConnectedComponents(graph)

    val communityIds = communities.map(_.map(_.value))
    communityIds should contain theSameElementsAs Seq(Set(1, 2, 3, 4, 5), Set(6, 7, 8, 9, 10))
  }

  it should "detect a single community in a fully connected graph" in {
    val graph: Set[Edge[Int, Vertex[Int]]] = Set((1, 2), (2, 3), (3, 4), (4, 1), (1, 3), (2, 4)).map { case (a, b) =>
      Edge(new Vertex(a), new Vertex(b))
    }

    val communityDetection = new ConnectedComponents[Int]()
    val communities        = communityDetection.findConnectedComponents(graph)

    val communityIds = communities.map(_.map(_.value))
    communityIds should contain theSameElementsAs Seq(Set(1, 2, 3, 4))
  }

  it should "handle an empty graph" in {
    val graph: Set[Edge[Int, Vertex[Int]]] =
      Set.empty[(Int, Int)].map { case (a, b) => Edge(new Vertex(a), new Vertex(b)) }

    val communityDetection = new ConnectedComponents[Int]()
    val communities        = communityDetection.findConnectedComponents(graph)

    communities shouldBe empty
  }

  it should "handle a graph with isolated nodes" in {
    val graph: Set[Edge[Int, Vertex[Int]]] = Set(
      (1, 2),
      (2, 3), // First community
      (5, 6), // Second community
      (8, 9)  // Third community
    ).map { case (a, b) => Edge(new Vertex(a), new Vertex(b)) }

    val communityDetection = new ConnectedComponents[Int]()
    val communities        = communityDetection.findConnectedComponents(graph)

    val communityIds = communities.map(_.map(_.value))
    communityIds should have size 3
    communityIds should contain(Set(1, 2, 3))
    communityIds should contain(Set(5, 6))
    communityIds should contain(Set(8, 9))
  }

  it should "handle a graph with multiple disconnected components" in {
    val graph: Set[Edge[Int, Vertex[Int]]] = Set(
      (1, 2),
      (2, 3),
      (3, 1), // Triangular community
      (4, 5),
      (5, 6),
      (6, 4), // Another triangular community
      (7, 8), // Pair of nodes
      (10, 11),
      (11, 12),
      (12, 10) // Third triangular community
    ).map { case (a, b) => Edge(new Vertex(a), new Vertex(b)) }

    val communityDetection = new ConnectedComponents[Int]()
    val communities        = communityDetection.findConnectedComponents(graph)

    val communityIds = communities.map(_.map(_.value))
    communityIds should have size 4
    communityIds should contain(Set(1, 2, 3))
    communityIds should contain(Set(4, 5, 6))
    communityIds should contain(Set(7, 8))
    communityIds should contain(Set(10, 11, 12))
  }

  it should "handle a graph with a single node" in {
    val graph: Set[Edge[Int, Vertex[Int]]] = Set((1, 1)).map { case (a, b) => Edge(new Vertex(a), new Vertex(b)) }
    // Self-loop or single node

    val communityDetection = new ConnectedComponents[Int]()
    val communities        = communityDetection.findConnectedComponents(graph)

    val communityIds = communities.map(_.map(_.value))
    communityIds shouldBe Set(Set(1))
  }

  it should "handle a large graph with multiple disconnected components" in {
    val graph: Set[Edge[Int, Vertex[Int]]] = Set(
      // First component
      (1, 2),
      (2, 3),
      (3, 4),
      (4, 5),
      (5, 1),
      (2, 4),
      // Second component
      (10, 11),
      (11, 12),
      (12, 13),
      (13, 14),
      (14, 10),
      // Third component
      (20, 21),
      (21, 22),
      (22, 23),
      (23, 20)
    ).map { case (a, b) => Edge(new Vertex(a), new Vertex(b)) }

    val communityDetection = new ConnectedComponents[Int]()
    val communities        = communityDetection.findConnectedComponents(graph)

    val communityIds = communities.map(_.map(_.value))
    communityIds should have size 3
    communityIds should contain(Set(1, 2, 3, 4, 5))
    communityIds should contain(Set(10, 11, 12, 13, 14))
    communityIds should contain(Set(20, 21, 22, 23))
  }

}
