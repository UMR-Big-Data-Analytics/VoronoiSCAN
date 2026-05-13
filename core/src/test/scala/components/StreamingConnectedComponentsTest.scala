package components

import data.{Edge, Vertex}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class StreamingConnectedComponentsTest extends AnyFlatSpec with Matchers {

  "StreamingConnectedComponents" should "initialize empty" in {
    val scc = new StreamingConnectedComponents[String]()
    scc.getCurrentComponents shouldBe empty
  }

  it should "create a single component from one edge" in {
    val scc  = new StreamingConnectedComponents[String]()
    val edge = Edge[String, Vertex[String]](new Vertex("A"), new Vertex("B"))

    scc.processEdge(edge)

    val components = scc.getCurrentComponents
    components.size shouldBe 1
    (components.head should contain).allOf(new Vertex("A"), new Vertex("B"))
  }

  it should "correctly identify connected vertices" in {
    val scc = new StreamingConnectedComponents[String]()

    scc.processEdge(Edge(new Vertex("A"), new Vertex("B")))
    scc.processEdge(Edge(new Vertex("B"), new Vertex("C")))

    scc.isConnected(new Vertex("A"), new Vertex("C")) shouldBe true
    scc.isConnected(new Vertex("A"), new Vertex("B")) shouldBe true
    scc.isConnected(new Vertex("B"), new Vertex("C")) shouldBe true
  }

  it should "maintain separate components when no connecting edges exist" in {
    val scc = new StreamingConnectedComponents[String]()

    scc.processEdge(Edge(new Vertex("A"), new Vertex("B")))
    scc.processEdge(Edge(new Vertex("C"), new Vertex("D")))

    scc.isConnected(new Vertex("A"), new Vertex("C")) shouldBe false
    scc.isConnected(new Vertex("B"), new Vertex("D")) shouldBe false

    val components = scc.getCurrentComponents
    components.size shouldBe 2
    (components should contain).allOf(
      Set(new Vertex("A"), new Vertex("B")),
      Set(new Vertex("C"), new Vertex("D"))
    )
  }

  it should "merge components when connecting edge is added" in {
    val scc = new StreamingConnectedComponents[String]()

    scc.processEdge(Edge(new Vertex("A"), new Vertex("B")))
    scc.processEdge(Edge(new Vertex("C"), new Vertex("D")))
    scc.processEdge(Edge(new Vertex("B"), new Vertex("C")))

    val components = scc.getCurrentComponents
    components.size shouldBe 1
    (components.head should contain).allOf(
      new Vertex("A"),
      new Vertex("B"),
      new Vertex("C"),
      new Vertex("D")
    )
  }

  it should "handle self-loops" in {
    val scc    = new StreamingConnectedComponents[String]()
    val vertex = new Vertex("A")

    scc.processEdge(Edge(vertex, vertex))

    val components = scc.getCurrentComponents
    components.size shouldBe 1
    components.head should contain(vertex)
  }

  it should "work with different data types" in {
    val scc = new StreamingConnectedComponents[Int]()

    scc.processEdge(Edge(new Vertex(1), new Vertex(2)))
    scc.processEdge(Edge(new Vertex(2), new Vertex(3)))

    val components = scc.getCurrentComponents
    components.size shouldBe 1
    (components.head should contain).allOf(new Vertex(1), new Vertex(2), new Vertex(3))
  }

  it should "maintain component integrity with multiple operations" in {
    val scc = new StreamingConnectedComponents[String]()

    // Create initial components
    scc.processEdge(Edge(new Vertex("A"), new Vertex("B")))
    scc.processEdge(Edge(new Vertex("C"), new Vertex("D")))
    scc.processEdge(Edge(new Vertex("E"), new Vertex("F")))

    // Verify initial state
    scc.getCurrentComponents.size shouldBe 3

    // Merge two components
    scc.processEdge(Edge(new Vertex("B"), new Vertex("C")))
    scc.getCurrentComponents.size shouldBe 2

    // Add new vertex to existing component
    scc.processEdge(Edge(new Vertex("D"), new Vertex("G")))

    val finalComponents = scc.getCurrentComponents
    finalComponents.size shouldBe 2

    // Find the larger component
    val largerComponent = finalComponents.find(_.size > 2).get
    (largerComponent should contain).allOf(
      new Vertex("A"), new Vertex("B"), new Vertex("C"), new Vertex("D"), new Vertex("G")
    )

    // Check the other component remained unchanged
    (finalComponents.find(_.size == 2).get should contain).allOf(
      new Vertex("E"),
      new Vertex("F")
    )
  }

  it should "handle parallel edges" in {
    val scc  = new StreamingConnectedComponents[String]()
    val edge = Edge[String, Vertex[String]](new Vertex("A"), new Vertex("B"))

    scc.processEdge(edge)
    scc.processEdge(edge) // Same edge again

    val components = scc.getCurrentComponents
    components.size shouldBe 1
    (components.head should contain).allOf(new Vertex("A"), new Vertex("B"))
  }

}
