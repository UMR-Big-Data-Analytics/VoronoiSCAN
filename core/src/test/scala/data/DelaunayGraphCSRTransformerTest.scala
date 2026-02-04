package data

import delaunay.GraphCSRTransformer
import org.scalatest.funsuite.AnyFunSuite

class DelaunayGraphCSRTransformerTest extends AnyFunSuite {

  test("CSR conversion for simple triangle graph") {
    // 3 vertices: 0-1-2, edges: (0,1), (1,2), (2,0)
    val vertices = Seq(
      Point(Array(0f, 0f), 0L),
      Point(Array(1f, 0f), 1L),
      Point(Array(0f, 1f), 2L)
    )
    val edges = Set((0, 1), (1, 2), (2, 0))
    val graph = DelaunayGraph(vertices, edges)
    val csr   = GraphCSRTransformer.toCSR(graph)
    // Each vertex has 2 neighbors
    assert(csr.xadj.sameElements(Array(0, 2, 4, 6)))
    assert(csr.adjncy.sorted.sameElements(Array(0, 0, 1, 1, 2, 2)))
  }

  test("CSR conversion for single vertex, no edges") {
    val vertices = Seq(Point(Array(0f, 0f), 0L))
    val edges    = Set.empty[(Int, Int)]
    val graph    = DelaunayGraph(vertices, edges)
    val csr      = GraphCSRTransformer.toCSR(graph)
    assert(csr.xadj.sameElements(Array(0, 0)))
    assert(csr.adjncy.isEmpty)
  }

  test("CSR conversion for disconnected vertices") {
    val vertices = Seq(
      Point(Array(0f, 0f), 0L),
      Point(Array(1f, 0f), 1L),
      Point(Array(0f, 1f), 2L)
    )
    val edges = Set.empty[(Int, Int)]
    val graph = DelaunayGraph(vertices, edges)
    val csr   = GraphCSRTransformer.toCSR(graph)
    assert(csr.xadj.sameElements(Array(0, 0, 0, 0)))
    assert(csr.adjncy.isEmpty)
  }

  test("CSR conversion for complex graph") {
    // Create a more complex graph with 6 vertices and various connections
    val vertices = Seq(
      Point(Array(0f, 0f), 0L),
      Point(Array(1f, 0f), 1L),
      Point(Array(0f, 1f), 2L),
      Point(Array(1f, 1f), 3L),
      Point(Array(2f, 0f), 4L),
      Point(Array(2f, 1f), 5L)
    )

    // Create a graph with varied connectivity:
    // - vertex 0 connects to 1, 2
    // - vertex 1 connects to 0, 2, 3, 4
    // - vertex 2 connects to 0, 1, 3
    // - vertex 3 connects to 1, 2, 5
    // - vertex 4 connects to 1, 5
    // - vertex 5 connects to 3, 4
    val edges = Set(
      (0, 1),
      (0, 2),
      (1, 2),
      (1, 3),
      (1, 4),
      (2, 3),
      (3, 5),
      (4, 5)
    )

    val graph = DelaunayGraph(vertices, edges)
    val csr   = GraphCSRTransformer.toCSR(graph)

    // Verify the CSR structure:
    // For vertex 0: neighbors 1, 2 (indices 0-1)
    // For vertex 1: neighbors 0, 2, 3, 4 (indices 2-5)
    // For vertex 2: neighbors 0, 1, 3 (indices 6-8)
    // For vertex 3: neighbors 1, 2, 5 (indices 9-11)
    // For vertex 4: neighbors 1, 5 (indices 12-13)
    // For vertex 5: neighbors 3, 4 (indices 14-15)
    assert(csr.xadj.sameElements(Array(0, 2, 6, 9, 12, 14, 16)))

    // Verify that all the correct adjacency relationships are present
    // For each vertex, verify its neighbors are correctly represented
    val v0Neighbors = csr.adjncy.slice(csr.xadj(0), csr.xadj(1)).sorted
    val v1Neighbors = csr.adjncy.slice(csr.xadj(1), csr.xadj(2)).sorted
    val v2Neighbors = csr.adjncy.slice(csr.xadj(2), csr.xadj(3)).sorted
    val v3Neighbors = csr.adjncy.slice(csr.xadj(3), csr.xadj(4)).sorted
    val v4Neighbors = csr.adjncy.slice(csr.xadj(4), csr.xadj(5)).sorted
    val v5Neighbors = csr.adjncy.slice(csr.xadj(5), csr.xadj(6)).sorted

    assert(v0Neighbors.sameElements(Array(1, 2)))
    assert(v1Neighbors.sameElements(Array(0, 2, 3, 4)))
    assert(v2Neighbors.sameElements(Array(0, 1, 3)))
    assert(v3Neighbors.sameElements(Array(1, 2, 5)))
    assert(v4Neighbors.sameElements(Array(1, 5)))
    assert(v5Neighbors.sameElements(Array(3, 4)))
  }

  test("CSR conversion for star graph") {
    // Create a star-shaped graph with a central vertex connected to all others
    val vertices = Seq(
      Point(Array(0f, 0f), 0L), // Center
      Point(Array(1f, 0f), 1L), // Surrounding vertices
      Point(Array(0f, 1f), 2L),
      Point(Array(-1f, 0f), 3L),
      Point(Array(0f, -1f), 4L)
    )

    // All vertices connect only to vertex 0 (center)
    val edges = Set(
      (0, 1),
      (0, 2),
      (0, 3),
      (0, 4)
    )

    val graph = DelaunayGraph(vertices, edges)
    val csr   = GraphCSRTransformer.toCSR(graph)

    // Verify xadj: vertex 0 has 4 neighbors, others have 1 neighbor each
    assert(csr.xadj.sameElements(Array(0, 4, 5, 6, 7, 8)))

    // Check each vertex's neighbors
    val v0Neighbors = csr.adjncy.slice(csr.xadj(0), csr.xadj(1)).sorted
    assert(v0Neighbors.sameElements(Array(1, 2, 3, 4)))

    for (i <- 1 until 5) {
      val vNeighbors = csr.adjncy.slice(csr.xadj(i), csr.xadj(i + 1))
      assert(vNeighbors.sameElements(Array(0)))
    }
  }

  test("CSR conversion for line graph") {
    // Create a line graph: 0-1-2-3-4
    val vertices = Seq(
      Point(Array(0f, 0f), 0L),
      Point(Array(1f, 0f), 1L),
      Point(Array(2f, 0f), 2L),
      Point(Array(3f, 0f), 3L),
      Point(Array(4f, 0f), 4L)
    )

    val edges = Set(
      (0, 1),
      (1, 2),
      (2, 3),
      (3, 4)
    )

    val graph = DelaunayGraph(vertices, edges)
    val csr   = GraphCSRTransformer.toCSR(graph)

    // Verify xadj: endpoints have 1 neighbor, internal vertices have 2
    assert(csr.xadj.sameElements(Array(0, 1, 3, 5, 7, 8)))

    // Check neighbors for each vertex
    val neighborLists = Array(
      Array(1),    // Vertex 0 connects to 1
      Array(0, 2), // Vertex 1 connects to 0 and 2
      Array(1, 3), // Vertex 2 connects to 1 and 3
      Array(2, 4), // Vertex 3 connects to 2 and 4
      Array(3)     // Vertex 4 connects to 3
    )

    for (i <- vertices.indices) {
      val vNeighbors = csr.adjncy.slice(csr.xadj(i), csr.xadj(i + 1)).sorted
      assert(vNeighbors.sameElements(neighborLists(i)))
    }
  }

  test("CSR conversion for complete graph") {
    // Create a complete graph where every vertex connects to every other vertex
    val vertices = Seq(
      Point(Array(0f, 0f), 0L),
      Point(Array(1f, 0f), 1L),
      Point(Array(0f, 1f), 2L),
      Point(Array(1f, 1f), 3L)
    )

    val edges = Set(
      (0, 1),
      (0, 2),
      (0, 3),
      (1, 2),
      (1, 3),
      (2, 3)
    )

    val graph = DelaunayGraph(vertices, edges)
    val csr   = GraphCSRTransformer.toCSR(graph)

    // Each vertex connects to all 3 other vertices
    assert(csr.xadj.sameElements(Array(0, 3, 6, 9, 12)))

    // Check that each vertex has the correct neighbors
    for (i <- vertices.indices) {
      val vNeighbors        = csr.adjncy.slice(csr.xadj(i), csr.xadj(i + 1)).sorted
      val expectedNeighbors = (0 until vertices.length).filter(_ != i).toArray
      assert(vNeighbors.sameElements(expectedNeighbors))
    }
  }

  test("CSR conversion for graph with isolated vertices") {
    // Create a graph with some connected vertices and some isolated ones
    val vertices = Seq(
      Point(Array(0f, 0f), 0L),
      Point(Array(1f, 0f), 1L), // Isolated
      Point(Array(0f, 1f), 2L),
      Point(Array(1f, 1f), 3L), // Isolated
      Point(Array(2f, 0f), 4L)
    )

    val edges = Set(
      (0, 2),
      (0, 4),
      (2, 4)
    )

    val graph = DelaunayGraph(vertices, edges)
    val csr   = GraphCSRTransformer.toCSR(graph)

    // Verify xadj: isolated vertices have 0 neighbors
    assert(csr.xadj.sameElements(Array(0, 2, 2, 4, 4, 6)))

    // Check each vertex's neighbors
    val v0Neighbors = csr.adjncy.slice(csr.xadj(0), csr.xadj(1)).sorted
    val v1Neighbors = csr.adjncy.slice(csr.xadj(1), csr.xadj(2))
    val v2Neighbors = csr.adjncy.slice(csr.xadj(2), csr.xadj(3)).sorted
    val v3Neighbors = csr.adjncy.slice(csr.xadj(3), csr.xadj(4))
    val v4Neighbors = csr.adjncy.slice(csr.xadj(4), csr.xadj(5)).sorted

    assert(v0Neighbors.sameElements(Array(2, 4)))
    assert(v1Neighbors.isEmpty)
    assert(v2Neighbors.sameElements(Array(0, 4)))
    assert(v3Neighbors.isEmpty)
    assert(v4Neighbors.sameElements(Array(0, 2)))
  }

  test("CSR conversion for cycle graph") {
    // Create a cycle graph: 0-1-2-3-4-0
    val vertices = Seq(
      Point(Array(0f, 0f), 0L),
      Point(Array(1f, 1f), 1L),
      Point(Array(0f, 2f), 2L),
      Point(Array(-1f, 1f), 3L),
      Point(Array(-1f, -1f), 4L)
    )

    val edges = Set(
      (0, 1),
      (1, 2),
      (2, 3),
      (3, 4),
      (4, 0)
    )

    val graph = DelaunayGraph(vertices, edges)
    val csr   = GraphCSRTransformer.toCSR(graph)

    // Each vertex has exactly 2 neighbors in a cycle
    assert(csr.xadj.sameElements(Array(0, 2, 4, 6, 8, 10)))

    // Check that each vertex has the correct neighbors
    val expectedNeighbors = Array(
      Array(1, 4),
      Array(0, 2),
      Array(1, 3),
      Array(2, 4),
      Array(0, 3)
    )

    for (i <- vertices.indices) {
      val vNeighbors = csr.adjncy.slice(csr.xadj(i), csr.xadj(i + 1)).sorted
      assert(vNeighbors.sameElements(expectedNeighbors(i).sorted))
    }
  }

  test("CSR ordering property - neighbors should be sorted") {
    // Test that the neighbors in adjncy for each vertex are returned in sorted order
    val vertices = Seq(
      Point(Array(0f, 0f), 0L),
      Point(Array(1f, 0f), 1L),
      Point(Array(0f, 1f), 2L),
      Point(Array(1f, 1f), 3L),
      Point(Array(2f, 2f), 4L)
    )

    // Add edges in random order
    val edges = Set(
      (0, 4),
      (0, 2),
      (0, 3),
      (0, 1), // Vertex 0 connects to all others
      (1, 3),
      (2, 4) // Some additional connections
    )

    val graph = DelaunayGraph(vertices, edges)
    val csr   = GraphCSRTransformer.toCSR(graph)

    // For each vertex, verify that its neighbors are in sorted order in the CSR
    for (i <- vertices.indices) {
      val start     = csr.xadj(i)
      val end       = csr.xadj(i + 1)
      val neighbors = csr.adjncy.slice(start, end)

      // Check if neighbors are already sorted
      assert(
        neighbors.sameElements(neighbors.sorted),
        s"Neighbors for vertex $i are not sorted: ${neighbors.mkString(", ")}"
      )
    }
  }

}
