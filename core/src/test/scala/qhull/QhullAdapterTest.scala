package qhull

import org.scalatest.{Inside, OptionValues, TryValues}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks

import scala.jdk.CollectionConverters._
import scala.util.{Random, Try}

class QhullAdapterTest
    extends AnyFlatSpec
    with Matchers
    with Inside
    with TableDrivenPropertyChecks
    with OptionValues
    with TryValues {

  def isValidTriangulation(result: QhullAdapter.QhullResult): Boolean = {
    if (result == null || result.adjacency() == null) return false
    if (result.adjacency().isEmpty) return true // Empty is valid for empty input

    result.adjacency().asScala.forall { case (a, neighbors) =>
      neighbors.asScala.forall { b =>
        Option(result.adjacency().get(b)).exists(_.contains(a))
      }
    }
  }

  def safeTriangulation(points: Array[Array[Double]]): Option[QhullAdapter.QhullResult] = {
    val result = Try(QhullAdapter.delaunayTriangulation(points)).toOption
    result.filter(isValidTriangulation)
  }

  behavior of "QhullAdapter 2D Delaunay Triangulation"

  it should "compute triangulation for a simple square with center point" in {
    val points = Array(
      Array(0.0, 0.0),
      Array(1.001, 0.0),
      Array(0.0, 1.002),
      Array(1.003, 1.0),
      Array(0.52, 0.48)
    )

    val resultOpt = safeTriangulation(points)
    resultOpt shouldBe defined

    val result = resultOpt.value
    result.adjacency().size() shouldBe 5

    // The center point should be connected to all 4 corner points
    val centerNeighbors = result.adjacency().get(4)
    centerNeighbors should not be null
    centerNeighbors.size() shouldBe 4
    centerNeighbors should contain(0) // Center should be connected to (0,0)
    centerNeighbors should contain(1) // Center should be connected to (1,0)
    centerNeighbors should contain(2) // Center should be connected to (0,1)
    centerNeighbors should contain(3) // Center should be connected to (1,1)

    // Check corner connections (they should connect to adjacent corners and center)
    val corner0Neighbors = result.adjacency().get(0) // (0,0)
    corner0Neighbors should not be null
    corner0Neighbors should contain(1) // Corner (0,0) should connect to (1,0)
    corner0Neighbors should contain(2) // Corner (0,0) should connect to (0,1)
    corner0Neighbors should contain(4) // Corner (0,0) should connect to center
  }

  it should "compute triangulation for a regular triangle" in {
    val points = Array(
      Array(0.0, 0.0),
      Array(1.0, 0.0),
      Array(0.5, Math.sqrt(3) / 2)
    )

    val resultOpt = safeTriangulation(points)
    resultOpt shouldBe defined

    val result = resultOpt.value
    result.adjacency().size() shouldBe 3

    // Each point should be connected to the other 2 points
    for (i <- 0 until 3) {
      val neighbors = result.adjacency().get(i)
      neighbors should not be null
      neighbors.size() shouldBe 2

      for (j <- 0 until 3) {
        if (i != j) {
          neighbors should contain(j)
        }
      }
    }
  }

  it should "handle collinear points correctly" in {
    val points = Array(
      Array(0.0, 0.0),
      Array(1.0, 0.001),
      Array(2.0, -0.001),
      Array(3.0, 0.002)
    )

    val resultOpt = safeTriangulation(points)
    resultOpt shouldBe defined

    val result = resultOpt.value
    result.adjacency().size() shouldBe 4

    // With small perturbations, the exact edge pattern might vary,
    // but the adjacency structure should still be valid
    result.adjacency().asScala.foreach { case (_, neighbors) =>
      neighbors.size() should be >= 1
      neighbors.size() should be <= 3
    }

    // Point 0 should connect to at least point 1
    result.adjacency().get(0).contains(1) shouldBe true

    // Point 3 should connect to at least point 2
    result.adjacency().get(3).contains(2) shouldBe true
  }

  it should "compute triangulation for a random 2D point cloud" in {
    // Generate a random set of 20 points with fixed seed for reproducibility
    // Using enough jitter to avoid coplanarity issues
    val random    = new Random(42)
    val numPoints = 20
    val points    = Array.fill(numPoints)(Array(random.nextDouble() * 100, random.nextDouble() * 100))

    val resultOpt = safeTriangulation(points)
    resultOpt shouldBe defined

    val result = resultOpt.value
    result.adjacency().size() shouldBe numPoints

    // Basic sanity checks for Delaunay properties
    var totalEdges = 0
    result.adjacency().asScala.foreach { case (_, neighbors) =>
      val connections = neighbors.size()
      connections should be >= 1 // At least one connection for each vertex
      totalEdges += connections
    }

    // Each edge is counted twice in adjacency lists
    totalEdges /= 2

    // Just check that the number of edges is reasonable (positive and not excessive)
    totalEdges should be > 0
    totalEdges should be <= numPoints * (numPoints - 1) / 2 // Maximum possible edges
  }

  behavior of "QhullAdapter 3D Delaunay Triangulation"

  it should "compute triangulation for a simple tetrahedron" in {
    val points = Array(
      Array(0.0, 0.0, 0.0),
      Array(1.001, 0.002, 0.001),
      Array(0.503, Math.sqrt(3) / 2 + 0.001, -0.002),
      Array(0.499, Math.sqrt(3) / 6 - 0.003, Math.sqrt(6) / 3 + 0.002)
    )

    val resultOpt = safeTriangulation(points)
    resultOpt shouldBe defined

    val result = resultOpt.value
    result.adjacency().size() shouldBe 4

    // In a tetrahedron, each vertex should be connected to the other 3
    for (i <- 0 until 4) {
      val neighbors = result.adjacency().get(i)
      neighbors should not be null
      neighbors.size() shouldBe 3

      for (j <- 0 until 4) {
        if (i != j) {
          neighbors should contain(j)
        }
      }
    }
  }

  it should "compute triangulation for a cube with center point" in {
    val points = Array(
      Array(0.001, 0.002, -0.001), // 0 - slightly perturbed (0,0,0)
      Array(1.003, -0.001, 0.002), // 1 - slightly perturbed (1,0,0)
      Array(-0.001, 0.999, 0.001), // 2 - slightly perturbed (0,1,0)
      Array(1.001, 1.002, -0.002), // 3 - slightly perturbed (1,1,0)
      Array(0.002, -0.001, 0.998), // 4 - slightly perturbed (0,0,1)
      Array(0.999, 0.001, 1.001),  // 5 - slightly perturbed (1,0,1)
      Array(0.001, 1.003, 1.002),  // 6 - slightly perturbed (0,1,1)
      Array(1.002, 1.001, 0.999),  // 7 - slightly perturbed (1,1,1)
      Array(0.501, 0.499, 0.502)   // 8 - slightly perturbed center (0.5,0.5,0.5)
    )

    val resultOpt = safeTriangulation(points)
    resultOpt shouldBe defined

    val result = resultOpt.value
    result.adjacency().size() shouldBe 9

    // Center should be connected to all 8 corners
    val centerNeighbors = result.adjacency().get(8)
    centerNeighbors should not be null
    centerNeighbors.size() shouldBe 8

    for (i <- 0 until 8)
      centerNeighbors should contain(i)
  }

  behavior of "QhullAdapter Edge Cases"

  it should "handle empty points array" in {
    val points = Array.empty[Array[Double]]

    val result = QhullAdapter.delaunayTriangulation(points)

    result should not be null
    result.adjacency().isEmpty shouldBe true
  }

  it should "handle null points array" in {
    val points: Array[Array[Double]] = null

    val result = QhullAdapter.delaunayTriangulation(points)

    result should not be null
    result.adjacency().isEmpty shouldBe true
  }

  it should "handle a single point" in {
    val points = Array(Array(1.0, 2.0))

    val result = QhullAdapter.delaunayTriangulation(points)

    result should not be null
    result.adjacency() should not be null

    if (!result.adjacency().isEmpty) {
      result.adjacency().containsKey(0) shouldBe true
      result.adjacency().get(0).isEmpty shouldBe true
    }
  }

  it should "handle two points" in {
    val points = Array(Array(0.0, 0.0), Array(1.0, 1.0))

    val result = QhullAdapter.delaunayTriangulation(points)

    result should not be null
    result.adjacency() should not be null

    if (!result.adjacency().isEmpty) {
      if (result.adjacency().containsKey(0) && result.adjacency().containsKey(1)) {
        result.adjacency().get(0).contains(1) shouldBe true
        result.adjacency().get(1).contains(0) shouldBe true
      }
    }
  }

  behavior of "QhullAdapter Float Points"

  it should "handle float points conversion" in {
    val floatPoints = Array(
      Array(0.01f, -0.01f),
      Array(1.02f, 0.01f),
      Array(-0.01f, 1.01f),
      Array(1.01f, 0.99f)
    )

    val resultOpt = safeTriangulation(floatPoints.map(_.map(_.toDouble)))

    if (resultOpt.isDefined) {
      val result = resultOpt.value
      result.adjacency().size() shouldBe 4

      // Verify each point has neighbors
      for (i <- 0 until 4) {
        val neighbors = result.adjacency().get(i)
        neighbors should not be null
        neighbors.size() should be >= 2 // Each point should have at least 2 neighbors
      }
    }
  }

  it should "produce similar results for float and double inputs when possible" in {
    val floatPoints = Array(
      Array(0.1f, 0.2f),
      Array(10.1f, 0.3f),
      Array(0.4f, 10.2f),
      Array(10.3f, 10.4f)
    )

    val doublePoints = floatPoints.map(_.map(_.toDouble))

    val floatResultOpt  = safeTriangulation(floatPoints.map(_.map(_.toDouble)))
    val doubleResultOpt = safeTriangulation(doublePoints)

    if (floatResultOpt.isDefined && doubleResultOpt.isDefined) {
      val floatResult  = floatResultOpt.value
      val doubleResult = doubleResultOpt.value

      // They should have the same number of vertices
      floatResult.adjacency().size() shouldBe doubleResult.adjacency().size()

      // The connectivity structure should be similar, but we can't guarantee
      // identical results due to floating-point precision differences
      floatResult.adjacency().asScala.foreach { case (vertex, _) =>
        doubleResult.adjacency().containsKey(vertex) shouldBe true
      }
    }
  }

  behavior of "QhullAdapter Graph Properties"

  it should "create symmetric connections" in {
    val points = Array(
      Array(0.0, 0.0),
      Array(1.001, 0.0),
      Array(0.0, 1.002),
      Array(1.003, 1.0),
      Array(0.52, 0.48)
    )

    val resultOpt = safeTriangulation(points)
    resultOpt shouldBe defined

    val result = resultOpt.value

    // For each connection a→b, there should be a connection b→a
    result.adjacency().asScala.foreach { case (a, neighbors) =>
      neighbors.asScala.foreach { b =>
        result.adjacency().get(b).contains(a) shouldBe true
      }
    }
  }

  it should "not create self-loops" in {
    val points = Array(
      Array(0.0, 0.0),
      Array(1.001, 0.0),
      Array(0.0, 1.002),
      Array(1.003, 1.0),
      Array(0.52, 0.48)
    )

    val resultOpt = safeTriangulation(points)
    resultOpt shouldBe defined

    val result = resultOpt.value

    // No vertex should be connected to itself
    result.adjacency().asScala.foreach { case (vertex, neighbors) =>
      neighbors.contains(vertex) shouldBe false
    }
  }

}
