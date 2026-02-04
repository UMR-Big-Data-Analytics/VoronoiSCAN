package spatial.index

import data.Point
import org.scalatest.funsuite.AnyFunSuite

import scala.collection.mutable.ArrayBuffer

class QuadTreeTest extends AnyFunSuite {

  private def createPoint(x: Float, y: Float, id: Int) = Point(Array(x, y), id)

  private def createPoint(x: Float, y: Float, z: Float, id: Int) = Point(Array(x, y, z), id)

  test("QuadTree should initialize correctly with non-empty points") {
    val points = Array(createPoint(1.0f, 2.0f, 0), createPoint(3.0f, 4.0f, 1), createPoint(5.0f, 6.0f, 2))
    val tree = new QuadTree(points).build()
    assert(tree.points.length == 3)
  }

  test("QuadTree should throw an exception with empty points") {
    assertThrows[IllegalArgumentException] {
      new QuadTree(Array.empty[Point]).build()
    }
  }

  test("QuadTree should handle 2D points correctly") {
    val points = Array(createPoint(1.0f, 1.0f, 0), createPoint(2.0f, 2.0f, 1), createPoint(3.0f, 3.0f, 2))
    val tree = new QuadTree(points).build()

    // Test range query to find points within radius
    val results = tree.rangeQuery(createPoint(2.1f, 2.1f, -1), 0.2f)
    assert(results.length == 1)
    assert(results.head == points(1))
  }

  test("QuadTree should handle 3D points correctly") {
    val points =
      Array(createPoint(1.0f, 1.0f, 1.0f, 0), createPoint(2.0f, 2.0f, 2.0f, 1), createPoint(3.0f, 3.0f, 3.0f, 2))
    val tree = new QuadTree(points).build()

    // Test range query to find points within radius
    val results = tree.rangeQuery(createPoint(2.1f, 2.1f, 2.1f, -1), 0.2f)
    assert(results.length == 1)
    assert(results.head == points(1))
  }

  test("QuadTree range query should handle edge cases") {
    val points = Array(createPoint(0.0f, 0.0f, 0), createPoint(1.0f, 0.0f, 1), createPoint(0.0f, 1.0f, 2))
    val tree = new QuadTree(points).build()

    // Test with zero range
    val zeroRangeResults = tree.rangeQuery(Array(0.0f, 0.0f), 0.0f)
    assert(zeroRangeResults.length == 1)
    assert(zeroRangeResults.head == points(0))

    // Test with range that includes all points
    val allPointsResults = tree.rangeQuery(Array(0.5f, 0.5f), 1.5f)
    assert(allPointsResults.length == 3)

    // Test with range that includes no points
    val noPointsResults = tree.rangeQuery(Array(10.0f, 10.0f), 0.1f)
    assert(noPointsResults.isEmpty)
  }

  test("QuadTree should handle identical points") {
    val points = Array(createPoint(1.0f, 1.0f, 0), createPoint(1.0f, 1.0f, 1))
    val tree = new QuadTree(points).build()

    val rangeResults = tree.rangeQuery(Array(1.0f, 1.0f), 0.1f)
    assert(rangeResults.length == 2)
  }

  test("QuadTree should handle points at extreme coordinates") {
    val points = Array(
      createPoint(Float.MaxValue, Float.MaxValue, 0),
      createPoint(Float.MinValue, Float.MinValue, 1),
      createPoint(0.0f, 0.0f, 2)
    )
    val tree = new QuadTree(points).build()

    // Test range query around origin
    val results = tree.rangeQuery(Array(0.0f, 0.0f), 1.0f)
    assert(results.length == 1)
    assert(results.head.id == 2)
  }

  test("QuadTree should handle different leaf capacities") {
    val points = Array.tabulate(10)(i => createPoint(i.toFloat, i.toFloat, i))

    // Test with smaller leaf capacity
    val tree1 = new QuadTree(ArrayBuffer.from(points), 2).build()
    val results1 = tree1.rangeQuery(Array(5.0f, 5.0f), 1.0f)
    assert(results1.nonEmpty)

    // Test with larger leaf capacity
    val tree2 = new QuadTree(ArrayBuffer.from(points), 10).build()
    val results2 = tree2.rangeQuery(Array(5.0f, 5.0f), 1.0f)
    assert(results2.nonEmpty)

    // Results should be the same regardless of leaf capacity
    assert(results1.map(_.id).sorted.sameElements(results2.map(_.id).sorted))
  }

  test("QuadTree should handle large datasets efficiently") {
    val numPoints = 1000
    val points = Array.tabulate(numPoints) { i =>
      val x = (i % 32).toFloat
      val y = (i / 32).toFloat
      createPoint(x, y, i)
    }

    val tree = new QuadTree(points).build()
    assert(tree.points.length == numPoints)

    // Test range query on large dataset
    val results = tree.rangeQuery(Array(15.0f, 15.0f), 2.0f)
    assert(results.nonEmpty)

    // Verify all returned points are within the specified range
    results.foreach { point =>
      val distance = math.sqrt(
        math.pow(point.vector(0) - 15.0f, 2) +
          math.pow(point.vector(1) - 15.0f, 2)
      )
      assert(distance <= 2.0f + 0.001) // Small tolerance for floating point precision
    }
  }

  test("QuadTree should handle sparse data distribution") {
    val points = Array(
      createPoint(0.0f, 0.0f, 0),
      createPoint(100.0f, 100.0f, 1),
      createPoint(200.0f, 200.0f, 2),
      createPoint(300.0f, 300.0f, 3)
    )

    val tree = new QuadTree(points).build()

    // Test range queries in sparse regions
    val results1 = tree.rangeQuery(Array(0.0f, 0.0f), 10.0f)
    assert(results1.length == 1)
    assert(results1.head.id == 0)

    val results2 = tree.rangeQuery(Array(150.0f, 150.0f), 100.0f)
    assert(results2.length == 2) // Should find points at (100,100) and (200,200)
  }

  test("QuadTree should handle clustered data") {
    val cluster1 = Array.tabulate(5)(i => createPoint(1.0f + i * 0.1f, 1.0f + i * 0.1f, i))
    val cluster2 = Array.tabulate(5)(i => createPoint(10.0f + i * 0.1f, 10.0f + i * 0.1f, i + 5))
    val points = cluster1 ++ cluster2

    val tree = new QuadTree(points).build()

    // Test range query within first cluster
    val results1 = tree.rangeQuery(Array(1.2f, 1.2f), 0.5f)
    assert(results1.length == 5) // Should find all points in cluster1

    // Test range query within second cluster
    val results2 = tree.rangeQuery(Array(10.2f, 10.2f), 0.5f)
    assert(results2.length == 5) // Should find all points in cluster2

    // Test range query between clusters
    val results3 = tree.rangeQuery(Array(5.0f, 5.0f), 1.0f)
    assert(results3.isEmpty) // Should find no points between clusters
  }

  test("QuadTree kNN search should work correctly") {
    val points = Array(
      createPoint(0.0f, 0.0f, 0),
      createPoint(1.0f, 0.0f, 1),
      createPoint(0.0f, 1.0f, 2),
      createPoint(2.0f, 2.0f, 3),
      createPoint(-1.0f, -1.0f, 4)
    )

    val tree = new QuadTree(points).build()

    // Test k=1 nearest neighbor
    val nearest1 = tree.kNearestNeighbors(Array(0.1f, 0.1f), 1)
    assert(nearest1.length == 1)
    assert(nearest1.head._1.id == 0) // Origin should be closest

    // Test k=3 nearest neighbors
    val nearest3 = tree.kNearestNeighbors(Array(0.0f, 0.0f), 3)
    assert(nearest3.length == 3)
    val ids = nearest3.map(_._1.id).toSet
    assert(ids.contains(0)) // Origin
    assert(ids.contains(1) || ids.contains(2)) // Either (1,0) or (0,1)
  }

  test("QuadTree should handle boundary conditions correctly") {
    val points = Array(
      createPoint(0.0f, 0.0f, 0),
      createPoint(0.5f, 0.5f, 1),
      createPoint(1.0f, 1.0f, 2)
    )

    val tree = new QuadTree(points).build()

    // Test exact boundary conditions - use radius that includes boundary points
    // Distance from (0.5, 0.5) to (0.0, 0.0) and (1.0, 1.0) is sqrt(0.5) ≈ 0.707
    val results1 = tree.rangeQuery(Array(0.5f, 0.5f), 0.8f)
    assert(results1.length >= 2) // Should include center point and boundary points

    // Test with very small radius
    val results2 = tree.rangeQuery(Array(0.5f, 0.5f), 0.001f)
    assert(results2.length == 1)
    assert(results2.head.id == 1)
  }

  test("QuadTree should maintain accuracy with floating point precision") {
    val points = Array(
      createPoint(0.1f, 0.1f, 0),
      createPoint(0.2f, 0.2f, 1),
      createPoint(0.3f, 0.3f, 2)
    )

    val tree = new QuadTree(points).build()

    // Test with precise floating point calculations
    val queryPoint = Array(0.15f, 0.15f)
    val radius = math.sqrt(0.005).toFloat // Distance to (0.1, 0.1) and (0.2, 0.2)

    val results = tree.rangeQuery(queryPoint, radius)
    assert(results.length >= 1) // Should find at least one point within radius

    // Verify distances are within expected range
    results.foreach { point =>
      val actualDistance = math.sqrt(
        math.pow(point.vector(0) - queryPoint(0), 2) +
          math.pow(point.vector(1) - queryPoint(1), 2)
      )
      assert(actualDistance <= radius + 0.0001) // Small tolerance for floating point errors
    }
  }

  test("QuadTree should handle high-dimensional data") {
    val dimensions = 5
    val points = Array.tabulate(10) { i =>
      val coords = Array.tabulate(dimensions)(d => (i + d).toFloat)
      Point(coords, i)
    }

    val tree = new QuadTree(points).build()
    assert(tree.points.length == 10)

    // Test range query in high dimensions
    val queryPoint = Array.tabulate(dimensions)(d => 5.0f + d)
    val results = tree.rangeQuery(queryPoint, 2.0f)
    assert(results.nonEmpty)
  }

  test("QuadTree should return consistent results across multiple queries") {
    val points = Array.tabulate(20)(i => createPoint((i % 5).toFloat, (i / 5).toFloat, i))
    val tree = new QuadTree(points).build()

    val queryPoint = Array(2.0f, 2.0f)
    val radius = 1.5f

    // Run the same query multiple times
    val results1 = tree.rangeQuery(queryPoint, radius)
    val results2 = tree.rangeQuery(queryPoint, radius)
    val results3 = tree.rangeQuery(queryPoint, radius)

    // Results should be identical
    assert(results1.map(_.id).sorted.sameElements(results2.map(_.id).sorted))
    assert(results2.map(_.id).sorted.sameElements(results3.map(_.id).sorted))
  }

}
