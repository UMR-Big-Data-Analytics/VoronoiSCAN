package spatial.index

import data.Point
import org.scalatest.funsuite.AnyFunSuite

import scala.collection.mutable.ArrayBuffer

class KDTreeTest extends AnyFunSuite {

  private def createPoint(x: Float, y: Float, id: Int) = Point(Array(x, y), id)

  private def createPoint(x: Float, y: Float, z: Float, id: Int) = Point(Array(x, y, z), id)

  test("KDTree should initialize correctly with non-empty points") {
    val points = Array(createPoint(1.0f, 2.0f, 0), createPoint(3.0f, 4.0f, 1), createPoint(5.0f, 6.0f, 2))
    val tree   = new KDTree(points).build()
    assert(tree.root.isDefined)
    assert(tree.points.length == 3)
  }

  test("KDTree should throw an exception with empty points") {
    assertThrows[IllegalArgumentException] {
      new KDTree(Array.empty[Point]).build()
    }
  }

  test("KDTree should handle multiple insertions and maintain structure") {
    val initialPoints = Array(createPoint(0.0f, 0.0f, 0))
    val tree          = new KDTree(initialPoints).build()

    val newPoints = Seq(createPoint(1.0f, 1.0f, 1), createPoint(-1.0f, -1.0f, 2), createPoint(2.0f, 2.0f, 3))

    newPoints.foreach(tree.insert)
    assert(tree.points.length == 4)

    // Verify nearest neighbor queries still work correctly
    val (nearest1, distance1) = tree.nearestNeighbor(createPoint(0.9f, 0.9f, -1))
    assert(nearest1 == newPoints(0))
    // Distance from (0.9, 0.9) to (1.0, 1.0) should be sqrt((1.0-0.9)^2 + (1.0-0.9)^2) = sqrt(0.02) ≈ 0.141
    assert(math.abs(distance1 - math.sqrt(0.02f)) < 0.001)

    val (nearest2, distance2) = tree.nearestNeighbor(createPoint(-0.8f, -0.8f, -1))
    assert(nearest2 == newPoints(1))
    // Distance from (-0.8, -0.8) to (-1.0, -1.0) should be sqrt((-1.0-(-0.8))^2 + (-1.0-(-0.8))^2) = sqrt(0.08) ≈ 0.283
    assert(math.abs(distance2 - math.sqrt(0.08f)) < 0.001)
  }

  test("KDTree should handle 3D points correctly") {
    val points =
      Array(createPoint(1.0f, 1.0f, 1.0f, 0), createPoint(2.0f, 2.0f, 2.0f, 1), createPoint(3.0f, 3.0f, 3.0f, 2))
    val tree = new KDTree(points).build()

    val (nearest, distance) = tree.nearestNeighbor(createPoint(2.1f, 2.1f, 2.1f, -1))
    assert(nearest == points(1))
    // Distance from (2.1, 2.1, 2.1) to (2.0, 2.0, 2.0) should be sqrt(3 * 0.1^2) = sqrt(0.03) ≈ 0.173
    assert(math.abs(distance - math.sqrt(0.03f)) < 0.001)
  }

  test("KDTree should handle rebalancing after multiple insertions") {
    val tree = new KDTree(ArrayBuffer(createPoint(0.0f, 0.0f, 0)), rebalanceThreshold = 3)

    // Insert points that would create an unbalanced structure
    (1 to 5).foreach { i =>
      tree.insert(createPoint(i.toFloat, 0.0f, i))
    }

    // Verify tree is still functional after rebalancing
    val (nearest, distance) = tree.nearestNeighbor(createPoint(2.2f, 0.0f, -1))
    assert(nearest.id == 2)
    // Distance from (2.2, 0.0) to (2.0, 0.0) should be 0.2
    assert(math.abs(distance - 0.2f) < 0.001)
  }

  test("KDTree should handle deletions and maintain structure") {
    val points =
      Array(
        createPoint(1.0f, 1.0f, 0),
        createPoint(2.0f, 2.0f, 1),
        createPoint(3.0f, 3.0f, 2),
        createPoint(4.0f, 4.0f, 3)
      )
    val tree = new KDTree(points).build()

    tree.delete(createPoint(2.0f, 2.0f, 1))
    assert(!tree.points.exists(_.vector.sameElements(Array(2.0f, 2.0f))))

    // Verify nearest neighbor still works after deletion
    val (nearest, distance) = tree.nearestNeighbor(createPoint(2.1f, 2.1f, -1))
    assert(nearest.vector.sameElements(Array(3.0f, 3.0f)))
    // Distance from (2.1, 2.1) to (3.0, 3.0) should be sqrt((3-2.1)^2 + (3-2.1)^2) = sqrt(2 * 0.9^2) = sqrt(1.62) ≈ 1.273
    assert(math.abs(distance - math.sqrt(1.62f)) < 0.001)
  }

  test("KDTree range query should handle edge cases") {
    val points = Array(createPoint(0.0f, 0.0f, 0), createPoint(1.0f, 0.0f, 1), createPoint(0.0f, 1.0f, 2))
    val tree   = new KDTree(points).build()

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

  test("KDTree should handle identical points") {
    val points = Array(createPoint(1.0f, 1.0f, 0), createPoint(1.0f, 1.0f, 1))
    val tree   = new KDTree(points).build()

    val rangeResults = tree.rangeQuery(Array(1.0f, 1.0f), 0.1f)
    assert(rangeResults.length == 2)

    val (nearest, distance) = tree.nearestNeighbor(createPoint(1.0f, 1.0f, -1))
    assert(nearest.vector.sameElements(Array(1.0f, 1.0f)))
    // Distance should be 0 for identical points
    assert(distance == 0.0f)
  }

  test("KDTree should handle points at extreme coordinates") {
    val points = Array(
      createPoint(Float.MaxValue, Float.MaxValue, 0),
      createPoint(Float.MinValue, Float.MinValue, 1),
      createPoint(0.0f, 0.0f, 2)
    )
    val tree = new KDTree(points).build()

    val (nearest, distance) = tree.nearestNeighbor(createPoint(0.0f, 0.0f, -1))
    assert(nearest.id == 2)
    // Distance should be 0 for identical points
    assert(distance == 0.0f)
  }

  test("KDTree should maintain accuracy after multiple operations") {
    val tree = new KDTree(Array(createPoint(0.0f, 0.0f, 0))).build()

    // First, verify initial state
    assert(tree.points.length == 1)

    // Test single insert
    val newPoint = createPoint(1.0f, 1.0f, 1)
    tree.insert(newPoint)

    // Verify the point was actually inserted
    assert(
      tree.points.exists(p => p.id == 1 && p.vector.sameElements(Array(1.0f, 1.0f))),
      "Inserted point not found in the tree"
    )

    // Verify nearest neighbor works
    val (nearest1, distance1) = tree.nearestNeighbor(createPoint(0.9f, 0.9f, -1))
    assert(nearest1.id == 1, s"Expected point with id 1, but got ${nearest1.id}")
    // Distance from (0.9, 0.9) to (1.0, 1.0) should be sqrt(0.02) ≈ 0.141
    assert(math.abs(distance1 - math.sqrt(0.02f)) < 0.001)

    // Do a range query to verify both points are accessible
    val rangeResults = tree.rangeQuery(Array(0.5f, 0.5f), 1.0f)
    assert(rangeResults.length == 2, s"Expected 2 points in range, but got ${rangeResults.length}")
  }

  test("KDTree should return correct distances for various scenarios") {
    val points = Array(createPoint(0.0f, 0.0f, 0), createPoint(3.0f, 4.0f, 1), createPoint(1.0f, 1.0f, 2))
    val tree = new KDTree(points).build()

    // Test distance calculation for a 3-4-5 triangle
    val (nearest1, distance1) = tree.nearestNeighbor(createPoint(3.0f, 4.0f, -1))
    assert(nearest1.id == 1)
    assert(distance1 == 0.0f) // Exact match

    // Test distance to origin
    val (nearest2, distance2) = tree.nearestNeighbor(createPoint(0.0f, 0.0f, -1))
    assert(nearest2.id == 0)
    assert(distance2 == 0.0f) // Exact match

    // Test known distance calculation: from (0,0) the nearest to (6,8) should be (3,4) with distance 5
    val (nearest3, distance3) = tree.nearestNeighbor(createPoint(6.0f, 8.0f, -1))
    assert(nearest3.id == 1) // Point (3,4) should be nearest
    assert(math.abs(distance3 - 5.0f) < 0.001) // Distance from (6,8) to (3,4) is sqrt((6-3)^2 + (8-4)^2) = 5
  }

}
