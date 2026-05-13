package spatial.index

import data.Point
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.concurrent.ExecutionContext

class ParKDTreeTest extends AnyFlatSpec with Matchers {

  implicit val ec: ExecutionContext = ExecutionContext.global

  private def mkPoint(id: Long, x: Float, y: Float): Point = Point(Array(x, y), id)

  "ParKDTree" should "return all points within radius for a simple layout" in {
    val points = Array(
      mkPoint(0, 0f, 0f),
      mkPoint(1, 1f, 0f),
      mkPoint(2, 0f, 1f),
      mkPoint(3, 3f, 3f),
      mkPoint(4, 2.9f, 3.1f)
    )
    val tree = ParKDTree(points).build()

    val r1 = tree.rangeQuery(points(0), 1.01f).map(_.id).sorted // should include 0,1,2
    r1 should contain theSameElementsAs Array(0L, 1L, 2L)

    // Distance between (3,3) and (2.9,3.1) ~ 0.1414 < 0.25 so both must be present
    val r2 = tree.rangeQuery(points(3), 0.25f).map(_.id).sorted
    r2 should contain theSameElementsAs Array(3L, 4L)

    // Larger radius still contains the same two points
    val r3 = tree.rangeQuery(points(3), 0.5f).map(_.id).sorted
    r3 should contain theSameElementsAs Array(3L, 4L)
  }

  it should "cache neighboring points results" in {
    val points = Array(
      mkPoint(0, 0f, 0f),
      mkPoint(1, 0.5f, 0f),
      mkPoint(2, 5f, 5f)
    )
    val tree = ParKDTree(points, cacheEnabled = true).build()

    val first = tree.neighboringPoints(points(0), 1.0f)
    first.map(_.id).sorted should contain theSameElementsAs Array(1L) // exclude itself
    val cacheSizeAfterFirst = tree.cacheSize
    cacheSizeAfterFirst should be >= 1

    val second = tree.neighboringPoints(points(0), 1.0f)
    second.map(_.id).sorted should contain theSameElementsAs Array(1L)
    (second eq first) shouldBe true
    tree.cacheSize shouldEqual cacheSizeAfterFirst
  }

  it should "not cache when disabled" in {
    val points = Array(mkPoint(0, 0f, 0f), mkPoint(1, 0.4f, 0f))
    val tree   = ParKDTree(points, cacheEnabled = false).build()
    val a      = tree.neighboringPoints(points(0), 1.0f)
    val b      = tree.neighboringPoints(points(0), 1.0f)
    a.map(_.id) should contain theSameElementsAs b.map(_.id)
    tree.cacheSize shouldEqual 0
  }

}
