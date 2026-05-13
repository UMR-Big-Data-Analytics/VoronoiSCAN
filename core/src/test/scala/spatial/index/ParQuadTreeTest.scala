package spatial.index

import data.Point
import data.Point.Embedding
import org.scalatest.Ignore
import org.scalatest.funsuite.AnyFunSuite

import scala.concurrent.ExecutionContext

@Ignore
class ParQuadTreeTest extends AnyFunSuite {

  implicit val ec: ExecutionContext = ExecutionContext.global

  private def pt(coords: Embedding, id: Int) = Point(coords, id)

  private def pt(x: Float, y: Float, id: Int) = Point(Array(x, y), id)

  test("ParallelQuadTree basic range parity with QuadTree") {
    val points = Array.tabulate(200)(i => pt(i % 20, i / 20, i))
    val q1     = new QuadTree(points).build()
    val q2     = new ParQuadTree(points.clone()) // clone to avoid reordering affecting original

    val query = Array(5.5f, 4.5f)
    val r1    = q1.rangeQuery(query, 3.0f).map(_.id).sorted
    val r2    = q2.rangeQuery(query, 3.0f).map(_.id).sorted
    assert(r1.sameElements(r2))
  }

  test("ParallelQuadTree kNN parity small k") {
    val points = Array(pt(0f, 0f, 0), pt(1f, 0f, 1), pt(0f, 1f, 2), pt(2f, 2f, 3), pt(-1f, -1f, 4), pt(3f, 3f, 5))
    val q1     = new QuadTree(points).build()
    val q2     = new ParQuadTree(points.clone())

    val k     = 3
    val query = Array(0.1f, 0.1f)
    val n1    = q1.kNearestNeighbors(query, k).map(_._1.id).toSet
    val n2    = q2.kNearestNeighbors(query, k).map(_._1.id).toSet
    assert(n1 == n2)
  }

  test("ParallelQuadTree handles identical points (path compression)") {
    val points = Array.fill(50)(pt(1.0f, 1.0f, 0))
    val q      = new ParQuadTree(points.clone(), leafCapacity = 4, parallelThreshold = 8)
    val res    = q.rangeQuery(Array(1.0f, 1.0f), 0.01f)
    assert(res.nonEmpty)
  }

  test("ParallelQuadTree single point") {
    val points = Array(pt(2.2f, -3.1f, 42))
    val q      = new ParQuadTree(points.clone())
    val r      = q.rangeQuery(Array(2.2f, -3.1f), 0.0f)
    assert(r.length == 1 && r.head.id == 42)
  }

}
