package actors.dbscan

import cluster.DBSCANGrid
import data.Point
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class DBSCANAkkaEdgeCasesTest extends AnyFlatSpec with Matchers {

  behavior of "DBSCANGridAkka edge cases"

  it should "complete when all points are core (single dense cell, no border cells)" in {
    val dim       = 2
    val epsilon   = 0.05f // Large enough so all points end up in one grid cell and are neighbors
    val minPts    = 5
    val numPoints = 50
    val points = (0 until numPoints).map { i =>
      // tightly pack points within a very small square so they fall into same cell
      val x = 0.001f * (i % 5)
      val y = 0.001f * (i / 5)
      Point(Array(x, y), i.toLong)
    }.toArray

    val gridSeq         = new DBSCANGrid(epsilon, minPts)
    val (labelsSeq, _)  = gridSeq.fit(points)
    val gridAkka        = new DBSCANGridAkka(epsilon, minPts)
    val (labelsAkka, _) = gridAkka.fit(points)

    // All labels (excluding noise) should map to a single cluster id in each implementation
    labelsSeq.filter(_ != -1).distinct.length shouldBe 1
    labelsAkka.filter(_ != -1).distinct.length shouldBe 1
    // No noise expected
    labelsSeq.count(_ == -1) shouldBe 0
    labelsAkka.count(_ == -1) shouldBe 0
    // Implementations should match exactly on labels (since single cluster)
    (labelsSeq should contain).theSameElementsInOrderAs(labelsAkka)
  }

}
