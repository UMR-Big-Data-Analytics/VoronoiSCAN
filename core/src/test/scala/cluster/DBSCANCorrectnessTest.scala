package cluster

import data.Point
import data.Point.Embedding
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.util.Random

class DBSCANCorrectnessTest extends AnyFlatSpec with Matchers {

  private def samePartition(labelsA: Array[Int], labelsB: Array[Int]): Boolean = {
    require(labelsA.length == labelsB.length)
    val n = labelsA.length
    // Noise must match
    val noiseA = labelsA.indices.filter(i => labelsA(i) == -1).toSet
    val noiseB = labelsB.indices.filter(i => labelsB(i) == -1).toSet
    if (noiseA != noiseB) return false
    // Pairwise equivalence for non-noise
    var i = 0
    while (i < n) {
      var j = i + 1
      while (j < n) {
        val aSame = labelsA(i) != -1 && labelsA(j) != -1 && labelsA(i) == labelsA(j)
        val bSame = labelsB(i) != -1 && labelsB(j) != -1 && labelsB(i) == labelsB(j)
        if (aSame != bSame) return false
        j += 1
      }
      i += 1
    }
    true
  }

  private def mkCluster(center: Embedding, points: Int, spread: Float, startId: Long, rnd: Random): Array[Point] = {
    val dim = center.length
    Array.tabulate(points) { i =>
      val vec = Array.tabulate(dim)(d => center(d) + (rnd.nextFloat() - 0.5f) * spread)
      Point(vec, startId + i)
    }
  }

  "DBSCAN" should "find two well-separated clusters" in {
    val rnd = new Random(123)
    val c1 = mkCluster(Array(0f, 0f), 60, 0.1f, 0, rnd)
    val c2 = mkCluster(Array(5f, 5f), 60, 0.1f, 1000, rnd)
    val points = c1 ++ c2
    val db = new DBSCAN(epsilon = 0.3f, minPts = 5)
    val (labels, pts) = db.fit(points)
    labels.length shouldBe points.length
    db.getNumClusters shouldBe 2
    // All non-noise should belong to two groups
    labels.count(_ == -1) shouldBe 0
    // Order preserved
    pts.map(_.id) should equal(points.map(_.id))
  }

  it should "be order-invariant (up to relabeling)" in {
    val rnd = new Random(7)
    val points = Array.tabulate(120)(i => Point(Array(rnd.nextFloat() * 10f, rnd.nextFloat() * 10f), i))
    val db = new DBSCAN(0.6f, 4)
    val (labelsA, _) = db.fit(points)
    val shuffled = rnd.shuffle(points.toSeq).toArray
    val (labelsB, _) = db.fit(shuffled)
    // Map shuffled labels back to original order
    val idToPos = shuffled.indices.map(i => shuffled(i).id -> i).toMap
    val labelsBReordered = points.indices.map(i => labelsB(idToPos(points(i).id))).toArray
    samePartition(labelsA, labelsBReordered) shouldBe true
  }

  it should "mark isolated points as noise" in {
    val points = Array(Point(Array(0f, 0f), 1), Point(Array(100f, 100f), 2), Point(Array(-100f, -100f), 3))
    val db = new DBSCAN(0.5f, 2)
    val (labels, _) = db.fit(points)
    labels.count(_ == -1) shouldBe 3
    db.getNumClusters shouldBe 0
  }

  it should "throw on empty input" in {
    val db = new DBSCAN(0.5f, 2)
    db.fit(Array.empty[Point])._1.length shouldBe 0
  }

}
