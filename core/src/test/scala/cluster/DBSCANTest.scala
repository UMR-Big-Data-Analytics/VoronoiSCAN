package cluster

import data.Point
import data.Point.Embedding
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.util.{Random, Using}

trait DBSCANTest extends AnyFlatSpec with Matchers {

  protected def samePartition(labelsA: Array[Int], labelsB: Array[Int]): Boolean = {
    require(labelsA.length == labelsB.length, "Label arrays must be the same length")

    // Noise must match 1:1
    val noiseA = labelsA.zipWithIndex.collect { case (l, i) if l == -1 => i }.toSet
    val noiseB = labelsB.zipWithIndex.collect { case (l, i) if l == -1 => i }.toSet
    if (noiseA != noiseB) return false

    // Cluster membership equivalence for all pairs of non-noise points
    val n = labelsA.length
    for {
      i <- 0 until n
      j <- i + 1 until n
    } {
      val aSame = labelsA(i) != -1 && labelsA(j) != -1 && labelsA(i) == labelsA(j)
      val bSame = labelsB(i) != -1 && labelsB(j) != -1 && labelsB(i) == labelsB(j)
      if (aSame != bSame) return false
    }
    true
  }

  protected def asIdSet(nested: Array[Array[Point]]): Set[Long] = nested.flatten.map(_.id).toSet

  protected def mkCluster(center: Embedding, points: Int, spread: Float, startId: Long, rnd: Random): Array[Point] = {
    val dim = center.length
    Array.tabulate(points) { i =>
      val vec = Array.tabulate(dim)(d => center(d) + (rnd.nextFloat() - 0.5f) * spread)
      Point(vec, startId + i)
    }
  }

  protected def mkTwoSeparatedClusters(
      dim: Int,
      sizePerCluster: Int,
      spread: Float,
      gap: Float,
      seed: Int
  ): Array[Point] = {
    val rnd     = new Random(seed)
    val center1 = Array.fill(dim)(0.0f)
    val center2 = Array.fill(dim)(gap)
    val c1      = mkCluster(center1, sizePerCluster, spread, 0L, rnd)
    val c2      = mkCluster(center2, sizePerCluster, spread, sizePerCluster, rnd)
    c1 ++ c2
  }

  protected def loadDataset(resourcePath: String): Array[Point] = {
    val data: Array[Embedding] = Using(scala.io.Source.fromResource(resourcePath)) { source =>
      source
        .getLines()
        .drop(1)
        .map { line =>
          line.split(",").map(_.toFloat)
        }
        .toArray
    }.get
    data.zipWithIndex.map { case (coords, idx) => Point(coords, idx.toLong) }
  }

  protected val datasetNames: Seq[String] = Seq(
    "datasets/densired_1_eps0.015_minPts5.csv", "datasets/densired_2_eps0.015_minPts5.csv",
    "datasets/densired_3_eps0.015_minPts5.csv", "datasets/densired_4_eps0.015_minPts5.csv",
    "datasets/densired_5_eps0.015_minPts5.csv", "datasets/densired_6_eps0.015_minPts5.csv"
  )

  protected def setsEqualIgnoringOrderAndClusterId(a: Array[Set[Long]], b: Array[Set[Long]]): Boolean = {
    val aSets = a.map(_.toSet).filter(_.nonEmpty)
    val bSets = b.map(_.toSet).filter(_.nonEmpty)
    aSets.forall(s => bSets.contains(s)) && bSets.forall(s => aSets.contains(s))
  }

}
