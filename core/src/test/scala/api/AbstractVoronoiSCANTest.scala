package api

import cluster.DBSCAN
import data.Point
import data.Point.Embedding
import metrics.MutualInformation
import org.scalatest.funsuite.AnyFunSuite
import org.slf4j.LoggerFactory

import scala.util.Using

trait VoronoiSCANSupplier {

  def getVoronoiSCAN(minPts: Int, eps: Float, nCells: Int, minCellEpsDistance: Float): AbstractVoronoiSCAN

}

abstract class AbstractVoronoiSCANTest extends AnyFunSuite with VoronoiSCANSupplier {

  val logger = LoggerFactory.getLogger(getClass)

  test("VoronoiSCAN should correctly cluster the ICML dataset") {
    val epsilon = 25
    val minPts = 3
    val data: Array[Embedding] = getTestDataset

    val voronoiScan = getVoronoiSCAN(minPts, epsilon, 25, 2)
    val labels      = voronoiScan.fitTransform(data)
    logger.info(s"Number of clusters: ${labels.distinct.length}")

    assert(labels.nonEmpty, "Labels should not be empty")
    assert(labels.length == data.length, "Labels length should match data length")

    val expectedNumberOfClusters = 4
    assert(labels.distinct.length == expectedNumberOfClusters, s"Expected $expectedNumberOfClusters clusters")

    // Compare the results with the expected labels
    val groundTruthLabels = getGroundTruthForTestDataset

    val nmi = MutualInformation.normalizedMutualInfoScore(groundTruthLabels, labels)
    // Because the assignment of border points is not deterministic in DBSCAN, NMI can be slightly lower
    assert(nmi > 0.99, "NMI should be greater than 0.99")
    logger.info(s"Normalized Mutual Information Score: $nmi")
    Thread.sleep(300)
  }

  private val datasetNames = List(
    "datasets/densired_1_eps0.015_minPts5.csv", "datasets/densired_2_eps0.015_minPts5.csv",
    "datasets/densired_3_eps0.015_minPts5.csv", "datasets/densired_4_eps0.015_minPts5.csv",
    "datasets/densired_5_eps0.015_minPts5.csv", "datasets/densired_6_eps0.015_minPts5.csv"
  )

  test("VoronoiSCAN is the same as DBSCAN on sample dataset") {
    testDataset("datasets/densired_2_eps0.015_minPts5.csv", 5, 0.015f)
  }

  datasetNames.foreach { datasetName =>
    test("VoronoiSCAN is the same as DBSCAN on " + datasetName) {
      testDataset(datasetName, 5, 0.015f)
    }
  }

  def testDataset(datasetName: String, minPts: Int, epsilon: Float): Unit = {
    val data = getDatasetFromResource(datasetName)

    val voronoiScan = getVoronoiSCAN(minPts, epsilon, 5, 5)
    val labels      = voronoiScan.fitTransform(data)
    logger.info(s"Number of clusters: ${labels.distinct.length}")
    val (labelsSequential, _) = new DBSCAN(epsilon, minPts).fit(data.zipWithIndex.map { case (point, idx) =>
      Point(point, idx.toLong)
    })
    assert(labels.length == labelsSequential.length, "Labels length should match data length")
    val nmi = MutualInformation.normalizedMutualInfoScore(labelsSequential, labels)
    logger.info(s"NMI: $nmi")
    assert(nmi > 0.99, "Labels should match")
    // Wait a bit to ensure the actor system has time to terminate
    Thread.sleep(10)
  }

  protected def getGroundTruthForTestDataset: Array[Int] =
    Using(scala.io.Source.fromResource("icml_labels.csv")) { source =>
      source
        .getLines()
        .drop(1)
        .map { line =>
          line.split(",").drop(2).map(_.strip().toInt).head
        }
        .toArray
    }.get

  protected def getTestDataset: Array[Embedding] =
    getDatasetFromResource("icml.csv")

  protected def getDatasetFromResource(path: String): Array[Embedding] = {
    Using(scala.io.Source.fromResource(path)) { source =>
      source
        .getLines()
        .drop(1)
        .map { line =>
          val values = line.split(",").map(_.toFloat)
          values
        }
        .toArray
    }.get
  }

}
