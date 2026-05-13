package api

import cluster.{DBSCAN, DBSCANGrid}
import configuration.InputConfiguration
import data.Point
import metrics.MutualInformation

class VoronoiSCANAkkaTest extends AbstractVoronoiSCANTest {

  private val densired2ShrinkData = getDatasetFromResource("densired_2_shrink.csv")

  test("VoronoiSCANAkka is the same as DBSCAN") {
    val epsilon = 25
    val minPts  = 3
    val data    = getTestDataset

    val voronoiScan = getVoronoiSCAN(minPts, epsilon, 128, 2)
    val labels      = voronoiScan.fitTransform(data)
    logger.info(s"Number of clusters: ${labels.distinct.length}")
    val (labelsSequential, _) = new DBSCAN(epsilon, minPts).fit(data.zipWithIndex.map { case (point, idx) =>
      Point(point, idx.toLong)
    })
    val nmi = MutualInformation.normalizedMutualInfoScore(labelsSequential, labels)
    logger.info(s"NMI: $nmi")
    assert(nmi > 0.99, "Labels should match")
    // Sleep to allow for resource cleanup
    Thread.sleep(300)
  }

  test("VoronoiSCANAkka is the same as DBSCAN with grid sampler") {
    val epsilon = 25
    val minPts = 3
    val data = getTestDataset

    val voronoiScan = getVoronoiSCAN(minPts, epsilon, "data/icml/icml.csv", 128, 2, 100, "grid")
    val labels = voronoiScan.fitTransform(data)
    logger.info(s"Number of clusters: ${labels.distinct.length}")
    val (labelsSequential, _) = new DBSCAN(epsilon, minPts).fit(data.zipWithIndex.map { case (point, idx) =>
      Point(point, idx.toLong)
    })
    val nmi = MutualInformation.normalizedMutualInfoScore(labelsSequential, labels)
    logger.info(s"NMI: $nmi")
    assert(nmi > 0.99, "Labels should match")
    // Sleep to allow for resource cleanup
    Thread.sleep(300)
  }

  ignore("VoronoiSCANAkka matches DBSCAN on densired_2_shrink way too much cells") {
    val voronoiScan = getVoronoiSCAN(5, 0.5f, 128, 5)
    val labels      = voronoiScan.fitTransform(densired2ShrinkData)
    logger.info(s"Number of clusters: ${labels.distinct.length}")
    val (labelsSequential, _) = new DBSCANGrid(0.5f, 5).fit(densired2ShrinkData.zipWithIndex.map { case (point, idx) =>
      Point(point, idx.toLong)
    })
    assert(labels.length == labelsSequential.length, "Labels length should match data length")
    val nmi = MutualInformation.normalizedMutualInfoScore(labelsSequential, labels)
    logger.info(s"NMI: $nmi")
    assert(nmi > 0.99, "Labels should match")
  }

  override def getVoronoiSCAN(minPts: Int, eps: Float, nCells: Int, minCellDistance: Float): AbstractVoronoiSCAN =
    getVoronoiSCAN(minPts, eps, "data/icml/icml.csv", nCells, minCellDistance, 100)

  def getVoronoiSCAN(
      minPts: Int,
      eps: Float,
      inputPath: String,
      numCells: Int,
      minCellDistanceFactor: Float,
      batchSize: Int,
      sampler: String = "random"
  ): AbstractVoronoiSCAN = new VoronoiSCANAkka(
    Some(
      InputConfiguration(
        epsilon = eps, minPts = minPts, numCells = numCells, minCellDistanceFactor = minCellDistanceFactor,
        batchSize = batchSize, inputPath = inputPath, labelOutput = "memory", sampler = sampler, metricsPath = "",
        dbscanImpl = "grid", seed = 42
      )
    )
  )

}
