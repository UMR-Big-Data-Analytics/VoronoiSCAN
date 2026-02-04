package api

import cluster.{DBSCAN, DBSCANGrid}
import configuration.InputConfiguration
import data.Point
import metrics.MutualInformation

class VoronoiSCANAkkaTest extends AbstractVoronoiSCANTest {

  private val densired2ShrinkData = getDatasetFromResource("densired_2_shrink.csv")

  private val parameters: Seq[(Int, Float, Int)] = Seq(
    (5, 0.015f, 16),
    (10, 0.02f, 16),
    (15, 0.025f, 16),
    (5, 0.015f, 32),
    (10, 0.02f, 32),
    (15, 0.025f, 32),
    (5, 0.015f, 64),
    (10, 0.02f, 64),
    (15, 0.025f, 64)
  )

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

  parameters.foreach { case (minPts, eps, nCells) =>
    test(s"VoronoiSCANAkka matches DBSCAN on densired_2_shrink with minPts=$minPts and eps=$eps with $nCells cells") {
      val voronoiScan = getVoronoiSCAN(minPts, eps, nCells, 5)
      val labels      = voronoiScan.fitTransform(densired2ShrinkData)
      logger.info(s"Number of clusters: ${labels.distinct.length}")
      val (labelsSequential, _) = new DBSCAN(eps, minPts).fit(densired2ShrinkData.zipWithIndex.map {
        case (point, idx) => Point(point, idx.toLong)
      })
      assert(labels.length == labelsSequential.length, "Labels length should match data length")
      val nmi = MutualInformation.normalizedMutualInfoScore(labelsSequential, labels)
      logger.info(s"NMI: $nmi")
      assert(nmi > 0.99, "Labels should match")
    }
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
