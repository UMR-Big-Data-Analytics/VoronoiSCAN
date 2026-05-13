package cluster

import data.Point
import metrics.MutualInformation

import scala.util.Random

class DBSCANGridTest extends DBSCANTest {

  "DBSCANGrid" should "match standard DBSCAN on two well-separated clusters" in {
    val dim     = 2
    val points  = mkTwoSeparatedClusters(dim = dim, sizePerCluster = 50, spread = 0.1f, gap = 5.0f, seed = 42)
    val epsilon = 0.3f
    val minPts  = 5

    val dbscan          = new DBSCAN(epsilon, minPts)
    val (labelsA, ptsA) = dbscan.fit(points)

    val grid            = new DBSCANGrid(epsilon, minPts)
    val (labelsB, ptsB) = grid.fit(points)

    // Points order should be preserved
    ptsA.map(_.id) should equal(points.map(_.id))
    ptsB.map(_.id) should equal(points.map(_.id))

    // Same partitioning modulo label renaming
    samePartition(labelsA, labelsB) shouldBe true

    // Same number of clusters
    dbscan.getNumClusters should equal(grid.getNumClusters)

    // Core and border sets should match (order across clusters may differ)
    val aCoreIds   = asIdSet(dbscan.getCorePoints)
    val bCoreIds   = asIdSet(grid.getCorePoints)
    val aBorderIds = asIdSet(dbscan.getBorderPoints)
    val bBorderIds = asIdSet(grid.getBorderPoints)

    aCoreIds should equal(bCoreIds)
    aBorderIds should equal(bBorderIds)
  }

  it should "match on random data with moderate epsilon" in {
    val rnd     = new Random(7)
    val dim     = 3
    val n       = 200
    val points  = Array.tabulate(n)(i => Point(Array.fill(dim)(rnd.nextFloat() * 10f), i))
    val epsilon = 0.6f
    val minPts  = 4

    val dbscan       = new DBSCAN(epsilon, minPts)
    val (labelsA, _) = dbscan.fit(points)

    val grid         = new DBSCANGrid(epsilon, minPts)
    val (labelsB, _) = grid.fit(points)

    samePartition(labelsA, labelsB) shouldBe true
    dbscan.getNumClusters should equal(grid.getNumClusters)

    if (!labelsA.sameElements(labelsB)) {
      val nmi = MutualInformation.normalizedMutualInfoScore(labelsA, labelsB)
      nmi should be > 0.99
    }

    asIdSet(dbscan.getCorePoints) should equal(asIdSet(grid.getCorePoints))
    asIdSet(dbscan.getBorderPoints) should equal(asIdSet(grid.getBorderPoints))
  }

  it should "throw IllegalArgumentException on empty input just like DBSCAN" in {
    val epsilon = 0.5f
    val minPts  = 3

    val grid    = new DBSCANGrid(epsilon, minPts)
    val classic = new DBSCAN(epsilon, minPts)

    classic.fit(Array.empty[Point])._1.length shouldBe 0
    grid.fit(Array.empty[Point])._1.length shouldBe 0
  }

  it should "produce the same result with and without parallelism" in {
    val points = mkTwoSeparatedClusters(dim = 2, sizePerCluster = 80, spread = 0.15f, gap = 4.0f, seed = 99)
    val epsilon = 0.35f
    val minPts = 5

    val sequential = new DBSCANGrid(epsilon, minPts, parallelism = false)
    val parallel = new DBSCANGrid(epsilon, minPts, parallelism = true)

    val (sequentialLabels, _) = sequential.fit(points)
    val (parallelLabels, _) = parallel.fit(points)

    samePartition(sequentialLabels, parallelLabels) shouldBe true
    sequential.getNumClusters should equal(parallel.getNumClusters)
    asIdSet(sequential.getCorePoints) should equal(asIdSet(parallel.getCorePoints))
    asIdSet(sequential.getBorderPoints) should equal(asIdSet(parallel.getBorderPoints))
  }

  datasetNames.foreach { ds =>
    it should s"match standard DBSCAN on dataset $ds" in {
      val points  = loadDataset(ds)
      val epsilon = 0.015f
      val minPts  = 5

      val classic = new DBSCAN(epsilon, minPts)
      val (la, _) = classic.fit(points)
      val grid    = new DBSCANGrid(epsilon, minPts)
      val (lb, _) = grid.fit(points)

      val nmi = MutualInformation.normalizedMutualInfoScore(la, lb)
      withClue(s"NMI on $ds was $nmi")(nmi should be > 0.99)

      // Compare core and border point memberships irrespective of cluster label ordering
      def toArrayOfIdSets(nested: Array[Array[Point]]): Array[Set[Long]] =
        nested.map(arr => arr.map(_.id).toSet)

      val classicCore   = toArrayOfIdSets(classic.getCorePoints)
      val gridCore      = toArrayOfIdSets(grid.getCorePoints)
      val classicBorder = toArrayOfIdSets(classic.getBorderPoints)
      val gridBorder    = toArrayOfIdSets(grid.getBorderPoints)
      // Print histogram of core and border points for debugging
      println(s"Classic core: ${classicCore.map(_.size).sorted.mkString(", ")}")
      println(s"Grid core: ${gridCore.map(_.size).sorted.mkString(", ")}")
      println(s"Classic border: ${classicBorder.map(_.size).sorted.mkString(", ")}")
      println(s"Grid border: ${gridBorder.map(_.size).sorted.mkString(", ")}")

      val classicBorderCount = classicBorder.map(_.size).sum
      val gridBorderCount = gridBorder.map(_.size).sum
      withClue(s"Border point count difference: ${math.abs(classicBorderCount - gridBorderCount)}") {
        math.abs(classicBorderCount - gridBorderCount) should be <= 2
      }
      setsEqualIgnoringOrderAndClusterId(classicCore, gridCore) shouldBe true
    }
  }

}
