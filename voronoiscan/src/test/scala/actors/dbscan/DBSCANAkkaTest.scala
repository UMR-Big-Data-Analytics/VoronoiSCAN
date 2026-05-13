package actors.dbscan

import cluster.{DBSCANGrid, DBSCANTest}
import metrics.MutualInformation

class DBSCANAkkaTest extends DBSCANTest {

  datasetNames.foreach { ds =>
    it should s"match standard DBSCAN on dataset $ds" in {
      val points  = loadDataset(ds)
      val epsilon = 0.015f
      val minPts  = 5

      val grid     = new DBSCANGrid(epsilon, minPts)
      val (la, _)  = grid.fit(points)
      val gridAkka = new DBSCANGridAkka(epsilon, minPts)
      val (lb, _)  = gridAkka.fit(points)

      val nmi = MutualInformation.normalizedMutualInfoScore(la, lb)
      withClue(s"NMI on $ds was $nmi")(nmi should be > 0.99)
    }
  }

}
