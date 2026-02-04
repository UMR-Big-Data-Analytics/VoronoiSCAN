package cluster

import scala.collection.parallel.CollectionConverters.ArrayIsParallelizable

sealed class ConnectivityCheck(parallel: Boolean) {

  def isConnected(
                   g: Cell,
                   h: Cell,
                   coreFlags: collection.Map[Int, Boolean],
                   epsilon: Float
                 ): Boolean = {

    // Get only the core points from each cell
    val corePointsG = g.getPoints.filter(p => coreFlags.getOrElse(p.id.toInt, false))
    val corePointsH = h.getPoints.filter(p => coreFlags.getOrElse(p.id.toInt, false))

    if (corePointsG.isEmpty || corePointsH.isEmpty) {
      return false
    }

    //  Identify the smaller set to iterate over and the larger cell to query against
    val (smallerSet, largerCell) = if (corePointsG.length < corePointsH.length) {
      (corePointsG, h)
    } else {
      (corePointsH, g)
    }

    if (parallel) {
      smallerSet.par.exists { point =>
        largerCell.getTree.hasNeighborInRange(point.vector, epsilon)
      }
    } else {
      smallerSet.exists { point =>
        largerCell.getTree.hasNeighborInRange(point.vector, epsilon)
      }
    }

  }

}
