package cluster

import scala.collection.parallel.CollectionConverters.ArrayIsParallelizable

sealed class ConnectivityCheck(parallel: Boolean) {

  def isConnected(
                   g: Cell,
                   h: Cell,
                   coreFlags: collection.Map[Int, Boolean],
                   epsilon: Float
                 ): Boolean = {

    // 1. Get only the core points from each cell. This is a critical first step.
    // Parallelization here is usually slower
    val corePointsG = g.getPoints.filter(p => coreFlags.getOrElse(p.id.toInt, false))
    val corePointsH = h.getPoints.filter(p => coreFlags.getOrElse(p.id.toInt, false))

    // 2. Early exit if one of the cells has no core points.
    if (corePointsG.isEmpty || corePointsH.isEmpty) {
      return false
    }

    // 3. Identify the smaller set to iterate over and the larger cell to query against.
    // This minimizes the number of expensive range queries.
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
