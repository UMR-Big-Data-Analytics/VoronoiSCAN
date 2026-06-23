package cluster

import data.Point
import utils.Distances.euclideanDistanceSquared
import utils.Pool

import scala.collection.parallel.CollectionConverters.{ArrayIsParallelizable, ImmutableIterableIsParallelizable}

sealed class ConnectivityCheck(parallel: Boolean, blockSize: Int = 64, bruteForceFilter: Boolean = false) {

  def isConnected(
                   g: Cell,
                   h: Cell,
                   coreFlags: collection.Map[Long, Boolean],
                   epsilon: Float
                 ): Boolean = {

    val epsilonSq = epsilon.toDouble * epsilon.toDouble

    val corePointsG = g.getPoints.filter(p => coreFlags.getOrElse(p.id, false))
    val corePointsH = h.getPoints.filter(p => coreFlags.getOrElse(p.id, false))

    if (corePointsG.isEmpty || corePointsH.isEmpty) {
      return false
    }

    val filteredG = filterNearby(corePointsG, h, epsilon)
    val filteredH = filterNearby(corePointsH, g, epsilon)

    if (filteredG.isEmpty || filteredH.isEmpty) {
      return false
    }

    val blocksG = filteredG.grouped(blockSize).toArray
    val blocksH = filteredH.grouped(blockSize).toArray

    if (parallel) {
      bcpBlockParallel(blocksG, blocksH, epsilonSq)
    } else {
      bcpBlockSequential(blocksG, blocksH, epsilonSq)
    }
  }

  private def filterNearby(points: Array[Point], otherCell: Cell, epsilon: Float): Array[Point] = {
    if (bruteForceFilter) {
      val epsilonSq = epsilon.toDouble * epsilon.toDouble
      val otherPoints = otherCell.getPoints
      if (parallel) {
        val parPoints = points.par
        parPoints.tasksupport = Pool.taskSupport
        parPoints.filter(p => hasNeighborBrute(p.vector, otherPoints, epsilonSq)).toArray
      } else {
        points.filter(p => hasNeighborBrute(p.vector, otherPoints, epsilonSq))
      }
    } else if (parallel) {
      val parPoints = points.par
      parPoints.tasksupport = Pool.taskSupport
      parPoints.filter { p =>
        otherCell.getTree.hasNeighborInRange(p.vector, epsilon)
      }.toArray
    } else {
      points.filter { p =>
        otherCell.getTree.hasNeighborInRange(p.vector, epsilon)
      }
    }
  }

  private def hasNeighborBrute(v: Array[Float], others: Array[Point], epsilonSq: Double): Boolean = {
    var i = 0
    while (i < others.length) {
      if (euclideanDistanceSquared(v, others(i).vector) <= epsilonSq) return true
      i += 1
    }
    false
  }

  @inline private def pairDistSq(a: Array[Float], b: Array[Float]): Double =
    if (bruteForceFilter) euclideanDistanceSquared(a, b) else euclideanDistanceSquared(a, b)

  private def bcpBlockSequential(
                                  blocksG: Array[Array[Point]],
                                  blocksH: Array[Array[Point]],
                                  epsilonSq: Double
                                ): Boolean = {
    for (blockG <- blocksG) {
      for (blockH <- blocksH) {
        if (blockPairHasConnection(blockG, blockH, epsilonSq)) {
          return true
        }
      }
    }
    false
  }

  private def bcpBlockParallel(
                                blocksG: Array[Array[Point]],
                                blocksH: Array[Array[Point]],
                                epsilonSq: Double
                              ): Boolean = {
    val blockPairs = for {
      bg <- blocksG
      bh <- blocksH
    } yield (bg, bh)

    for ((blockG, blockH) <- blockPairs) {
      if (blockPairHasConnectionParallel(blockG, blockH, epsilonSq)) {
        return true
      }
    }
    false
  }

  private def blockPairHasConnection(
                                      blockG: Array[Point],
                                      blockH: Array[Point],
                                      epsilonSq: Double
                                    ): Boolean = {
    for (pg <- blockG) {
      for (ph <- blockH) {
        if (pairDistSq(pg.vector, ph.vector) <= epsilonSq) {
          return true
        }
      }
    }
    false
  }

  private def blockPairHasConnectionParallel(
                                              blockG: Array[Point],
                                              blockH: Array[Point],
                                              epsilonSq: Double
                                            ): Boolean = {
    val sizeG = blockG.length
    val sizeH = blockH.length
    val distances = new Array[Double](sizeG * sizeH)

    val idxPar = (0 until sizeG * sizeH).par
    idxPar.tasksupport = Pool.taskSupport
    idxPar.foreach { idx =>
      val i = idx / sizeH
      val j = idx % sizeH
      distances(idx) = pairDistSq(blockG(i).vector, blockH(j).vector)
    }

    var k = 0
    while (k < distances.length) {
      if (distances(k) <= epsilonSq) return true
      k += 1
    }
    false
  }

}
