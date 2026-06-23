package cluster

import components.union_find.LockFreeUnionFind
import data.Point
import utils.Pool

import scala.collection.mutable
import scala.collection.parallel.CollectionConverters._

final class DBSCANGridParallelOps(
    epsilon: Float,
    minPts: Int,
    parallelBucketSize: Int,
    connectivityCheck: ConnectivityCheck,
    collectNeighborClusterIds: (
        Point,
        Vector[GridCell],
          mutable.Map[Long, Boolean],
          collection.Map[Long, Int]
    ) => Set[Int]
) {

  private val taskSupport = Pool.taskSupport

  @inline def warmUpSparseCellTrees(grid: Grid): Unit = {
    val parColl = grid.cellsMap.values.filter(_.numPoints < minPts).toVector.par
    parColl.tasksupport = taskSupport
    parColl.foreach(_.getTree)
  }

  @inline def markCore(grid: Grid): (Array[GridCell], mutable.Map[Long, Boolean]) = {
    val parCells = grid.cellsMap.values.toVector.par
    parCells.tasksupport = taskSupport
    val evaluatedCells = parCells.map { cell =>
      if (cell.numPoints >= minPts) {
        val pointIds = cell.points.map(_.id).toVector
        (cell, true, pointIds)
      } else {
        val neighborCells = grid.queryEpsNeighborCells(cell.id)
        val corePointIds = cell.points.flatMap { point =>
          var count  = cell.rangeCount(point.vector, grid.epsilon)
          var isCore = false
          val it     = neighborCells.iterator
          while (!isCore && it.hasNext) {
            count += it.next().rangeCount(point.vector, grid.epsilon)
            if (count >= minPts) {
              isCore = true
            }
          }
          if (isCore) Some(point.id) else None
        }.toVector
        (cell, corePointIds.nonEmpty, corePointIds)
      }
    }.toVector

    val coreFlags = mutable.Map.empty[Long, Boolean]
    val coreCells = mutable.Set[GridCell]()
    for ((cell, isCoreCell, corePointIds) <- evaluatedCells) {
      if (isCoreCell) {
        cell.setIsCore(true)
        coreCells += cell
      }
      corePointIds.foreach(pid => coreFlags(pid) = true)
    }

    (coreCells.toArray, coreFlags)
  }

  @inline def clusterCore(
      grid: Grid,
      coreCells: Array[GridCell],
      coreFlags: mutable.Map[Long, Boolean],
      corePointsMap: mutable.Map[Int, mutable.Set[Point]]
                         ): (mutable.Map[Long, Int], Int) = {
    val uf              = new LockFreeUnionFind[GridCell](coreCells.toSet)
    val sortedCoreCells = coreCells.sortBy(_.numPoints)(Ordering[Int].reverse).toVector

    for (batch <- sortedCoreCells.grouped(parallelBucketSize)) {
      val parBatch = batch.par
      parBatch.tasksupport = taskSupport
      parBatch.foreach { cell =>
        grid
          .queryEpsNeighborCells(cell.id)
          .filter(nc => nc.isCore && cell.id > nc.id)
          .foreach { neighbor =>
            val rootA = uf.find(cell)
            val rootB = uf.find(neighbor)
            if (rootA != rootB && connectivityCheck.isConnected(cell, neighbor, coreFlags, epsilon)) {
              uf.union(rootA, rootB)
            }
          }
      }
    }

    val clusters = mutable.Map.empty[Long, Int]
    var clusterID       = 0
    val rootToClusterID = mutable.Map.empty[GridCell, Int]

    for (cell <- sortedCoreCells) {
      val root = uf.find(cell)
      if (!rootToClusterID.contains(root)) {
        clusterID += 1
        rootToClusterID(root) = clusterID
      }
      val currentClusterId = rootToClusterID(root)
      for (point <- cell.points.filter(p => coreFlags.getOrElse(p.id, false))) {
        clusters(point.id) = currentClusterId
        corePointsMap.getOrElseUpdate(currentClusterId, mutable.Set.empty) += point
      }
    }

    val numClusters = sortedCoreCells.iterator.map(uf.find).toSet.size
    (clusters, numClusters)
  }

  @inline def clusterBorder(
      grid: Grid,
      coreFlags: mutable.Map[Long, Boolean],
      clusters: mutable.Map[Long, Int],
      borderPointsMap: mutable.Map[Int, mutable.Set[Point]]
  ): Unit = {
    val borderCells       = grid.cellsMap.values.filter(_.numPoints < minPts).toVector
    val coreClusterLookup = clusters.toMap

    val parBorder = borderCells.par
    parBorder.tasksupport = taskSupport
    val assignments = parBorder.flatMap { cell =>
      val potentialBorderPoints = cell.points.filter(p => !coreFlags.getOrElse(p.id, false))
      val cellsToSearch         = Vector(cell) ++ grid.queryEpsNeighborCells(cell.id)

      val parPoints = potentialBorderPoints.toVector.par
      parPoints.tasksupport = taskSupport
      parPoints.flatMap { point =>
        val clusterIds = collectNeighborClusterIds(point, cellsToSearch, coreFlags, coreClusterLookup)
        if (clusterIds.nonEmpty) Some((point, clusterIds)) else None
      }.seq
    }.toVector

    for ((point, clusterIds) <- assignments) {
      if (!clusters.contains(point.id)) {
        val primaryClusterId = clusterIds.min
        clusters(point.id) = primaryClusterId
        borderPointsMap.getOrElseUpdate(primaryClusterId, mutable.Set.empty) += point
      }
    }
  }

}
