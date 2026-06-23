package cluster

import components.union_find.StaticUnionFind
import data.Point
import data.Point.Embedding
import spatial.index._
import utils.Distances.euclideanDistanceSquared

import scala.collection.mutable
import scala.language.postfixOps
import scala.util.control.Breaks.{break, breakable}

class DBSCANGrid(
    val epsilon: Float,
    val minPts: Int,
    val parallelism: Boolean = false,
    val connectivityCheck: ConnectivityCheck = null,
    val parallelBucketSize: Int = 128
) extends BaseDBSCAN {

  require(parallelBucketSize > 0, "parallelBucketSize must be > 0")

  private val epsilonSquared = epsilon * epsilon

  private val effectiveConnectivityCheck: ConnectivityCheck =
    if (connectivityCheck == null) new ConnectivityCheck(parallelism) else connectivityCheck

  private var numClusters: Int = _

  private val corePointsMap: mutable.Map[Int, mutable.Set[Point]] = mutable.Map.empty

  private val borderPointsMap: mutable.Map[Int, mutable.Set[Point]] = mutable.Map.empty

  private lazy val parallelOps = new DBSCANGridParallelOps(
    epsilon,
    minPts,
    parallelBucketSize,
    effectiveConnectivityCheck,
    (point, cellsToSearch, flags, clusterLookup) =>
      collectNeighborClusterIds(point, cellsToSearch, flags, clusterLookup)
  )

  @inline override def fit(points: Array[Point]): (Array[Int], Array[Point]) = {
    if (points.isEmpty) {
      println("Warning: Empty points collection provided to DBSCANGrid.fit()")
      return (Array.empty[Int], Array.empty[Point])
    }
    corePointsMap.clear()
    borderPointsMap.clear()

    val grid = makeCells(points)
    if (parallelism) {
      parallelOps.warmUpSparseCellTrees(grid)
    }
    val (coreCells, coreFlags) = markCore(grid)
    val clusters               = clusterCore(grid, coreCells, coreFlags)
    clusterBorder(grid, coreFlags, clusters)

    val labels = points.map(p => clusters.getOrElse(p.id, -1))
    (labels, points)
  }

  @inline private def clusterBorder(
      grid: Grid,
      coreFlags: mutable.Map[Long, Boolean],
      clusters: mutable.Map[Long, Int]
  ): Unit = {
    if (parallelism) {
      parallelOps.clusterBorder(grid, coreFlags, clusters, borderPointsMap)
      return
    }
    for (cell <- grid.cellsMap.values.filter(_.numPoints < minPts)) {
      val potentialBorderPoints = cell.points.filter(p => !coreFlags.getOrElse(p.id, false))
      val cellsToSearch         = Vector(cell) ++ grid.queryEpsNeighborCells(cell.id)

      for (point <- potentialBorderPoints) {
        val clusterIds = collectNeighborClusterIds(point, cellsToSearch, coreFlags, clusters)
        if (clusterIds.nonEmpty) {
          val primaryClusterId = clusterIds.min
          clusters(point.id) = primaryClusterId
          borderPointsMap.getOrElseUpdate(primaryClusterId, mutable.Set.empty) += point
        }
      }
    }
  }

  @inline private[cluster] def collectNeighborClusterIds(
                                                          point: Point,
                                                          cellsToSearch: Vector[GridCell],
                                                          coreFlags: mutable.Map[Long, Boolean],
                                                          coreClusterLookup: collection.Map[Long, Int]
                                                        ): Set[Int] = {
    cellsToSearch.iterator
      .flatMap(_.points.iterator)
      .filter(coreNeighbor => coreFlags.getOrElse(coreNeighbor.id, false))
      .filter(coreNeighbor => euclideanDistanceSquared(point.vector, coreNeighbor.vector) <= epsilonSquared)
      .flatMap(coreNeighbor => coreClusterLookup.get(coreNeighbor.id))
      .toSet
  }

  @inline private def makeCells(points: Array[Point]): Grid = {
    val dim = points.head.vector.length
    new Grid(points, epsilon, dim, parallelism)
  }

  @inline private def markCore(grid: Grid): (Array[GridCell], mutable.Map[Long, Boolean]) = {
    if (parallelism) {
      return parallelOps.markCore(grid)
    }

    val coreFlags = mutable.Map.empty[Long, Boolean]
    val coreCells = mutable.Set[GridCell]()

    for (cell <- grid.cellsMap.values) {
      if (cell.numPoints >= minPts) {
        cell.setIsCore(true)
        coreCells += cell
        for (point <- cell.points)
          coreFlags(point.id) = true
      } else {
        val neighborCells = grid.queryEpsNeighborCells(cell.id)
        for (point <- cell.points) {
          breakable {
            var count = cell.rangeCount(point.vector, grid.epsilon)
            for (neighbor <- neighborCells) {
              count += neighbor.rangeCount(point.vector, grid.epsilon)
              if (count >= minPts) {
                coreFlags(point.id) = true
                if (!cell.isCore) {
                  cell.setIsCore(true)
                  coreCells += cell
                }
                break()
              }
            }
          }
        }
      }
    }

    (coreCells.toArray, coreFlags)
  }

  @inline private def clusterCore(
      grid: Grid,
      coreCells: Array[GridCell],
      coreFlags: mutable.Map[Long, Boolean]
                                 ): mutable.Map[Long, Int] = {
    if (parallelism) {
      val (parallelClusters, parallelNumClusters) = parallelOps.clusterCore(grid, coreCells, coreFlags, corePointsMap)
      numClusters = parallelNumClusters
      return parallelClusters
    }

    val uf              = new StaticUnionFind[GridCell](coreCells.toSet)
    val sortedCoreCells = coreCells.sortBy(_.numPoints)(Ordering[Int].reverse)
    for (cell <- sortedCoreCells) {
      for (neighbor <- grid.queryEpsNeighborCells(cell.id).filter(_.isCore)) {
        if (cell.id > neighbor.id && uf.find(cell) != uf.find(neighbor)) {
          if (effectiveConnectivityCheck.isConnected(cell, neighbor, coreFlags, epsilon)) {
            uf.union(cell, neighbor)
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
    numClusters = uf.numComponents
    clusters
  }

  @inline override def getPointsArray(pointsMap: mutable.Map[Int, mutable.Set[Point]]): Array[Array[Point]] = {
    val maxClusterId = math.max(
      if (corePointsMap.isEmpty) 0 else corePointsMap.keys.max,
      if (borderPointsMap.isEmpty) 0 else borderPointsMap.keys.max
    )
    if (pointsMap.isEmpty) {
      return Array.fill(maxClusterId + 1)(Array.empty[Point])
    }

    val result = new Array[Array[Point]](maxClusterId + 1)
    for (clusterId <- 0 to maxClusterId)
      result(clusterId) = pointsMap.getOrElse(clusterId, mutable.Set()).toArray
    result
  }

  override def getNumClusters: Int = numClusters

  override def getCorePoints: Array[Array[Point]] = getPointsArray(corePointsMap)

  override def getBorderPoints: Array[Array[Point]] = getPointsArray(borderPointsMap)

}

case class GridCell(
    id: Int,
    gridCoords: Seq[Int],
    length: Float,
    searchRadius: Float,
    points: mutable.ArrayBuffer[Point] = mutable.ArrayBuffer.empty
) extends Cell {

  val center: Embedding = gridCoords.map(c => (c * length + 0.5 * length).toFloat).toArray // Center of the cell in integer coordinates

  var isCore: Boolean = false

  @volatile private var _tree: QuadTree = _

  private val treeLock = new Object()

  private def tree: QuadTree = {
    if (_tree == null) {
      treeLock.synchronized {
        if (_tree == null) {
          _tree = buildTree()
        }
      }
    }
    _tree
  }

  def numPoints: Int = points.length

  def setIsCore(isCore: Boolean): Unit =
    this.isCore = isCore

  override def getPoints: Array[Point] = points.toArray

  override def hashCode(): Int = id

  override def equals(obj: Any): Boolean = obj match {
    case that: GridCell => this.id == that.id
    case _              => false
  }

  private def buildTree() = {
    val indexTree = new QuadTree(points).build()
    indexTree
  }

  def rangeQuery(anchorPoint: Embedding, range: Float): Array[Point] =
    tree.rangeQuery(anchorPoint, range)

  def rangeCount(anchorPoint: Embedding, range: Float): Int =
    tree.rangeCount(anchorPoint, range)

  def rangeCountUpTo(anchorPoint: Embedding, range: Float, limit: Int): Int =
    tree.rangeCountUpTo(anchorPoint, range, limit)

  override def getId: Int = id

  override def getTree: QuadTree = tree

}

class Grid(var points: Array[Point], val epsilon: Float, val dim: Int, val parallel: Boolean) {

  val cellsMap: mutable.Map[Vector[Int], GridCell] = mutable.Map.empty

  val cellIdToKey: mutable.Map[Long, Vector[Int]] = mutable.Map.empty

  private val cellSize: Float = epsilon / math.sqrt(dim).toFloat

  // See https://github.com/wangyiqiu/dbscan-python/blob/42e1e719c2aa363c8991a0ada8e52a32b8702b15/include/dbscan/grid.h#L169
  // analytically, the search radius should be 2 * epsilon, but the reference implementation uses this value
  // cellSize * math.sqrt(dim + 3).toFloat * 1.0000001f
  private val searchRadius: Float = math.sqrt(dim).toFloat * cellSize + epsilon

  private val minRealCoords: Embedding = Array.fill(dim)(0f)

  private val gridOrigin = minRealCoords.map(c => c - 0.5f * cellSize)

  private val dimRange = (0 until dim).toArray

  var centerTree: KDTree = initializeGrid()

  private val neighborCache = if (parallel) {
    new java.util.concurrent.ConcurrentHashMap[Int, Vector[GridCell]]()
  } else {
    new java.util.HashMap[Int, Vector[GridCell]]()
  };

  @inline def queryEpsNeighborCells(cellIdx: Int): Vector[GridCell] = {
    neighborCache.computeIfAbsent(
      cellIdx,
      { idx =>
        val cellCoords = cellIdToKey(idx)
        val cell = cellsMap(cellCoords)

        val queryResult = centerTree.rangeQuery(cell.center, searchRadius)
        if (queryResult.isEmpty) {
          Vector.empty
        } else {
          val cellId = cell.id
          val builder = Vector.newBuilder[GridCell]
          var i = 0
          while (i < queryResult.length) {
            val neighbor = queryResult(i)
            if (neighbor.id != cellId) {
              builder += cellsMap(cellIdToKey(neighbor.id))
            }
            i += 1
          }
          builder.result()
        }
      }
    )
  }

  @inline def initializeGrid(): KDTree = {
    if (points.isEmpty) {
      throw new IllegalArgumentException("Points collection must not be empty")
    }

    var cellId = 0
    for (point <- points) {
      val cellCoords = getIntegerGridCoords(point)
      if (cellsMap.contains(cellCoords)) {
        cellsMap(cellCoords).points += point
      } else {
        val cell = GridCell(cellId, cellCoords, cellSize, searchRadius, mutable.ArrayBuffer(point))
        cellsMap(cellCoords) = cell
        cellIdToKey(cellId) = cellCoords
        cellId += 1
      }
    }

    val cellCenters = cellsMap.values.map(cell => Point(cell.center, cell.id))
    new KDTree(cellCenters.toArray).build()
  }

  @inline private def getIntegerGridCoords(point: Point) =
    dimRange.map { d =>
      math.floor((point.vector(d) - gridOrigin(d)) / cellSize).toInt
    }.toVector

  def merge(other: Grid): Grid = {
    val maxCellId = if (cellIdToKey.isEmpty) -1 else cellIdToKey.keys.max.toInt
    var nextCellId = maxCellId

    for ((coords, cell) <- other.cellsMap) {
      if (this.cellsMap.contains(coords)) {
        this.cellsMap(coords).points ++= cell.points
      } else {
        nextCellId += 1
        val newCell = GridCell(
          nextCellId, cell.gridCoords, cell.length, cell.searchRadius, cell.points
        )
        this.cellsMap(coords) = newCell
        this.cellIdToKey(nextCellId) = coords
      }
    }

    this.points ++= other.points

    this.neighborCache.clear()
    this
  }

  def rebuildTree(): Unit = {
    val cellCenters = cellsMap.values.map(cell => Point(cell.center, cell.id)).toArray
    this.centerTree = new KDTree(cellCenters).build()
  }

}
