package cluster

import components.union_find.StaticUnionFind
import data.Point
import data.Point.Embedding
import io.{CSVWriter, OpenCSVReader}
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
    // Clear maps for consecutive runs
    corePointsMap.clear()
    borderPointsMap.clear()

    val grid = makeCells(points)
    if (parallelism) {
      parallelOps.warmUpSparseCellTrees(grid)
    }
    val (coreCells, coreFlags) = markCore(grid)
    val clusters               = clusterCore(grid, coreCells, coreFlags)
    clusterBorder(grid, coreFlags, clusters)

    val labels = points.map(p => clusters.getOrElse(p.id.toInt, -1))
    (labels, points)
  }

  /** Identifies border points and assigns them to a cluster. A border point is a non-core point that is within the
   * epsilon radius of a core point.
   */
  @inline private def clusterBorder(
      grid: Grid,
      coreFlags: mutable.Map[Int, Boolean],
      clusters: mutable.Map[Int, Int]
  ): Unit = {
    if (parallelism) {
      parallelOps.clusterBorder(grid, coreFlags, clusters, borderPointsMap)
      return
    }
    // Iterate over ALL cells, not just those with less than minPts.
    // A non-core point (potential border point) can exist in any cell.
    for (cell <- grid.cellsMap.values.filter(_.numPoints < minPts)) {
      // Find all points in the cell that were NOT marked as core points.
      val potentialBorderPoints = cell.points.filter(p => !coreFlags.getOrElse(p.id.toInt, false))
      val cellsToSearch         = Vector(cell) ++ grid.queryEpsNeighborCells(cell.id)
      val coreClusterLookup = clusters.toMap

      for (point <- potentialBorderPoints) {
        val clusterIds = collectNeighborClusterIds(point, cellsToSearch, coreFlags, coreClusterLookup)
        if (clusterIds.nonEmpty) {
          // Keep a deterministic primary label for the legacy single-label output.
          val primaryClusterId = clusterIds.min
          clusters(point.id.toInt) = primaryClusterId
          borderPointsMap.getOrElseUpdate(primaryClusterId, mutable.Set.empty) += point
        }
      }
    }
  }

  @inline private[cluster] def collectNeighborClusterIds(
                                                          point: Point,
                                                          cellsToSearch: Vector[GridCell],
                                                          coreFlags: mutable.Map[Int, Boolean],
                                                          coreClusterLookup: collection.Map[Int, Int]
                                                        ): Set[Int] = {
    cellsToSearch.iterator
      .flatMap(_.points.iterator)
      .filter(coreNeighbor => coreFlags.getOrElse(coreNeighbor.id.toInt, false))
      .filter(coreNeighbor => euclideanDistanceSquared(point.vector, coreNeighbor.vector) <= epsilonSquared)
      .flatMap(coreNeighbor => coreClusterLookup.get(coreNeighbor.id.toInt))
      .toSet
  }

  @inline private def makeCells(points: Array[Point]): Grid = {
    val dim = points.head.vector.length
    new Grid(points, epsilon, dim)
  }

  @inline private def markCore(grid: Grid): (Array[GridCell], mutable.Map[Int, Boolean]) = {
    if (parallelism) {
      return parallelOps.markCore(grid)
    }

    val coreFlags = mutable.Map.empty[Int, Boolean]
    val coreCells = mutable.Set[GridCell]()

    for (cell <- grid.cellsMap.values) {
      // Optimization: If a cell has >= minPts, assume all points in it are core.
      // This is a primary source of difference with classic DBSCAN.
      if (cell.numPoints >= minPts) {
        cell.setIsCore(true)
        coreCells += cell
        for (point <- cell.points)
          coreFlags(point.id.toInt) = true
      } else {
        // For less dense cells, check each point individually.
        val neighborCells = grid.queryEpsNeighborCells(cell.id)
        for (point <- cell.points) {
          breakable {
            var count = cell.rangeCount(point.vector, grid.epsilon)
            for (neighbor <- neighborCells) {
              count += neighbor.rangeCount(point.vector, grid.epsilon)
              if (count >= minPts) {
                coreFlags(point.id.toInt) = true
                // The cell containing a core point is considered a core cell for clustering.
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
      coreFlags: mutable.Map[Int, Boolean]
  ): mutable.Map[Int, Int] = {
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

    val clusters        = mutable.Map.empty[Int, Int]
    var clusterID       = 0
    val rootToClusterID = mutable.Map.empty[GridCell, Int]

    for (cell <- sortedCoreCells) {
      val root = uf.find(cell)
      if (!rootToClusterID.contains(root)) {
        clusterID += 1
        rootToClusterID(root) = clusterID
      }
      val currentClusterId = rootToClusterID(root)
      for (point <- cell.points.filter(p => coreFlags.getOrElse(p.id.toInt, false))) {
        clusters(point.id.toInt) = currentClusterId
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

  // Lazy initialization of the tree is important, otherwise the tree is incomplete
  var isCore: Boolean = false

  // Use thread-safe lazy initialization with synchronized block
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
    // implicit val ec: scala.concurrent.ExecutionContext = scala.concurrent.ExecutionContext.global
    val indexTree = new QuadTree(points).build()
    indexTree
  }

  def rangeQuery(anchorPoint: Embedding, range: Float): Array[Point] =
    tree.rangeQuery(anchorPoint, range)

  def rangeCount(anchorPoint: Embedding, range: Float): Int =
    tree.rangeCount(anchorPoint, range)

  override def getId: Int = id

  override def getTree: QuadTree = tree

}

class Grid(var points: Array[Point], val epsilon: Float, val dim: Int) {

  val cellsMap: mutable.Map[Vector[Int], GridCell] = mutable.Map.empty

  val cellIdToKey: mutable.Map[Long, Vector[Int]] = mutable.Map.empty

  // Ensure dependencies are initialized before building the center tree
  private val cellSize: Float = epsilon / math.sqrt(dim).toFloat

  // See https://github.com/wangyiqiu/dbscan-python/blob/42e1e719c2aa363c8991a0ada8e52a32b8702b15/include/dbscan/grid.h#L169
  // analytically, the search radius should be 2 * epsilon, but the reference implementation uses this value
  // cellSize * math.sqrt(dim + 3).toFloat * 1.0000001f
  private val searchRadius: Float = math.sqrt(dim).toFloat * cellSize + epsilon

  private val minRealCoords: Embedding = Array.fill(dim)(0f)

  private val gridOrigin = minRealCoords.map(c => c - 0.5f * cellSize)

  private val dimRange = (0 until dim).toArray

  var centerTree: KDTree = initializeGrid()

  private val neighborCache = mutable.Map.empty[Int, Vector[GridCell]]

  @inline def queryEpsNeighborCells(cellIdx: Int): Vector[GridCell] = {
    neighborCache.synchronized {
      neighborCache.getOrElseUpdate(
        cellIdx, {
          val cellCoords = cellIdToKey(cellIdx)
          val cell = cellsMap(cellCoords)

          val queryResult = centerTree.rangeQuery(cell.center, searchRadius)
          if (queryResult.isEmpty) {
            Vector.empty
          } else {
            val neighbors = queryResult.filter(_.id != cell.id)
            neighbors.toVector.map { neighbor =>
              val neighborCellCoords = cellIdToKey(neighbor.id)
              cellsMap(neighborCellCoords)
            }
          }
        }
      )
    }
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
    // Find the maximum existing cell ID
    val maxCellId = if (cellIdToKey.isEmpty) -1 else cellIdToKey.keys.max.toInt
    var nextCellId = maxCellId

    for ((coords, cell) <- other.cellsMap) {
      if (this.cellsMap.contains(coords)) {
        // Merge points into existing cell
        this.cellsMap(coords).points ++= cell.points
      } else {
        // Add new cell with a new ID
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
    // Rebuild the center tree with all cells. Avoid forcing every cell's QuadTree build here;
    // they remain lazily initialized upon first range query / count to reduce upfront cost.
    val cellCenters = cellsMap.values.map(cell => Point(cell.center, cell.id)).toArray
    this.centerTree = new KDTree(cellCenters).build()
  }

}

object DBSCANGrid {

  def main(args: Array[String]): Unit = {
    val data = new OpenCSVReader("data/densired_4_eps0.015_minPts5.csv").read
    val epsilon = 0.015f
    val minPts = 5
    val pointsWithIds = data.zipWithIndex.map { case (point, id) => Point(point, id.toLong) }
    val dbscan = new DBSCANGrid(epsilon, minPts)
    val (labels, points) = dbscan.fit(pointsWithIds)
    new CSVWriter("data/densired_4_eps0.015_minPts5_results.csv").write(points, labels)
  }

}
