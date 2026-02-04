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
    val connectivityCheck: ConnectivityCheck = new ConnectivityCheck(false)
) extends BaseDBSCAN {

  private val epsilonSquared = epsilon * epsilon

  private var numClusters: Int = _

  private val corePointsMap: mutable.Map[Int, mutable.Set[Point]] = mutable.Map.empty

  private val borderPointsMap: mutable.Map[Int, mutable.Set[Point]] = mutable.Map.empty

  @inline override def fit(points: Array[Point]): (Array[Int], Array[Point]) = {
    if (points.isEmpty) {
      println("Warning: Empty points collection provided to DBSCANGrid.fit()")
      return (Array.empty[Int], Array.empty[Point])
    }
    // Clear maps for consecutive runs
    corePointsMap.clear()
    borderPointsMap.clear()

    val grid                   = makeCells(points)
    val (coreCells, coreFlags) = markCore(grid)
    val clusters               = clusterCore(grid, coreCells, coreFlags)
    clusterBorder(grid, coreFlags, clusters)

    val labels = points.map(p => clusters.getOrElse(p.id.toInt, -1))
    (labels, points)
  }

  @inline private def clusterBorder(
      grid: Grid,
      coreFlags: mutable.Map[Int, Boolean],
      clusters: mutable.Map[Int, Int]
  ): Unit = {
    for (cell <- grid.cellsMap.values.filter(_.numPoints < minPts)) {
      // Find all points in the cell that were NOT marked as core points.
      val potentialBorderPoints = cell.points.filter(p => !coreFlags.getOrElse(p.id.toInt, false))
      val cellsToSearch         = Vector(cell) ++ grid.queryEpsNeighborCells(cell.id)

      for (point <- potentialBorderPoints) {
        // Query the current cell and its neighbors.
        breakable {
          for (neighborCell <- cellsToSearch) {
            // Find a core point in a neighboring cell that is within epsilon distance.
            for (coreNeighbor <- neighborCell.points.filter(p => coreFlags.getOrElse(p.id.toInt, false))) {
              if (euclideanDistanceSquared(point.vector, coreNeighbor.vector) <= epsilonSquared) {
                clusters.get(coreNeighbor.id.toInt) match {
                  case Some(clusterId) =>
                    clusters(point.id.toInt) = clusterId
                    // Add the point to the border points map for that cluster.
                    borderPointsMap.getOrElseUpdate(clusterId, mutable.Set.empty) += point
                    // Once assigned, break to avoid re-assigning to another cluster.
                    break()
                  case None =>
                }
              }
            }
          }
        }
      }
    }
  }

  @inline private def makeCells(points: Array[Point]): Grid = {
    val dim = points.head.vector.length
    new Grid(points, epsilon, dim)
  }

  @inline private def markCore(grid: Grid): (Array[GridCell], mutable.Map[Int, Boolean]) = {
    val coreFlags = mutable.Map.empty[Int, Boolean]
    val coreCells = mutable.Set[GridCell]()

    for (cell <- grid.cellsMap.values) {
      // Check for dense cells
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
    val uf              = new StaticUnionFind[GridCell](coreCells.toSet)
    val sortedCoreCells = coreCells.sortBy(_.numPoints)(Ordering[Int].reverse)
    for (cell <- sortedCoreCells) {
      for (neighbor <- grid.queryEpsNeighborCells(cell.id).filter(_.isCore)) {
        if (cell.id > neighbor.id && uf.find(cell) != uf.find(neighbor)) {
          if (connectivityCheck.isConnected(cell, neighbor, coreFlags, epsilon)) {
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

  override def getId: Int = id

  override def getTree: QuadTree = tree

}

class Grid(var points: Array[Point], val epsilon: Float, val dim: Int) {

  val cellsMap: mutable.Map[Vector[Int], GridCell] = mutable.Map.empty

  val cellIdToKey: mutable.Map[Long, Vector[Int]] = mutable.Map.empty

  private val cellSize: Float = epsilon / math.sqrt(dim).toFloat

  private val searchRadius: Float = math.sqrt(dim).toFloat * cellSize + epsilon

  private val minRealCoords: Embedding = Array.fill(dim)(0f)

  private val gridOrigin = minRealCoords.map(c => c - 0.5f * cellSize)

  private val dimRange = (0 until dim).toArray

  var centerTree: KDTree = initializeGrid()

  private val neighborCache = mutable.Map.empty[Int, Vector[GridCell]]

  @inline def queryEpsNeighborCells(cellIdx: Int): Vector[GridCell] = {
    if (neighborCache.contains(cellIdx)) {
      return neighborCache(cellIdx)
    }
    val cellCoords = cellIdToKey(cellIdx)
    val cell       = cellsMap(cellCoords)

    val queryResult = centerTree.rangeQuery(cell.center, searchRadius)
    if (queryResult.isEmpty) {
      neighborCache(cellIdx) = Vector.empty
      return Vector.empty
    }
    val test = queryResult.filter(_ == null)
    if (test.length > 0) {
      println()
    }
    val neighbors = queryResult.filter(_.id != cell.id)
    val neighborCells = neighbors.toVector.map { neighbor =>
      val neighborCellCoords = cellIdToKey(neighbor.id)
      cellsMap(neighborCellCoords)
    }
    neighborCache(cellIdx) = neighborCells
    neighborCells
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

  def rebuildTree(): Unit = {
    val cellCenters = cellsMap.values.map(cell => Point(cell.center, cell.id)).toArray
    this.centerTree = new KDTree(cellCenters).build()
  }

}
