package cluster

import components.union_find.DisjointSets
import data.Point
import data.Point.Embedding
import spatial.index._
import utils.Distances.euclideanDistanceSquared

import java.util.concurrent.atomic.AtomicInteger
import scala.collection.concurrent.TrieMap
import scala.collection.mutable
import scala.collection.parallel.CollectionConverters._
import scala.collection.parallel.mutable.{ParArray, ParTrieMap}
import scala.util.control.Breaks.{break, breakable}

class ParDBSCANGrid(
    val epsilon: Float,
    val minPts: Int,
    val connectivityCheck: ConnectivityCheck = new ConnectivityCheck(true)
) extends BaseDBSCAN {

  private val epsilonSquared = epsilon * epsilon

  private val corePoints = TrieMap.empty[Int, mutable.Map[Point, Boolean]]

  private val borderPoints = TrieMap.empty[Int, mutable.Map[Point, Boolean]]

  private var numClusters: Int = _

  private var clusters = mutable.Map.empty[Int, Int]

  private var coreFlags = mutable.Map.empty[Int, Boolean]

  override def fit(points: Array[Point]): (Array[Int], Array[Point]) = {
    if (points.isEmpty) return (Array.emptyIntArray, points)

    val pPoints = points.par
    val grid    = makeCells(pPoints)

    grid.cellsMap.values.par
      .filter(_.numPoints < minPts)
      .foreach(_.buildTree())

    val (coreCells, flags) = markCore(grid)
    coreFlags = flags
    clusters = clusterCore(grid, coreCells, coreFlags)
    clusterBorder(grid, coreFlags, clusters)

    (pPoints.map(p => clusters.getOrElse(p.id.toInt, -1)).toArray, points)
  }

  private def clusterBorder(
      grid: ParGrid,
      coreFlags: mutable.Map[Int, Boolean],
      clusters: mutable.Map[Int, Int]
  ): Unit = {
    for ((_, cell) <- grid.cellsMap.par.filter(_._2.numPoints < minPts)) {
      for ((pointId, point) <- cell.points.par.filter(p => !coreFlags.contains(p._1.toInt))) {
        breakable {
          val selfAndNeighbors = grid.queryEpsNeighborCells(cell.id) :+ cell
          for (candidate <- selfAndNeighbors.par) {
            for ((corePointId, corePoint) <- candidate.points.par.filter(p => coreFlags.contains(p._1.toInt))) {
              val distance = euclideanDistanceSquared(point.vector, corePoint.vector)
              if (distance <= epsilonSquared) {
                val clusterId = clusters(corePointId.toInt)
                if (!clusters.contains(pointId.toInt)) {
                  clusters.put(pointId.toInt, clusterId)
                }
                borderPoints.getOrElseUpdate(clusterId, mutable.Map[Point, Boolean]()).put(point, true)
                break
              }
            }
          }
        }
      }
    }
  }

  private def clusterCore(
      grid: ParGrid,
      coreCells: Array[ParGridCell],
      coreFlags: mutable.Map[Int, Boolean]
  ): TrieMap[Int, Int] = {
    // Dense index mapping for union-find
    val indexed = coreCells
    val idMap   = indexed.zipWithIndex.map { case (c, idx) => c.id -> idx }.toMap
    val uf      = new DisjointSets(indexed.length)

    // Parallel unions
    for (cell <- indexed.par) {
      val cellIdx = idMap(cell.id)
      for (neighbor <- grid.queryEpsNeighborCells(cell.id).filter(_.isCore).par) {
        val neighIdxOpt = idMap.get(neighbor.id)
        neighIdxOpt.foreach { neighIdx =>
          if (cellIdx > neighIdx && uf.find(cellIdx) != uf.find(neighIdx)) {
            if (connectivityCheck.isConnected(cell, neighbor, coreFlags, epsilon)) {
              uf.unite(cellIdx, neighIdx)
            }
          }
        }
      }
    }

    // Sequential labeling for stability
    val clusters        = TrieMap.empty[Int, Int]
    val rootToClusterId = mutable.HashMap.empty[Int, Int]
    var nextClusterId   = 0

    indexed.par.foreach { cell =>
      val rootIdx = uf.find(idMap(cell.id))
      val clusterId = rootToClusterId.getOrElseUpdate(
        rootIdx, {
          val id = nextClusterId
          nextClusterId += 1
          id
        }
      )

      // Assign core points of this cell
      cell.points.par.foreach { case (pid, point) =>
        if (coreFlags.getOrElse(pid.toInt, false)) {
          corePoints
            .getOrElseUpdate(clusterId, new TrieMap[Point, Boolean]())
            .put(point, true)
          clusters.put(pid.toInt, clusterId)
        }
      }
    }

    numClusters = rootToClusterId.size
    clusters
  }

  private def makeCells(points: ParArray[Point]): ParGrid = {
    val dim = points.head.vector.length
    new ParGrid(points, epsilon, dim)
  }

  private def markCore(grid: ParGrid): (Array[ParGridCell], TrieMap[Int, Boolean]) = {
    val flags = TrieMap.empty[Int, Boolean]
    val cells = TrieMap.empty[ParGridCell, Boolean]

    for ((_, cell) <- grid.cellsMap.par) {
      if (cell.numPoints >= minPts) {
        cell.setIsCore(true)
        cells.put(cell, true)
        cell.points.foreach { case (pid, _) => flags.put(pid.toInt, true) }
      } else {
        val neighborCells = grid.queryEpsNeighborCells(cell.id) :+ cell
        for ((pid, point) <- cell.points.par) {
          breakable {
            var count = 0
            for (neighbor <- neighborCells) {
              count += neighbor.rangeCount(point.vector, grid.epsilon)
              if (count >= minPts) {
                flags.put(point.id.toInt, true)
                if (!cell.isCore) {
                  cell.setIsCore(true)
                  cells.put(cell, true)
                }
                break()
              }
            }
          }
        }
      }
    }
    (cells.keys.toArray, flags)
  }

  override def getNumClusters: Int = numClusters

  override def getCorePoints: Array[Array[Point]] = flattenMap(corePoints)

  override def getBorderPoints: Array[Array[Point]] = flattenMap(borderPoints)

  private def flattenMap(map: TrieMap[Int, mutable.Map[Point, Boolean]]) =
    map.toSeq.sortBy(_._1).map { case (_, m) => m.keys.toArray }.toArray

}

class ParGrid(val points: ParArray[Point], val epsilon: Float, val dim: Int) {

  val cellsMap = TrieMap[Vector[Int], ParGridCell]()

  val cellIdToKey = TrieMap[Long, Vector[Int]]()

  private val cellSize: Float = epsilon / math.sqrt(dim).toFloat

  private val searchRadius: Float = math.sqrt(dim + 3).toFloat * 1.0000001f * epsilon

  private val minRealCords = Array.fill(dim)(0f)

  private val dimRange = (0 until dim).toArray

  val centerTree: KDTree = initializeGrid()

  private val neighborCache = mutable.Map.empty[Long, Vector[ParGridCell]]

  def queryEpsNeighborCells(cellIdx: Long): Vector[ParGridCell] = {
    if (neighborCache.contains(cellIdx)) {
      return neighborCache(cellIdx)
    }
    // Use synchronized block for thread-safe caching
    neighborCache.synchronized {
      neighborCache.getOrElseUpdate(
        cellIdx, {
          val cellCoords = cellIdToKey(cellIdx)
          val cell       = cellsMap(cellCoords)
          val neighbors  = centerTree.rangeQuery(cell.center, searchRadius).filter(_.id != cell.id)
          neighbors.toVector.map { neighbor =>
            val neighborCellCoords = cellIdToKey(neighbor.id)
            cellsMap(neighborCellCoords)
          }
        }
      )
    }
  }

  private def initializeGrid(): KDTree = {
    if (points.isEmpty) {
      throw new IllegalArgumentException("Points collection must not be empty")
    }

    val cellId = new AtomicInteger(-1)
    for (point <- points.par) {
      val cellCoords = getIntegerGridCoords(point)
      val cell = cellsMap.getOrElseUpdate(
        cellCoords, {
          val id      = cellId.incrementAndGet()
          val buf     = new ParTrieMap[Long, Point]()
          val newCell = ParGridCell(id, cellCoords, cellSize, this, searchRadius, buf)
          cellIdToKey.put(id, cellCoords)
          newCell
        }
      )

      cell.points.addOne(point.id, point)
    }
    val cellCenters = cellsMap.par.map { case (_, cell) =>
      Point(cell.center, cell.id)
    }
    new KDTree(cellCenters.toArray).build()
  }

  private def getIntegerGridCoords(point: Point) = {
    dimRange.map { d =>
      math.floor((point.vector(d) - minRealCords(d)) / cellSize).toInt
    }.toVector
  }

  private def hashGridCoords(gridCoords: Array[Int]): Int =
    gridCoords.zipWithIndex.map { case (coord, idx) => coord * (2 ^ idx) }.sum

}

case class ParGridCell(
    id: Int,
    gridCoords: Seq[Int],
    length: Float,
    grid: ParGrid,
    searchRadius: Float,
    points: ParTrieMap[Long, Point] = ParTrieMap[Long, Point]()
) extends Cell {

  val center: Embedding = gridCoords.map(c => (c * length + 0.5 * length).toFloat).toArray // Center of the cell in integer coordinates

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

  def numPoints: Int = points.size

  def setIsCore(bool: Boolean): Unit =
    isCore = bool

  def rangeQuery(queryPoint: Embedding, searchRadius: Float): Array[Point] =
    tree.rangeQuery(Point(queryPoint, -1L), searchRadius)

  def rangeCount(anchorPoint: Embedding, range: Float): Int =
    // Use the QuadTree index structure for efficient range counting
    // This leverages the spatial partitioning instead of brute-force search
    tree.rangeQuery(Point(anchorPoint, -1L), range).length

  def buildTree(): QuadTree =
    // implicit val ec: scala.concurrent.ExecutionContext = scala.concurrent.ExecutionContext.global
    new QuadTree(points.seq.values.toArray).build()

  override def getPoints: Array[Point] = points.values.seq.toArray

  override def getId: Int = id

  override def getTree: QuadTree = tree

}
