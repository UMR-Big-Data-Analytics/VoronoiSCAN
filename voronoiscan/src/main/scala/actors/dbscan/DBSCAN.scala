package actors.dbscan

import akka.actor.typed._
import akka.actor.typed.scaladsl.AskPattern.Askable
import akka.actor.typed.scaladsl.{ActorContext, Behaviors, Routers}
import cluster.{BaseDBSCAN, ConnectivityCheck, Grid, GridCell}
import components.union_find.StaticUnionFind
import data.Point
import utils.Distances.euclideanDistanceSquared

import scala.collection.mutable
import scala.language.postfixOps
import scala.util.control.Breaks.{break, breakable}

class DBSCANGridAkka[T](
    val epsilon: Float,
    val minPts: Int,
    val parentContext: Option[ActorContext[T]] = None
) extends BaseDBSCAN {

  private var numClusters: Int = _

  private var corePointsMap: mutable.Map[Int, mutable.Set[Point]] = mutable.Map.empty

  private var borderPointsMap: mutable.Map[Int, mutable.Set[Point]] = mutable.Map.empty

  private def computeWithNewActorSystem(points: Array[Point]) = {
    // Create an actor system to manage DBSCAN clustering
    import akka.actor.typed.ActorSystem
    import akka.util.Timeout

    import scala.concurrent.duration.DurationInt
    import scala.concurrent.{Await, Future}

    val system = ActorSystem(
      DBSCANController(points, epsilon, minPts),
      "DBSCANSystem"
    )

    implicit val timeout: Timeout     = 300.minutes
    implicit val scheduler: Scheduler = system.scheduler
    // Import the execution context for the ask pattern
    // import system.executionContext // removed unused import
    val futureLabels: Future[DBSCANControllerResponse] =
      system.ask(ref => StartDBSCAN(ref))

    val result: DBSCANControllerResponse = Await.result(futureLabels, timeout.duration)
    val (clusters, corePointsMap, borderPointMaps, numCluster) = result match {
      case DBSCANCompleted(c, core, border, n) => (c, core, border, n)
    }
    system.terminate()
    (clusters, corePointsMap, borderPointMaps, numCluster)
  }

  private def computeWithExistingContext(context: ActorContext[T], points: Array[Point]) = {
    // Use the existing actor system from the parent context
    import akka.actor.typed.scaladsl.AskPattern.Askable
    import akka.util.Timeout

    import scala.concurrent.duration.DurationInt
    import scala.concurrent.{Await, Future}

    implicit val timeout: Timeout     = 300.minutes
    implicit val scheduler: Scheduler = context.system.scheduler
    // removed unused implicit ec

    val dbscanController = context.spawn(
      DBSCANController(points, epsilon, minPts),
      s"DBSCANController-${System.currentTimeMillis()}"
    )

    val futureLabels: Future[DBSCANControllerResponse] =
      dbscanController.ask(ref => StartDBSCAN(ref))

    val result: DBSCANControllerResponse = Await.result(futureLabels, timeout.duration)
    result match {
      case DBSCANCompleted(c, core, border, n) => (c, core, border, n)
    }
  }

  override def fit(points: Array[Point]): (Array[Int], Array[Point]) = {
    val (clusters, corePointsMap, borderPointMaps, numCluster) = parentContext match {
      case Some(context) => computeWithExistingContext(context, points)
      case None          => computeWithNewActorSystem(points)
    }
    this.corePointsMap = corePointsMap
    this.borderPointsMap = borderPointMaps
    this.numClusters = numCluster

    val labels = points.map(p => clusters.getOrElse(p.id.toInt, -1))
    (labels, points)
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

sealed trait DBSCANControllerRequest

final case class StartDBSCAN(
    replyTo: ActorRef[DBSCANControllerResponse]
) extends DBSCANControllerRequest

final case class GridConstructed(grid: Grid) extends DBSCANControllerRequest

final case class MarkCore(grid: Grid) extends DBSCANControllerRequest

final case class ClusterCore(grid: Grid, coreCells: mutable.Set[GridCell], coreFlags: mutable.Map[Int, Boolean])
    extends DBSCANControllerRequest

final case class ClusterCoreResult(
    grid: Grid,
    coreFlags: mutable.Map[Int, Boolean],
    clusters: mutable.Map[Int, Int],
    corePointsMap: mutable.Map[Int, mutable.Set[Point]],
    numClusters: Int
) extends DBSCANControllerRequest

final case class ClusterBorder(grid: Grid, coreFlags: mutable.Map[Int, Boolean], clusters: mutable.Map[Int, Int])
    extends DBSCANControllerRequest

final case class ClusterBorderResult(
    clusters: mutable.Map[Int, Int],
    borderPointsMap: mutable.Map[Int, mutable.Set[Point]]
) extends DBSCANControllerRequest

sealed trait DBSCANControllerResponse

final case class DBSCANCompleted(
    clusters: mutable.Map[Int, Int],
    corePointsMap: mutable.Map[Int, mutable.Set[Point]],
    borderPointsMap: mutable.Map[Int, mutable.Set[Point]],
    numCluster: Int
) extends DBSCANControllerResponse

object DBSCANController {

  def apply(
      points: Array[Point],
      epsilon: Float,
      minPts: Int
  ): Behavior[DBSCANControllerRequest] =
    new DBSCANController(points, epsilon, minPts).behavior

}

private class DBSCANController(
    points: Array[Point],
    epsilon: Float,
    minPts: Int,
    numGridConstructors: Int = 10
) {

  private var tStart: Long = 0L

  private var tGridDone: Long = 0L

  private var tMarkCoreDone: Long = 0L

  private var tClusterCoreDone: Long = 0L

  private var tBorderDone: Long = 0L

  private var pointGrid: Option[Grid] = None

  private def behavior: Behavior[DBSCANControllerRequest] = Behaviors.setup { context =>
    val dim          = points.head.dim
    val maxPointId   = points.map(_.id.toInt).max
    val coreFlagsArr = Array.fill[Boolean](maxPointId + 1)(false)
    val clustersArr  = Array.fill[Int](maxPointId + 1)(-1)
    val coreFlagsMap = mutable.Map.empty[Int, Boolean]

    var ask: Option[ActorRef[DBSCANControllerResponse]] = None
    var numberOfClusters                                = -1

    val gridConstructors = (0 until numGridConstructors).map { i =>
      context.spawn(
        new GridConstructor(epsilon, dim).behavior,
        s"GridConstructor-$i-${System.currentTimeMillis()}"
      )
    }

    var numGridResponses                                    = 0
    val coreCells                                           = mutable.Set.empty[GridCell]
    var processedCoreCellsCount                             = 0
    var corePointsMap: mutable.Map[Int, mutable.Set[Point]] = mutable.Map.empty

    val availableProcessors  = Runtime.getRuntime.availableProcessors()
    val markCorePoolSize     = math.max(1, availableProcessors)
    val connectivityPoolSize = math.max(1, availableProcessors)

    val markCoreRouter = context.spawn(
      Routers.pool(poolSize = markCorePoolSize) {
        new MarkCoreActor(minPts).behavior
      },
      s"MarkCoreRouter-${System.currentTimeMillis()}"
    )

    Behaviors.receiveMessage {
      case StartDBSCAN(replyTo) =>
        tStart = System.nanoTime()
        ask = Some(replyTo)
        gridConstructors.zipWithIndex.foreach { case (ref, i) =>
          val partitionSize = points.length / numGridConstructors
          val startIdx      = i * partitionSize
          val endIdx        = if (i == numGridConstructors - 1) points.length else (i + 1) * partitionSize
          val partition     = points.slice(startIdx, endIdx)
          ref ! ConstructGrid(partition, context.self)
        }
        Behaviors.same
      case GridConstructed(grid) =>
        pointGrid match {
          case None =>
            pointGrid = Some(grid)
            numGridResponses += 1
            if (numGridResponses == numGridConstructors) {
              tGridDone = System.nanoTime()
              context.log.debug(f"Grid construction finished in ${(tGridDone - tStart) / 1e6}%.2f ms")
              pointGrid.get.rebuildTree()
              context.self ! MarkCore(pointGrid.get)
            }
            Behaviors.same
          case Some(existingGrid) =>
            pointGrid = Some(existingGrid.merge(grid))
            numGridResponses += 1
            if (numGridResponses == numGridConstructors) {
              tGridDone = System.nanoTime()
              context.log.debug(f"Grid construction finished in ${(tGridDone - tStart) / 1e6}%.2f ms")
              pointGrid.get.rebuildTree()
              context.self ! MarkCore(pointGrid.get)
            }
            Behaviors.same
        }
      case MarkCore(grid) =>
        val allCells   = grid.cellsMap.values.toSeq
        val totalCells = allCells.size
        val batchSize =
          if (totalCells > 5000) 128
          else if (totalCells > 1000) 64
          else if (totalCells > 200) 32
          else 16
        allCells.grouped(batchSize).foreach { batch =>
          DBSCANMetrics.totalMarkCoreMessages.addAndGet(batch.size)
          DBSCANMetrics.activeMarkCore.addAndGet(batch.size)
          markCoreRouter ! MarkCoreCellsBatch(grid, batch, context.self)
        }
        Behaviors.same
      case MarkCoreBatchResult(results) =>
        for ((cell, flags) <- results) {
          DBSCANMetrics.activeMarkCore.decrementAndGet()
          var anyAddedToCell = false
          for ((pid, isCoreFlag) <- flags if isCoreFlag) {
            coreFlagsArr(pid) = true; coreFlagsMap(pid) = true; anyAddedToCell = true
          }
          if (anyAddedToCell) cell.setIsCore(true)
          if (cell.isCore) coreCells += cell
          processedCoreCellsCount    += 1
        }
        val grid = pointGrid.get
        if (processedCoreCellsCount == grid.cellsMap.size) {
          tMarkCoreDone = System.nanoTime()
          context.log.debug(
            f"MarkCore (batched) finished in ${(tMarkCoreDone - tGridDone) / 1e6}%.2f ms (cumulative ${(tMarkCoreDone - tStart) / 1e6}%.2f ms) batches=${math.ceil(processedCoreCellsCount.toDouble / math.max(1, results.size))} -> ${DBSCANMetrics.snapshot}"
          )
          context.self ! ClusterCore(grid, coreCells, mutable.Map.empty)
        }
        Behaviors.same
      case ClusterCore(grid, cells, _) =>
        val batchRouter = context.spawn(
          Routers.pool(poolSize = connectivityPoolSize) {
            new ClusterCoreWorker(coreFlagsMap).behavior
          },
          s"ClusterCoreBatchRouter-${System.currentTimeMillis()}"
        )
        context.spawn(
          new ClusterCoreManager(grid, cells, coreFlagsArr, epsilon, context.self, batchRouter, clustersArr).behavior,
          "ClusterCoreManager-" + System.currentTimeMillis()
        )
        Behaviors.same
      case ClusterCoreResult(grid, _, _, corePoints, numClusters) =>
        corePointsMap = corePoints
        numberOfClusters = numClusters
        context.log.debug(s"DBSCAN clustering completed. Total clusters found: $numClusters. ${DBSCANMetrics.snapshot}")
        tClusterCoreDone = System.nanoTime()
        context.log.debug(
          f"ClusterCore finished in ${(tClusterCoreDone - tMarkCoreDone) / 1e6}%.2f ms (cumulative ${(tClusterCoreDone - tStart) / 1e6}%.2f ms)"
        )
        val clustersMap = mutable.Map.empty[Int, Int]
        var i           = 0
        while (i <= maxPointId) { val cid = clustersArr(i); if (cid != -1) clustersMap(i) = cid; i += 1 }
        context.self ! ClusterBorder(grid, mutable.Map.empty, clustersMap)
        Behaviors.same
      case ClusterBorder(grid, _, clusters) =>
        val borderManager = context.spawn(
          new ClusterBorderManager(minPts, grid, coreFlagsArr, clusters, context.self).behavior,
          "ClusterBorderManager-" + System.currentTimeMillis()
        )
        borderManager ! StartClusterBorderProcessing()
        Behaviors.same
      case ClusterBorderResult(clustersMap, borderPointsMap) =>
        tBorderDone = System.nanoTime()
        context.log.debug(
          f"Border assignment finished in ${(tBorderDone - tClusterCoreDone) / 1e6}%.2f ms (total ${(tBorderDone - tStart) / 1e6}%.2f ms) -> ${DBSCANMetrics.snapshot}"
        )
        ask.foreach(_ ! DBSCANCompleted(clustersMap, corePointsMap, borderPointsMap, numberOfClusters))
        Behaviors.same
    }
  }

}

sealed trait GridConstructorRequest

final case class ConstructGrid(
    points: Array[Point],
    replyTo: ActorRef[DBSCANControllerRequest]
) extends GridConstructorRequest

private class GridConstructor(
    epsilon: Float,
    dim: Int
) {

  def behavior: Behavior[GridConstructorRequest] = Behaviors.receive { (_, message) =>
    message match {
      case ConstructGrid(points, replyTo) =>
        val grid = new Grid(points, epsilon, dim)
        replyTo ! GridConstructed(grid)
        Behaviors.stopped
    }
  }

}

sealed trait MarkCoreRequest

final case class MarkCoreCells(
    grid: Grid,
    cell: GridCell,
    replyTo: ActorRef[DBSCANControllerRequest]
) extends MarkCoreRequest

final case class MarkCoreCellsBatch(grid: Grid, cells: Seq[GridCell], replyTo: ActorRef[DBSCANControllerRequest])
    extends MarkCoreRequest

final case class MarkCoreBatchResult(results: Seq[(GridCell, mutable.Map[Int, Boolean])])
    extends DBSCANControllerRequest

private class MarkCoreActor(minPts: Int) {

  def behavior: Behavior[MarkCoreRequest] = Behaviors.receive { (_, message) =>
    message match {
      case MarkCoreCellsBatch(grid, cells, replyTo) =>
        val batchResults = cells.map { cell =>
          val coreFlags = mutable.Map.empty[Int, Boolean]
          if (cell.numPoints >= minPts) {
            cell.setIsCore(true)
            cell.points.foreach(p => coreFlags(p.id.toInt) = true)
          } else {
            val neighborCells = grid.queryEpsNeighborCells(cell.id)
            for (point <- cell.points) {
              breakable {
                var count = cell.rangeCount(point.vector, grid.epsilon)
                for (neighbor <- neighborCells) {
                  count += neighbor.rangeCount(point.vector, grid.epsilon)
                  if (count >= minPts) {
                    coreFlags(point.id.toInt) = true
                    if (!cell.isCore) cell.setIsCore(true)
                    break()
                  }
                }
              }
            }
          }
          (cell, coreFlags)
        }
        replyTo ! MarkCoreBatchResult(batchResults)
        Behaviors.same
      case MarkCoreCells(grid, cell, replyTo) =>
        val coreFlags = mutable.Map.empty[Int, Boolean]
        if (cell.numPoints >= minPts) {
          cell.setIsCore(true)
          cell.points.foreach(p => coreFlags(p.id.toInt) = true)
        } else {
          val neighborCells = grid.queryEpsNeighborCells(cell.id)
          for (point <- cell.points) {
            breakable {
              var count = cell.rangeCount(point.vector, grid.epsilon)
              for (neighbor <- neighborCells) {
                count += neighbor.rangeCount(point.vector, grid.epsilon)
                if (count >= minPts) {
                  coreFlags(point.id.toInt) = true
                  if (!cell.isCore) cell.setIsCore(true)
                  break()
                }
              }
            }
          }
        }
        replyTo ! MarkCoreBatchResult(Seq((cell, coreFlags)))
        Behaviors.same
    }
  }

}

sealed trait ClusterCoreManagerRequest

final case class CoreCellsConnected(
    cellA: GridCell,
    cellB: GridCell,
    connected: Boolean
) extends ClusterCoreManagerRequest

private class ClusterCoreManager(
    grid: Grid,
    coreCells: mutable.Set[GridCell],
    coreFlagsArr: Array[Boolean],
    epsilon: Float,
    replyTo: ActorRef[DBSCANControllerRequest],
    clusterCoreRouter: ActorRef[ClusterCoreRequest],
    clustersArr: Array[Int]
) {

  private val uf = new StaticUnionFind[GridCell](coreCells)

  private val sortedCoreCells = coreCells.toArray.sortBy(_.numPoints)(Ordering[Int].reverse)

  def behavior: Behavior[ClusterCoreManagerRequest] = Behaviors.setup { context =>
    val rawPairs = for {
      cell     <- sortedCoreCells
      neighbor <- grid.queryEpsNeighborCells(cell.id).filter(c => c.isCore && cell.id > c.id)
      if cell.id > neighbor.id
    } yield (cell, neighbor)
    var tasks      = mutable.Queue.from(rawPairs)
    val totalPairs = tasks.size
    context.log.debug(s"ClusterCoreManager: total candidate pairs=$totalPairs")
    if (tasks.isEmpty) {
      context.log.debug("ClusterCoreManager: no connectivity tasks -> finalize immediately.")
      finialize()
      Behaviors.stopped
    } else {
      // Adaptive batch size for connectivity
      def batchSizeFor(remaining: Int): Int =
        if (remaining > 20000) 512
        else if (remaining > 5000) 256
        else if (remaining > 1000) 128
        else if (remaining > 200) 64
        else if (remaining > 50) 32
        else 16

      var inFlightBatches = 0
      var processedPairs  = 0

      def dispatchNextBatch(context: ActorContext[ClusterCoreManagerRequest]): Unit = {
        if (tasks.nonEmpty) {
          val size  = batchSizeFor(tasks.size)
          val batch = (0 until math.min(size, tasks.size)).map(_ => tasks.dequeue())
          inFlightBatches += 1
          clusterCoreRouter ! ClusterCoreCellsBatch(batch, epsilon, context.self)
          DBSCANMetrics.totalConnectivityMessages.addAndGet(batch.size) // count individual pairs
          DBSCANMetrics.activeCoreConnectivity.addAndGet(batch.size)
        }
      }

      val parallelism    = Runtime.getRuntime.availableProcessors()
      val initialBatches = math.max(1, parallelism)
      for (_ <- 0 until initialBatches if tasks.nonEmpty) dispatchNextBatch(context)

      Behaviors.receiveMessage { case ClusterCoreBatchResult(results) =>
        inFlightBatches -= 1
        DBSCANMetrics.activeCoreConnectivity.addAndGet(-results.size)
        // Apply unions for connected pairs
        results.foreach { case (a, b, connected) =>
          processedPairs += 1
          if (connected) uf.union(a, b)
        }
        // Filter remaining tasks whose components already unified
        if (processedPairs % 1000 == 0 || tasks.isEmpty) {
          tasks = tasks.filter { case (a, b) => uf.find(a) != uf.find(b) }
        }
        if (tasks.isEmpty && inFlightBatches == 0) {
          context.log.debug(s"ClusterCoreManager: processedPairs=$processedPairs connected=${results.count(_._3)}")
          finialize()
          Behaviors.stopped
        } else {
          // Maintain a pipeline of batches up to parallelism
          while (inFlightBatches < parallelism && tasks.nonEmpty) dispatchNextBatch(context)
          Behaviors.same
        }
      }
    }
  }

  private def finialize(): Unit = {
    var clusterID                                             = 0
    val rootToClusterID                                       = mutable.Map.empty[GridCell, Int]
    val corePointsLocal: mutable.Map[Int, mutable.Set[Point]] = mutable.Map.empty
    for (cell <- sortedCoreCells) {
      val root = uf.find(cell)
      if (!rootToClusterID.contains(root)) { clusterID += 1; rootToClusterID(root) = clusterID }
      val currentClusterId = rootToClusterID(root)
      for (point <- cell.points) {
        val pid = point.id.toInt
        if (coreFlagsArr(pid)) {
          clustersArr(pid) = currentClusterId
          corePointsLocal.getOrElseUpdate(currentClusterId, mutable.Set.empty) += point
        }
      }
    }
    val clustersMapDummy = mutable.Map.empty[Int, Int]
    replyTo ! ClusterCoreResult(grid, mutable.Map.empty, clustersMapDummy, corePointsLocal, uf.numComponents)
  }

}

sealed trait ClusterCoreRequest

final case class ClusterCoreCells(
    cellA: GridCell,
    cellB: GridCell,
    epsilon: Float,
    coreFlags: mutable.Map[Int, Boolean],
    replyTo: ActorRef[ClusterCoreManagerRequest]
) extends ClusterCoreRequest

final case class ClusterCoreCellsBatch(
    pairs: Seq[(GridCell, GridCell)],
    epsilon: Float,
    replyTo: ActorRef[ClusterCoreManagerRequest]
) extends ClusterCoreRequest

final case class ClusterCoreBatchResult(results: Seq[(GridCell, GridCell, Boolean)]) extends ClusterCoreManagerRequest

private class ClusterCoreWorker(coreFlagsMap: mutable.Map[Int, Boolean]) {

  def behavior: Behavior[ClusterCoreRequest] = Behaviors.receive { (_, message) =>
    message match {
      case ClusterCoreCellsBatch(pairs, epsilon, replyTo) =>
        val results = pairs.map { case (a, b) =>
          val connected = new ConnectivityCheck(false).isConnected(a, b, coreFlagsMap, epsilon)
          (a, b, connected)
        }
        replyTo ! ClusterCoreBatchResult(results)
        Behaviors.same
      case ClusterCoreCells(cellA, cellB, epsilon, coreFlags, replyTo) =>
        val connected = new ConnectivityCheck(false).isConnected(cellA, cellB, coreFlags, epsilon)
        replyTo ! CoreCellsConnected(cellA, cellB, connected)
        DBSCANMetrics.activeCoreConnectivity.decrementAndGet()
        Behaviors.same
    }
  }

}

sealed trait ClusterBorderManagerRequest

final case class StartClusterBorderProcessing() extends ClusterBorderManagerRequest

final case class ProcessedBorderPointsForCell(
    assignedClusters: mutable.Map[Int, Int],
    borderPointsMap: mutable.Map[Int, mutable.Set[Point]]
) extends ClusterBorderManagerRequest

private class ClusterBorderManager(
    minPts: Int,
    grid: Grid,
    coreFlagsArr: Array[Boolean],
    clusters: mutable.Map[Int, Int],
    replyTo: ActorRef[DBSCANControllerRequest]
) {

  def behavior: Behavior[ClusterBorderManagerRequest] = Behaviors.setup { context =>
    val pool = Routers.pool(poolSize = 16) {
      Behaviors
        .supervise(new ClusterBorderPointWorker(grid.epsilon * grid.epsilon, coreFlagsArr).behavior)
        .onFailure[Exception](SupervisorStrategy.restart)
    }
    val borderCellRouter = context.spawn(
      pool,
      s"ClusterBorderPointRouter-${System.currentTimeMillis()}"
    )
    val clustersCopy = mutable.Map[Int, Int]()
    clustersCopy ++= clusters
    val borderPointsMap         = mutable.Map.empty[Int, mutable.Set[Point]]
    var numRemainingBorderCells = 0
    Behaviors.receiveMessage {
      case StartClusterBorderProcessing() =>
        for (cell <- grid.cellsMap.values.filter(_.numPoints < minPts)) {
          numRemainingBorderCells += 1
          val worker =
            context.spawn(
              new ClusterBorderCellWorker(borderCellRouter, context.self, coreFlagsArr).behavior,
              s"ClusterBorderCellWorker-${cell.id}"
            )
          worker ! ProcessBorderCell(cell, grid, mutable.Map.empty[Int, Boolean], clusters)
        }
        if (numRemainingBorderCells == 0) {
          context.log.debug("ClusterBorderManager: no border cells -> finalize immediately.")
          replyTo ! ClusterBorderResult(clustersCopy, borderPointsMap)
          Behaviors.stopped
        } else Behaviors.same
      case ProcessedBorderPointsForCell(assignedClusters, borderPoints) =>
        numRemainingBorderCells -= 1
        clustersCopy           ++= assignedClusters
        for ((clusterId, points) <- borderPoints) {
          val borderSet = borderPointsMap.getOrElseUpdate(clusterId, mutable.Set.empty)
          borderSet ++= points
        }
        if (numRemainingBorderCells == 0) {
          replyTo ! ClusterBorderResult(clustersCopy, borderPointsMap)
          Behaviors.stopped
        } else
          Behaviors.same
    }

  }

}

sealed trait ClusterBorderCellRequest

final case class ProcessBorderCell(
    cell: GridCell,
    grid: Grid,
    coreFlags: mutable.Map[Int, Boolean],
    clusters: mutable.Map[Int, Int]
) extends ClusterBorderCellRequest

final case class ProcessedBorderPoints(
    assignedClusters: mutable.Map[Int, Int],
    borderPointsMap: mutable.Map[Int, mutable.Set[Point]]
) extends ClusterBorderCellRequest

private class ClusterBorderCellWorker(
    borderPointRouter: ActorRef[ClusterBorderPointRequest],
    replyTo: ActorRef[ClusterBorderManagerRequest],
    coreFlagsArr: Array[Boolean]
) {

  private val BATCH_SIZE = 100

  def behavior: Behavior[ClusterBorderCellRequest] = {
    Behaviors.setup { context =>
      var numRemainingResponses = 0
      val borderPointsMap       = mutable.Map.empty[Int, mutable.Set[Point]]
      val clustersMap           = mutable.Map.empty[Int, Int]
      Behaviors.receiveMessage[ClusterBorderCellRequest] {
        case ProcessBorderCell(cell, grid, _, clusters) =>
          val potentialBorderPoints = cell.points.filter(p => !coreFlagsArr(p.id.toInt))
          if (potentialBorderPoints.isEmpty) {
            replyTo ! ProcessedBorderPointsForCell(clustersMap, borderPointsMap)
            Behaviors.stopped
          } else {
            val cellsToSearch = Vector(cell) ++ grid.queryEpsNeighborCells(cell.id)
            val batches       = mutable.Queue.from(potentialBorderPoints.grouped(BATCH_SIZE))
            while (batches.nonEmpty) {
              val batch = batches.dequeue()
              numRemainingResponses += 1
              borderPointRouter ! ProcessBorderPoints(
                batch,
                cellsToSearch,
                mutable.Map.empty[Int, Boolean],
                clusters,
                context.self
              )
              DBSCANMetrics.totalBorderPointBatchMessages.incrementAndGet()
              DBSCANMetrics.activeBorderPointBatches.incrementAndGet()
            }
            Behaviors.same
          }

        case ProcessedBorderPoints(assignedClusters, borderPoints) =>
          clustersMap ++= assignedClusters
          for ((clusterId, points) <- borderPoints) {
            val borderSet = borderPointsMap.getOrElseUpdate(clusterId, mutable.Set.empty)
            borderSet ++= points
          }
          numRemainingResponses -= 1
          if (numRemainingResponses == 0) {
            replyTo ! ProcessedBorderPointsForCell(clustersMap, borderPointsMap)
            DBSCANMetrics.activeBorderPointBatches.decrementAndGet()
            Behaviors.stopped
          } else {
            Behaviors.same
          }
      }
    }
  }

}

sealed trait ClusterBorderPointRequest

final case class ProcessBorderPoints(
    points: Iterable[Point],
    cellsToSearch: Vector[GridCell],
    coreFlags: mutable.Map[Int, Boolean],
    clusters: mutable.Map[Int, Int],
    replyTo: ActorRef[ClusterBorderCellRequest]
) extends ClusterBorderPointRequest

private class ClusterBorderPointWorker(epsilonSquared: Float, coreFlagsArr: Array[Boolean]) {

  def behavior: Behavior[ClusterBorderPointRequest] = Behaviors.receive { (_, message) =>
    message match {
      case ProcessBorderPoints(points, cellsToSearch, _, clusters, replyTo: ActorRef[ClusterBorderCellRequest]) =>
        val assignedClusters = mutable.Map.empty[Int, Int]
        val borderPointsMap  = mutable.Map.empty[Int, mutable.Set[Point]]
        for (point <- points) {
          breakable {
            for (neighborCell <- cellsToSearch) {
              for (coreNeighbor <- neighborCell.points if coreFlagsArr(coreNeighbor.id.toInt)) {
                if (euclideanDistanceSquared(point.vector, coreNeighbor.vector) <= epsilonSquared) {
                  clusters.get(coreNeighbor.id.toInt) match {
                    case Some(clusterId) =>
                      assignedClusters(point.id.toInt) = clusterId
                      borderPointsMap.getOrElseUpdate(clusterId, mutable.Set.empty) += point
                      break()
                    case None =>
                  }
                }
              }
            }
          }
        }
        replyTo ! ProcessedBorderPoints(assignedClusters, borderPointsMap)
        Behaviors.same
    }
  }

}
