package actors.dbscan

import akka.actor.typed.scaladsl.{ActorContext, Behaviors}
import akka.actor.typed.{ActorRef, Behavior, DispatcherSelector}
import cluster.{ConnectivityCheck, Grid, GridCell}
import components.union_find.LockFreeUnionFind
import data.Point
import protocol._
import utils.Distances.euclideanDistanceSquared

import scala.collection.mutable

object DBSCANGridAkka {

  final case class Result(
                           labels: Array[Int],
                           corePoints: Array[Array[Point]],
                           borderPoints: Array[Array[Point]],
                           numClusters: Int
                         )

  def run[T](
              points: Array[Point],
              epsilon: Float,
              minPts: Int,
              context: ActorContext[T],
              replyTo: ActorRef[Result]
            ): Unit = {
    if (points.isEmpty) {
      replyTo ! Result(Array.emptyIntArray, Array.empty, Array.empty, 0)
      return
    }

    val controller = context.spawn(
      DBSCANController(points, epsilon, minPts, replyTo),
      s"DBSCANController-${System.nanoTime()}",
      DispatcherSelector.fromConfig("akka.worker-pool-dispatcher")
    )
    controller ! DBSCANController.Start
  }

  private[dbscan] def toPointArrays(
                                     core: mutable.Map[Int, mutable.Set[Point]],
                                     border: mutable.Map[Int, mutable.Set[Point]],
                                     pointsMap: mutable.Map[Int, mutable.Set[Point]]
                                   ): Array[Array[Point]] = {
    val maxClusterId = math.max(
      if (core.isEmpty) 0 else core.keys.max,
      if (border.isEmpty) 0 else border.keys.max
    )
    if (pointsMap.isEmpty) Array.fill(maxClusterId + 1)(Array.empty[Point])
    else Array.tabulate(maxClusterId + 1)(id => pointsMap.getOrElse(id, mutable.Set.empty).toArray)
  }
}


object DBSCANController {

  sealed trait Message

  case object Start extends Message

  private final case class MarkCoreDone(coreCells: mutable.Set[GridCell], coreFlags: mutable.LongMap[Boolean])
    extends Message

  private final case class ClusterCoreDone(
                                            clusters: mutable.LongMap[Int],
                                            corePointsMap: mutable.Map[Int, mutable.Set[Point]],
                                            numClusters: Int
                                          ) extends Message

  private final case class BorderDone(borderPointsMap: mutable.Map[Int, mutable.Set[Point]]) extends Message

  private val denseChunkSize = 8192

  def apply(
             points: Array[Point],
             epsilon: Float,
             minPts: Int,
             replyTo: ActorRef[DBSCANGridAkka.Result]
           ): Behavior[Message] =
    new DBSCANController(points, epsilon, minPts, replyTo).behavior
}

private class DBSCANController(
                                points: Array[Point],
                                epsilon: Float,
                                minPts: Int,
                                replyTo: ActorRef[DBSCANGridAkka.Result]
                              ) {

  import DBSCANController._

  private val numWorkers = math.max(1, Runtime.getRuntime.availableProcessors())
  private val workerDispatcher = DispatcherSelector.fromConfig("akka.worker-pool-dispatcher")

  def behavior: Behavior[Message] = Behaviors.setup { context =>
    // All these vars are touched only on the controller's single thread (the work-pulling
    // coordinators hand their aggregated results back via messages), so no synchronization is needed.
    var grid: Grid = null
    var coreFlags: mutable.LongMap[Boolean] = null
    var clusters: mutable.LongMap[Int] = null
    var corePointsMap: mutable.Map[Int, mutable.Set[Point]] = null
    var numClusters = 0

    var tStart = 0L
    var tMark = 0L
    var tCore = 0L
    var tBorder = 0L

    def ms(fromNs: Long): Long = (System.nanoTime() - fromNs) / 1000000L

    def spawnCoordinator[Tk, R](
                                 name: String,
                                 tasks: IndexedSeq[Tk],
                                 workerFactory: ActorRef[CoordinatorProtocol[Tk, R]] => Behavior[WorkPullerMessage[Tk]],
                                 onResult: R => Unit,
                                 onComplete: () => Unit
                               ): Unit = {
      context.spawn(
        WorkPulling.coordinator(tasks, numWorkers, name + "-w", workerFactory, onResult, onComplete, workerDispatcher),
        s"$name-${System.nanoTime()}",
        DispatcherSelector.fromConfig("akka.worker-pool-dispatcher")
      )
    }

    Behaviors.receiveMessage {
      case Start =>
        tStart = System.nanoTime()
        val dim = points.head.dim
        grid = new Grid(points, epsilon, dim, parallel = true)
        context.log.info("[DBSCAN timing] grid build: {} ms", ms(tStart))

        val tRebuild = System.nanoTime()
        grid.rebuildTree()
        context.log.info("[DBSCAN timing] rebuild tree: {} ms", ms(tRebuild))

        tMark = System.nanoTime()
        val tasks = buildMarkCoreTasks(grid)

        val flags = mutable.LongMap.empty[Boolean]
        val cells = mutable.Set.empty[GridCell]

        spawnCoordinator[MarkCoreTask, MarkCoreResult](
          "MarkCoreCoordinator",
          tasks,
          coord => markCoreWorker(grid, epsilon, minPts, coord),
          result =>
            if (result.coreIds.length > 0) {
              result.cell.setIsCore(true)
              cells += result.cell
              var i = 0
              while (i < result.coreIds.length) {
                flags(result.coreIds(i)) = true;
                i += 1
              }
            },
          () => context.self ! MarkCoreDone(cells, flags)
        )
        Behaviors.same

      case MarkCoreDone(coreCells, markedFlags) =>
        context.log.info("[DBSCAN timing] mark core: {} ms", ms(tMark))
        coreFlags = markedFlags

        tCore = System.nanoTime()
        if (coreCells.isEmpty) {
          context.self ! ClusterCoreDone(mutable.LongMap.empty, mutable.Map.empty, 0)
        } else {
          val uf = new LockFreeUnionFind[GridCell](coreCells)
          val sortedCoreCells = coreCells.toArray.sortBy(_.numPoints)(Ordering[Int].reverse)
          val tasks = sortedCoreCells.iterator.map(ClusterCoreTask).toIndexedSeq

          spawnCoordinator[ClusterCoreTask, Unit](
            "ClusterCoreCoordinator",
            tasks,
            coord => clusterCoreWorker(grid, epsilon, coreFlags, uf, coord),
            _ => (),
            () => {
              val (cl, cp, nc) = finalizeClusterCore(uf, sortedCoreCells, coreFlags)
              context.self ! ClusterCoreDone(cl, cp, nc)
            }
          )
        }
        Behaviors.same

      case ClusterCoreDone(coreClusters, coreClusterPoints, clusterCount) =>
        context.log.info("[DBSCAN timing] cluster core: {} ms", ms(tCore))
        clusters = coreClusters
        corePointsMap = coreClusterPoints
        numClusters = clusterCount

        tBorder = System.nanoTime()
        val borderCells = grid.cellsMap.values.iterator.filter(_.numPoints < minPts).toIndexedSeq

        if (borderCells.isEmpty) {
          context.self ! BorderDone(mutable.Map.empty)
        } else {
          val coreClusterLookup = mutable.LongMap.empty[Int]
          coreClusterLookup ++= clusters
          val readonlyFlags = coreFlags
          val assignedClusters = clusters
          val borderPointsMap = mutable.Map.empty[Int, mutable.Set[Point]]

          spawnCoordinator[BorderTask, BorderResult](
            "BorderCoordinator",
            borderCells.map(BorderTask),
            coord => borderWorker(grid, epsilon, readonlyFlags, coreClusterLookup, coord),
            result => {
              val a = result.assignments
              var i = 0
              while (i < a.length) {
                val (point, clusterId) = a(i)
                if (!assignedClusters.contains(point.id)) {
                  assignedClusters(point.id) = clusterId
                  borderPointsMap.getOrElseUpdate(clusterId, mutable.Set.empty) += point
                }
                i += 1
              }
            },
            () => context.self ! BorderDone(borderPointsMap)
          )
        }
        Behaviors.same

      case BorderDone(borderPointsMap) =>
        context.log.info("[DBSCAN timing] cluster border: {} ms", ms(tBorder))
        context.log.info("[DBSCAN timing] total: {} ms", ms(tStart))

        val labels = points.map(p => clusters.getOrElse(p.id, -1))
        val corePoints = DBSCANGridAkka.toPointArrays(corePointsMap, borderPointsMap, corePointsMap)
        val borderPoints = DBSCANGridAkka.toPointArrays(corePointsMap, borderPointsMap, borderPointsMap)
        replyTo ! DBSCANGridAkka.Result(labels, corePoints, borderPoints, numClusters)
        Behaviors.stopped
    }
  }

  private def buildMarkCoreTasks(grid: Grid): IndexedSeq[MarkCoreTask] = {
    val tasks = mutable.ArrayBuffer.empty[MarkCoreTask]
    for (cell <- grid.cellsMap.values) {
      val n = cell.numPoints
      if (n >= minPts) {
        var from = 0
        while (from < n) {
          val until = math.min(from + denseChunkSize, n)
          tasks += MarkCoreTask(cell, from, until, dense = true, estimatedCost = (until - from).toLong)
          from = until
        }
      } else if (n > 0) {
        tasks += MarkCoreTask(cell, 0, n, dense = false, estimatedCost = n.toLong * minPts.toLong)
      }
    }
    tasks.sortBy(_.estimatedCost)(Ordering[Long].reverse).toIndexedSeq
  }

  private def markCoreWorker(
                              grid: Grid,
                              epsilon: Float,
                              minPts: Int,
                              coordinator: ActorRef[CoordinatorProtocol[MarkCoreTask, MarkCoreResult]]
                            ): Behavior[WorkPullerMessage[MarkCoreTask]] =
    Behaviors.setup { _ =>
      val epsilonSq = epsilon.toDouble * epsilon.toDouble
      Behaviors.receive { (context, message) =>
        message match {
          case Assignment(task) =>
            val cell = task.cell
            val coreIds =
              if (task.dense) {
                val arr = new Array[Long](task.until - task.from)
                var i = task.from
                var k = 0
                while (i < task.until) {
                  arr(k) = cell.points(i).id;
                  i += 1;
                  k += 1
                }
                arr
              } else {
                val neighbors = grid.queryEpsNeighborCells(cell.id)
                val cellCount = cell.numPoints
                val buf = mutable.ArrayBuffer.empty[Long]
                var i = task.from
                while (i < task.until) {
                  val point = cell.points(i)
                  val pv = point.vector
                  var count = cellCount
                  var j = 0
                  while (j < neighbors.length && count < minPts) {
                    count += countNeighborsUpTo(neighbors(j), pv, epsilonSq, minPts - count)
                    j += 1
                  }
                  if (count >= minPts) buf += point.id
                  i += 1
                }
                buf.toArray
              }
            coordinator ! TaskCompleted(MarkCoreResult(cell, coreIds), context.self)
            Behaviors.same

          case Dismiss =>
            Behaviors.stopped
        }
      }
    }

  private def countNeighborsUpTo(cell: GridCell, pv: Array[Float], epsilonSq: Double, limit: Int): Int = {
    if (limit <= 0) return 0
    val pts = cell.points
    val n = pts.length
    var found = 0
    var i = 0
    while (i < n && found < limit) {
      if (euclideanDistanceSquared(pv, pts(i).vector) <= epsilonSq) found += 1
      i += 1
    }
    found
  }

  private def clusterCoreWorker(
                                 grid: Grid,
                                 epsilon: Float,
                                 coreFlags: mutable.LongMap[Boolean],
                                 uf: LockFreeUnionFind[GridCell],
                                 coordinator: ActorRef[CoordinatorProtocol[ClusterCoreTask, Unit]]
                               ): Behavior[WorkPullerMessage[ClusterCoreTask]] =
    Behaviors.setup { _ =>
      val checker = new ConnectivityCheck(false, bruteForceFilter = true)
      Behaviors.receive { (context, message) =>
        message match {
          case Assignment(task) =>
            val cell = task.cell
            val neighbors = grid.queryEpsNeighborCells(cell.id)
            var k = 0
            while (k < neighbors.length) {
              val neighbor = neighbors(k)
              if (neighbor.isCore && cell.id > neighbor.id) {
                val rootA = uf.find(cell)
                val rootB = uf.find(neighbor)
                if (rootA != rootB && checker.isConnected(cell, neighbor, coreFlags, epsilon)) {
                  uf.union(rootA, rootB)
                }
              }
              k += 1
            }
            coordinator ! TaskCompleted((), context.self)
            Behaviors.same

          case Dismiss =>
            Behaviors.stopped
        }
      }
    }

  private def finalizeClusterCore(
                                   uf: LockFreeUnionFind[GridCell],
                                   sortedCoreCells: Array[GridCell],
                                   coreFlags: mutable.LongMap[Boolean]
                                 ): (mutable.LongMap[Int], mutable.Map[Int, mutable.Set[Point]], Int) = {
    val clusters = mutable.LongMap.empty[Int]
    val corePointsMap = mutable.Map.empty[Int, mutable.Set[Point]]
    var clusterID = 0
    val rootToClusterID = mutable.Map.empty[GridCell, Int]

    var c = 0
    while (c < sortedCoreCells.length) {
      val cell = sortedCoreCells(c)
      val root = uf.find(cell)
      if (!rootToClusterID.contains(root)) {
        clusterID += 1
        rootToClusterID(root) = clusterID
      }
      val currentClusterId = rootToClusterID(root)
      val pts = cell.points
      var i = 0
      while (i < pts.length) {
        val point = pts(i)
        if (coreFlags.getOrElse(point.id, false)) {
          clusters(point.id) = currentClusterId
          corePointsMap.getOrElseUpdate(currentClusterId, mutable.Set.empty) += point
        }
        i += 1
      }
      c += 1
    }

    val numClusters = sortedCoreCells.iterator.map(uf.find).toSet.size
    (clusters, corePointsMap, numClusters)
  }

  private def borderWorker(
                            grid: Grid,
                            epsilon: Float,
                            coreFlags: mutable.LongMap[Boolean],
                            coreClusterLookup: mutable.LongMap[Int],
                            coordinator: ActorRef[CoordinatorProtocol[BorderTask, BorderResult]]
                          ): Behavior[WorkPullerMessage[BorderTask]] =
    Behaviors.setup { _ =>
      val epsilonSq = epsilon.toDouble * epsilon.toDouble
      Behaviors.receive { (context, message) =>
        message match {
          case Assignment(task) =>
            val cell = task.cell
            val neighbors = grid.queryEpsNeighborCells(cell.id)
            val out = mutable.ArrayBuffer.empty[(Point, Int)]
            val pts = cell.points
            var p = 0
            while (p < pts.length) {
              val point = pts(p)
              if (!coreFlags.getOrElse(point.id, false)) {
                val clusterId =
                  minNeighborClusterId(point, cell, neighbors, coreFlags, coreClusterLookup, epsilonSq)
                if (clusterId != Int.MaxValue) out += ((point, clusterId))
              }
              p += 1
            }
            coordinator ! TaskCompleted(BorderResult(out.toArray), context.self)
            Behaviors.same

          case Dismiss =>
            Behaviors.stopped
        }
      }
    }

  private def minNeighborClusterId(
                                    point: Point,
                                    cell: GridCell,
                                    neighbors: Vector[GridCell],
                                    coreFlags: mutable.LongMap[Boolean],
                                    coreClusterLookup: mutable.LongMap[Int],
                                    epsilonSq: Double
                                  ): Int = {
    var minId = Int.MaxValue

    val homePts = cell.points
    var i = 0
    while (i < homePts.length) {
      val cn = homePts(i)
      if (coreFlags.getOrElse(cn.id, false)) {
        val cid = coreClusterLookup.getOrElse(cn.id, Int.MaxValue)
        if (cid < minId) minId = cid
      }
      i += 1
    }

    val pv = point.vector
    var k = 0
    while (k < neighbors.length) {
      val pts = neighbors(k).points
      var j = 0
      while (j < pts.length) {
        val cn = pts(j)
        if (coreFlags.getOrElse(cn.id, false) &&
          euclideanDistanceSquared(pv, cn.vector) <= epsilonSq) {
          val cid = coreClusterLookup.getOrElse(cn.id, Int.MaxValue)
          if (cid < minId) minId = cid
        }
        j += 1
      }
      k += 1
    }
    minId
  }
}
