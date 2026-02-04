package actors.clusterer

import actors.clusterer.EpsilonExtensionManager.EpsilonExtensionManagerRequest
import akka.actor.typed.{ActorRef, Behavior}
import akka.actor.typed.scaladsl.{ActorContext, Behaviors}
import cluster.{DBSCAN, DBSCANGrid}
import data.{LocalDBSCANResult, Point, VoronoiCell}
import dto.LocalDBSCANResultDto
import it.unimi.dsi.fastutil.ints.Int2ObjectOpenHashMap
import it.unimi.dsi.fastutil.longs.{Long2IntOpenHashMap, LongOpenHashSet}
import it.unimi.dsi.fastutil.objects.ObjectOpenHashSet
import protocol.ClustererProtocol.ClustererRequest
import protocol.OrchestratorProtocol.OrchestratorRequest
import protocol.StatsProtocol.StatsRequest
import protocol._
import serialization.LocalDBSCANResultDtoConv
import utils.CpuTime
import utils.Distances.{euclideanDistance, euclideanDistanceSquared}

import java.time.Instant
import scala.collection.mutable
import scala.util.control.Breaks.{break, breakable}

object Clusterer {

  final val DEFAULT_NAME: String = "Clusterer"

  def apply(
      cell: VoronoiCell,
      orchestrator: ActorRef[OrchestratorRequest],
      epsilonExtensionManager: ActorRef[EpsilonExtensionManagerRequest],
      stats: ActorRef[StatsRequest],
      writer: ActorRef[WriterProtocol.WriterRequest],
      epsilon: Float,
      minPts: Int,
      dbscanImpl: String
  ): Behavior[ClustererRequest] = {
    require(cell.extendedVoronoiCell != null, "Extended Voronoi Cell must not be null")
    Behaviors.setup { context =>
      new Clusterer(context, cell, orchestrator, epsilonExtensionManager, stats, writer, epsilon, minPts,
        dbscanImpl).behavior
    }
  }

}

class Clusterer private (
    context: ActorContext[ClustererRequest],
    cell: VoronoiCell,
    orchestrator: ActorRef[OrchestratorRequest],
    epsilonExtensionManager: ActorRef[EpsilonExtensionManagerRequest],
    stats: ActorRef[StatsRequest],
    writer: ActorRef[WriterProtocol.WriterRequest], // Add writer reference
    epsilon: Float,
    minPts: Int,
    dbscanImpl: String
) {

  private val epsilonExtensionState = new EpsilonExtensionState(cell)

  private val dbscanState = new DBSCANState(context, cell, epsilon, minPts)

  private var epsilonExtensionCpuTimeMs = 0L

  private var epsilonExtensionLastActivity: Option[Instant] = None

  private var epsilonExtensionPhaseStart: Option[Instant] = None

  private def measureEpsilonCpu[T](block: => T): T = {
    val startCpu = CpuTime.nowNanos
    val result = block
    val endCpu = CpuTime.nowNanos
    epsilonExtensionCpuTimeMs += CpuTime.toMillis(endCpu - startCpu)
    epsilonExtensionLastActivity = Some(Instant.now())
    result
  }

  private def markEpsilonPhaseStartIfNeeded(): Unit =
    if (epsilonExtensionPhaseStart.isEmpty) {
      val now = Instant.now()
      epsilonExtensionPhaseStart = Some(now)
      epsilonExtensionLastActivity = Some(now)
      epsilonExtensionCpuTimeMs = 0L
    }

  epsilonExtensionManager ! EpsilonExtensionManager.RegisterEpsilonExtensionActor(context.self, cell.idx)

  def behavior: Behavior[ClustererRequest] =
    Behaviors.receiveMessage {
      // Partitioning phase
      case ClustererProtocol.AddPoints(points) =>
        measureEpsilonCpu {
          epsilonExtensionState.addPoints(points)
        }
        Behaviors.same
      case ClustererProtocol.ExchangeClustererActors(refs) =>
        measureEpsilonCpu {
          epsilonExtensionState.setClustererActors(refs)
          orchestrator ! OrchestratorProtocol.DistributedVoronoiCellNeighbors()
        }
        Behaviors.same

      // Distribution phase
      case ClustererProtocol.StartIteration() =>
        markEpsilonPhaseStartIfNeeded()
        measureEpsilonCpu {
          context.log.debug(
            "Starting new iteration for cell {} with {}",
            cell.idx,
            epsilonExtensionState.getPoints.length
          )
          epsilonExtensionState.setupNewIteration()
          handleDistributedPoints()
        }
        Behaviors.same
      case ClustererProtocol.DistributePoints(points, senderIdx, replyTo) =>
        require(senderIdx != cell.idx, s"Sender index ($senderIdx) is not different from cell index (${cell.idx})")
        measureEpsilonCpu {
          epsilonExtensionState.receivePoints(points, senderIdx)
          replyTo ! ClustererProtocol.DistributedPoints()
        }
        Behaviors.same
      case ClustererProtocol.DistributedPoints() =>
        measureEpsilonCpu {
          handleDistributedPoints()
        }
        Behaviors.same

      // Clustering phase
      case ClustererProtocol.ExecuteDBSCAN() =>
        val epsilonStartTime = epsilonExtensionPhaseStart.getOrElse(epsilonExtensionState.getStartTime)
        val epsilonEndTime = epsilonExtensionLastActivity.getOrElse(Instant.now())
        stats ! StatsProtocol.ReportEpsilonExtensionTime(
          cell.idx, epsilonStartTime, epsilonEndTime, epsilonExtensionCpuTimeMs, epsilonExtensionState.getNumPointsSend,
          epsilonExtensionState.getNumPointsReceived, epsilonExtensionState.getNumIterations
        )
        val points    = epsilonExtensionState.getPoints
        val numPoints = points.length
        context.log.debug(
          "Executing DBSCAN for cell {} ({}) with {} points",
          cell.idx,
          cell.center,
          numPoints
        )
        val startTime = System.currentTimeMillis()
        val startCpu = CpuTime.nowNanos
        dbscanState.executeDBSCAN(points, dbscanImpl)
        val endCpu = CpuTime.nowNanos
        val endTimeMs = System.currentTimeMillis()
        val cpuTimeMs = CpuTime.toMillis(endCpu - startCpu)
        stats ! StatsProtocol.ReportClusteringTime(
          cell.idx, Instant.ofEpochMilli(startTime), Instant.ofEpochMilli(endTimeMs), numPoints, cpuTimeMs
        )
        orchestrator ! OrchestratorProtocol.ExecutedDBSCAN(context.self, cell.idx, dbscanState.numClusters)
        Behaviors.same
      case ClustererProtocol.SendClusteringResultToEpsilonMerger(actorRef, otherCellIdx) =>
        val sharedBorder = dbscanState.getSharedBorder(otherCellIdx)
        require(
          dbscanState.clusteringResult.borderPoints.length == dbscanState.clusteringResult.corePoints.length,
          s"Border points array length (${dbscanState.clusteringResult.borderPoints.length}) must match core points array length (${dbscanState.clusteringResult.corePoints.length})"
        )
        val dto = LocalDBSCANResultDto.fromLocalDBSCANResult(sharedBorder)
        // Convert DTO to protobuf and serialize to bytes
        val pbDto = LocalDBSCANResultDtoConv.toProto(dto)
        val bytes       = pbDto.toByteArray
        val chunkSize   = 256 * 1024 - 1000 // 256kb - 1000 bytes for overhead
        val totalChunks = (bytes.length + chunkSize - 1) / chunkSize
        for (chunkIdx <- 0 until totalChunks) {
          val start = chunkIdx * chunkSize
          val end   = math.min(start + chunkSize, bytes.length)
          val chunk = bytes.slice(start, end)
          actorRef ! EpsilonMergeProtocol.PushDBSCANResultChunk(chunk, chunkIdx, totalChunks, cell.idx)
        }
        if (totalChunks == 0) {
          // If no chunks were sent, send an empty chunk to indicate completion
          actorRef ! EpsilonMergeProtocol.PushDBSCANResultChunk(Array.emptyByteArray, 0, 1, cell.idx)
        }
        Behaviors.same

      // Labeling phase
      case ClustererProtocol.LabelPoints(clusterMapDto) =>
        val start      = System.currentTimeMillis()
        val startCpu = CpuTime.nowNanos
        val clusterMap = clusterMapDto.toClusterMap
        context.log.debug("Cell {}: had {} clusters", cell.idx, dbscanState.numClusters)
        val (ids, labels) = dbscanState.getLabels(clusterMap, epsilonExtensionState.originalPoints)
        // Chunking logic for LabeledPoints - send to Writer instead of Master
        val maxChunkSize = 256 * 1024 / 12 - 4096 // ~21k elements per chunk (8 bytes for Long + 4 bytes for Int) - 2000 bytes for overhead
        val totalChunks  = (ids.length + maxChunkSize - 1) / maxChunkSize
        for (chunkIdx <- 0 until totalChunks) {
          val start       = chunkIdx * maxChunkSize
          val end         = math.min(start + maxChunkSize, ids.length)
          val idsChunk    = ids.slice(start, end)
          val labelsChunk = labels.slice(start, end)
          writer ! WriterProtocol.LabeledPointsChunk(idsChunk, labelsChunk, chunkIdx, totalChunks)
        }
        if (totalChunks == 0) {
          // If no chunks were sent, send an empty chunk to indicate completion
          writer ! WriterProtocol.LabeledPointsChunk(Array.emptyLongArray, Array.emptyIntArray, 0, 1)
        }
        val end = System.currentTimeMillis()
        val endCpu = CpuTime.nowNanos
        val cpuTimeMs = CpuTime.toMillis(endCpu - startCpu)
        stats ! StatsProtocol.ReportLabelingTime(
          cell.idx,
          Instant.ofEpochMilli(start),
          Instant.ofEpochMilli(end),
          cpuTimeMs
        )
        context.log.debug("Sent {} labeled points in {} chunks", ids.length, totalChunks)
        Behaviors.same
    }

  private def handleDistributedPoints(): Unit =
    epsilonExtensionState.nextBatch match {
      case Some((_, points, ref)) =>
        ref ! ClustererProtocol.DistributePoints(points, cell.idx, context.self)
      case None =>
        epsilonExtensionManager ! EpsilonExtensionManager.IterationDone(epsilonExtensionState.hasPointsToDistribute)
    }

}

private class EpsilonExtensionState(cell: VoronoiCell) {

  private val distributePointsQueue: mutable.Queue[(Int, Array[Point], ActorRef[ClustererRequest])] =
    mutable.Queue.empty

  private val points = mutable.Set[Point]()

  private val BATCH_SIZE = 10000

  private val receivedPoints = {
    val neighbors = cell.getNeighbors
    val p = new Int2ObjectOpenHashMap[LongOpenHashSet](neighbors.size)
    for (neighbor <- neighbors)
      p.put(neighbor.idx, new LongOpenHashSet())
    p
  }

  private val pointsForDistribution = {
    val neighbors = cell.getNeighbors
    val p = new Int2ObjectOpenHashMap[ObjectOpenHashSet[Point]](neighbors.size)
    for (neighbor <- neighbors)
      p.put(neighbor.idx, new ObjectOpenHashSet[Point]())
    p
  }

  private var numPointsSend = 0

  private var numPointsReceived = 0

  private var numIterations = 0

  private var startTime: Instant = _

  private val clustererActors: Int2ObjectOpenHashMap[ActorRef[ClustererRequest]] = new Int2ObjectOpenHashMap()

  val originalPoints = new LongOpenHashSet()

  def addPoints(newPoints: Array[Point]): Unit = {
    if (startTime == null) {
      startTime = Instant.now()
    }
    points ++= newPoints
    for (point <- newPoints) {
      originalPoints.add(point.id)
      breakable {
        if (point.distanceToCenter < cell.getInnerDiameter) {
          break
        }
        for (neighbor <- cell.getNeighbors) {
          if (neighbor.extendedVoronoiCell.contains(point)) {
            numPointsSend += 1
            pointsForDistribution(neighbor.idx).add(point)
          }
        }
      }
    }
  }

  def getStartTime: Instant = startTime

  def getPoints: Array[Point] = points.toArray

  def receivePoints(newPoints: Array[Point], neighborIdx: Int): Unit = {
    for (point <- newPoints) {
      breakable {
        if (points.contains(point)) {
          break
        }
        points            += point
        numPointsReceived += 1

        receivedPoints.get(neighborIdx).add(point.id)
        for (neighbor <- cell.getNeighbors) {
          if (
            neighbor.idx != neighborIdx &&
            !receivedPoints.get(neighbor.idx).contains(point.id) &&
            neighbor.extendedVoronoiCell.contains(point)
          ) {
            numPointsSend += 1
            pointsForDistribution(neighbor.idx).add(point)
          }
        }
      }
    }
  }

  def nextBatch: Option[(Int, Array[Point], ActorRef[ClustererRequest])] =
    if (distributePointsQueue.nonEmpty) {
      Some(distributePointsQueue.dequeue())
    } else {
      None
    }

  def hasPointsToDistribute: Boolean = {
    val it = pointsForDistribution.values().iterator()
    while (it.hasNext) {
      if (!it.next().isEmpty) {
        return true
      }
    }
    false
  }

  def setClustererActors(actors: Array[(Int, ActorRef[ClustererRequest])]): Unit = {
    for (i <- actors.indices) {
      val (idx, ref) = actors(i)
      clustererActors.put(idx, ref)
    }
  }

  def setupNewIteration(): Unit = {
    numIterations += 1
    // Build distribution queue now that clusterer actors should be known
    for (neighbor <- cell.getNeighbors) {
      val neighborIdx = neighbor.idx
      val actorRef    = clustererActors.get(neighborIdx)
      require(
        neighborIdx == actorRef.path.name.split("-").last.toInt,
        s"Actor reference ID (${actorRef.path.name.split("-").last.toInt}) must match neighbor index ($neighborIdx)"
      )
      val pointsToDistribute = getPointsToDistribute(neighborIdx)
      // Group points into batches and enqueue each batch
      pointsToDistribute.grouped(BATCH_SIZE).foreach { batch =>
        distributePointsQueue += ((neighborIdx, batch, actorRef))
      }
    }
  }

  private def getPointsToDistribute(cellIdx: Int): Array[Point] = {
    val pointsSet = pointsForDistribution.get(cellIdx)
    val size = pointsSet.size()
    val result = new Array[Point](size)
    val it = pointsSet.iterator()
    for (i <- 0 until size)
      result(i) = it.next()
    pointsSet.clear()
    result
  }

  def getNumPointsSend: Int = numPointsSend

  def getNumPointsReceived: Int = numPointsReceived

  def getNumIterations: Int = numIterations

}

private class DBSCANState(
    val context: ActorContext[ClustererRequest],
    val cell: VoronoiCell,
    val epsilon: Float,
    val minPts: Int
) {

  var clusteringResult: LocalDBSCANResult = _

  var numClusters: Int = _

  private def getDbscanImpl(dbscanImpl: String) = {
    dbscanImpl match {
      case "grid" => new DBSCANGrid(epsilon, minPts)
      case "standard" => new DBSCAN(epsilon, minPts)
      case other => throw new IllegalArgumentException(s"Unknown DBSCAN implementation: $other")
    }
  }

  @inline def executeDBSCAN(points: Array[Point], dbscanImpl: String): Unit = {
    if (points.isEmpty) {
      clusteringResult = LocalDBSCANResult(cell.idx, Array(Array()), Array(Array()), new Long2IntOpenHashMap(0))
      numClusters = 0
      return
    }
    val maxCenterDist = points.map(p => euclideanDistanceSquared(p.vector, cell.center)).max
    if (maxCenterDist < math.pow(epsilon / 2, 2)) {
      // All points are within epsilon/2 of the center, so they all belong to the same cluster
      val labelMap = new Long2IntOpenHashMap(points.length)
      for (i <- points.indices)
        labelMap.put(points(i).id, 0)
      clusteringResult = LocalDBSCANResult(cell.idx, Array(points), Array(Array()), labelMap)
      numClusters = 1
      return
    }
    val dbscan = getDbscanImpl(dbscanImpl)
    val (labelsArr, _) = dbscan.fit(points)
    val labelMap = new Long2IntOpenHashMap(points.length)
    for (i <- points.indices)
      labelMap.put(points(i).id, labelsArr(i))
    clusteringResult = LocalDBSCANResult(cell.idx, dbscan.getCorePoints, dbscan.getBorderPoints, labelMap)
    numClusters = dbscan.getNumClusters
  }

  @inline def getSharedBorder(otherCellIdx: Int): LocalDBSCANResult = {
    require(
      otherCellIdx != cell.idx,
      s"Other cell index ($otherCellIdx) must not be the same as current cell index (${cell.idx})"
    )
    // Filter points that are in the shared border between the two cells
    val isVeryClose = euclideanDistance(cell.center, cell.getNeighbor(otherCellIdx).center) <= 2 * epsilon

    val thisCellCenter       = cell.center
    val otherMovedCellCenter = cell.shrunkVoronoiCell.getNeighborCenter(otherCellIdx)
    val isInSharedBorder = (point: Point) => {
      val otherDist = euclideanDistanceSquared(otherMovedCellCenter, point.vector)
      val thisDist  = euclideanDistanceSquared(thisCellCenter, point.vector)
      otherDist <= thisDist
    }
    val isInSharedBorderVeryClose = (point: Point) => {
      val otherDist = euclideanDistanceSquared(otherMovedCellCenter, point.vector)
      val thisDist  = euclideanDistanceSquared(thisCellCenter, point.vector)
      otherDist >= thisDist
    }
    val marginPoints = clusteringResult.corePoints.iterator.flatten
      .concat(clusteringResult.borderPoints.iterator.flatten)
      .filter(if (isVeryClose) isInSharedBorderVeryClose else isInSharedBorder)
      .map(_.id)
      .toSet

    clusteringResult.subset(marginPoints)
  }

  @inline def getLabels(
      clusterMap: Map[(Int, Int), Int],
      originalPoints: LongOpenHashSet
  ): (Array[Long], Array[Int]) = {
    val ids = originalPoints.toLongArray
    val labels = new Array[Int](ids.length)
    val primitive = clusteringResult.labels // Use primitive map for performance
    for (i <- ids.indices) {
      val localLabel = primitive.get(ids(i))
      labels(i) = clusterMap.getOrElse((cell.idx, localLabel), -1)
    }
    (ids, labels)
  }

}
