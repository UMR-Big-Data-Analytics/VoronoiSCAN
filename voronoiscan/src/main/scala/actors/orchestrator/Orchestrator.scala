package actors.orchestrator

import actors.clusterer.EpsilonExtensionManager
import actors.merging.GlobalMerger
import actors.partitioner.Partitioner
import actors.reader.Reader
import actors.sampler.{GridSampler, KMeansSampler, RandomSampler, Sampler}
import actors.stats.Stats
import akka.actor.typed.scaladsl.{ActorContext, Behaviors}
import akka.actor.typed.{ActorRef, Behavior, ChildFailed, Terminated}
import configuration.{InputConfiguration, SystemConfiguration}
import data._
import delaunay.DelaunayGraphBuilder.buildDelaunayGraph
import dto.{ClusterMapDto, VoronoiCellCollectionDto}
import it.unimi.dsi.fastutil.ints.Int2IntOpenHashMap
import nodedistribution.{KaHIPNodeDistributor, SingleNodeDistributor}
import protocol.StatsProtocol.ReportTotalStartTime
import protocol._
import spatial.index.KDTree
import utils.Utils.constructVoronoiCells

import java.time.Instant
import scala.collection.mutable

object Orchestrator {

  final val DEFAULT_NAME = "Orchestrator"

  def apply(
      master: ActorRef[MasterProtocol.MasterRequest],
      writer: ActorRef[WriterProtocol.WriterRequest]
  ): Behavior[OrchestratorProtocol.OrchestratorRequest] =
    Behaviors.setup { context =>
      new Orchestrator(context, master, writer).behavior
    }

}

private class Orchestrator(
    context: ActorContext[OrchestratorProtocol.OrchestratorRequest],
    master: ActorRef[MasterProtocol.MasterRequest],
    writer: ActorRef[WriterProtocol.WriterRequest]
) {

  master.path.address.hasGlobalScope

  private val state = ClusterState(master, writer)

  def behavior: Behavior[OrchestratorProtocol.OrchestratorRequest] = {
    Behaviors.setup { _ =>
      Behaviors
        .receiveMessage[OrchestratorProtocol.OrchestratorRequest] {
          case OrchestratorProtocol.ExecuteVoronoiSCAN(parameters, workers) =>
            handleExecuteVoronoiSCAN(parameters, workers)

          case OrchestratorProtocol.DistributedVoronoiCellNeighbors() =>
            handleDistributedVoronoiCellNeighbors()

          case OrchestratorProtocol.EpsilonExtensionCompleted() =>
            handleEpsilonExtensionCompleted()

          case OrchestratorProtocol.ExecutedDBSCAN(actorRef, cellIdx, numClusters) =>
            handleExecutedDBSCAN(actorRef, cellIdx, numClusters)

          case OrchestratorProtocol.EpsilonMergeCompleted() =>
            handleEpsilonMergeCompleted()

          case OrchestratorProtocol.ExtractedSamples(points, kdTree, graph) =>
            handleExtractedSamples(points, kdTree, graph)

          case OrchestratorProtocol.SpawnedWorker(clusterer, epsilonMerger) =>
            handleSpawnedWorker(clusterer, epsilonMerger)

          case OrchestratorProtocol.ReaderInitialized() =>
            handleReaderInitialized()

          case OrchestratorProtocol.PartitionerFinished(numPartitionedPoints) =>
            handlePartitionerFinished(numPartitionedPoints)

          case OrchestratorProtocol.FinishedGlobalMerge(globalMergeResult) =>
            handleFinishedGlobalMerge(globalMergeResult)

          case OrchestratorProtocol.Shutdown() =>
            handleShutdown()

          case _ =>
            Behaviors.same
        }
        .receiveSignal {
          case (_, ChildFailed(ref, cause)) =>
            context.log.error(
              "Actor {} failed unexpectedly with exception: {}, shutting down the system",
              ref.path,
              cause.getMessage
            )
            throw cause
          case (_, Terminated(ref)) =>
            context.log.debug("Actor {} terminated", ref.path)
            Behaviors.same
        }
    }
  }

  private def handleExecuteVoronoiSCAN(
      parameters: InputConfiguration,
      workers: Seq[ActorRef[WorkerProtocol.WorkerRequest]]
  ): Behavior[OrchestratorProtocol.OrchestratorRequest] = {
    val stats  = context.spawn(Stats(parameters), actors.stats.Stats.DEFAULT_NAME)
    val reader = context.spawn(Reader(parameters.batchSize), Reader.DEFAULT_NAME)
    context.watch(stats)
    context.watch(reader)
    val cellSampler = parameters.sampler match {
      case "grid" => new GridSampler()
      case "random" =>
        new RandomSampler(
          {
            if (parameters.seed < 0) {
              None
            } else {
              Some(parameters.seed)
            }
          },
          parameters
        )
      case "kmeans" => new KMeansSampler()
      case other => throw new IllegalArgumentException(s"Unknown sampler: $other")
    }
    val sampler = context.spawn(Sampler(context.self, cellSampler), Sampler.DEFAULT_NAME)
    context.watch(sampler)
    state.setReader(reader)
    state.setParameters(parameters)
    state.addWorkers(workers)
    state.setStats(stats)

    stats ! ReportTotalStartTime(Instant.now())

    sampler ! SamplerProtocol.ExtractSample(
      parameters.numCells,
      parameters.minCellDistanceFactor * parameters.epsilon,
      parameters.inputPath
    )
    Behaviors.same
  }

  private def handleDistributedVoronoiCellNeighbors(): Behavior[OrchestratorProtocol.OrchestratorRequest] = {
    state.addDistributeNeighborAck()
    context.log.debug("Received DistributeNeighborsDone")
    checkAndStartPartitioning()
    Behaviors.same
  }

  private def handleEpsilonExtensionCompleted(): Behavior[OrchestratorProtocol.OrchestratorRequest] = {
    context.log.info("All points processed. Executing DBSCAN.")
    state.reportEpsilonExtensionGlobalEnd()
    state.getClusterers.foreach(_ ! ClustererProtocol.ExecuteDBSCAN())
    Behaviors.same
  }

  private def handleExecutedDBSCAN(
      actorRef: ActorRef[ClustererProtocol.ClustererRequest],
      cellIdx: Int,
      numClusters: Int
  ): Behavior[OrchestratorProtocol.OrchestratorRequest] = {
    state.addNewCluster(cellIdx, numClusters)

    val neighborEdges = state.getNeighbors(cellIdx)
    neighborEdges.foreach { edge =>
      val otherCellIdx = if (edge.left.value == cellIdx) edge.right.value else edge.left.value
      val mergeActor = state.getEpsilonMergers(edge)
      actorRef ! ClustererProtocol.SendClusteringResultToEpsilonMerger(mergeActor, otherCellIdx)
    }
    val clusteringsLeft = state.getClusterers.length - state.getClusterCounts.size
    context.log.info("DBSCAN executed for cell {} with {} clusters. Left: {}", cellIdx, numClusters, clusteringsLeft)
    Behaviors.same
  }

  private def handleEpsilonMergeCompleted(): Behavior[OrchestratorProtocol.OrchestratorRequest] = {
    state.completeEpsilonMerge()

    if (state.epsilonMergeCompleted) {
      context.log.info("All epsilon merges completed successfully")
    }
    if (state.getNumberOfOpenMerges % 10 == 0) {
      context.log.info(s"Number of open epsilon merges: ${state.getNumberOfOpenMerges}")
    }
    Behaviors.same
  }

  private def handleExtractedSamples(
      points: Array[Point],
      kdTree: KDTree,
      graph: DelaunayGraph
  ): Behavior[OrchestratorProtocol.OrchestratorRequest] = {
    val parameters = state.getParameters
    val start      = System.currentTimeMillis()
    val _ = buildDelaunayGraph(points)
    val end = System.currentTimeMillis()
    context.log.info("Delaunay graph constructed with {} vertices and {} edges", graph.vertices.size, graph.edges.size)
    state.getStats ! StatsProtocol.ReportDelaunayComputationTime(
      Instant.ofEpochMilli(start), Instant.ofEpochMilli(end), points.length, points.head.dim, cpuTimeMs = 0L
    )
    val cells = constructVoronoiCells(graph, points, parameters.epsilon)
    val globalMerger =
      context.spawn(GlobalMerger(context.self, graph.edges.size, state.getStats), GlobalMerger.DEFAULT_NAME)
    context.watch(globalMerger)
    val epsilonMergeManager =
      context.spawn(EpsilonExtensionManager(context.self, cells), EpsilonExtensionManager.DEFAULT_NAME)
    context.watch(epsilonMergeManager)

    state.setKdTree(kdTree)
    state.setGraph(graph)
    state.setCells(cells)
    state.setEpsilonMergeManager(epsilonMergeManager)
    state.initializeClusterers(cells.length)

    // For the grid partitioning the actual number of cells may differ from the requested number
    // so we report the actual number of constructed cells to the writer in order to shut down correctly
    writer ! WriterProtocol.ReportNumberOfCells(cells.length)

    setupWorkers(graph, cells, parameters, globalMerger)
    context.log.info("Voronoi cells constructed with {} cells", cells.length)
    Behaviors.same
  }

  private def setupWorkers(
                            graph: DelaunayGraph,
                            cells: Array[VoronoiCell],
                            parameters: InputConfiguration,
                            globalMerger: ActorRef[GlobalMergeProtocol.GlobalMergeRequest]
  ): Unit = {
    val cellMapping         = constructWorkerCellMapping(graph, cells)
    val epsilonMergeMapping = constructWorkerEpsilonMergeMapping(graph, cellMapping)
    cellMapping.foreach { case (idx, cellsToSpawn) =>
      val edges    = epsilonMergeMapping(idx).toArray
      val cellsDto = VoronoiCellCollectionDto(cells, cellsToSpawn.map(_.idx).toSet)
      state.getWorkers(idx % state.getWorkers.length) ! WorkerProtocol.SpawnWorker(
        context.self, cellsDto, edges, parameters.epsilon, parameters.minPts, parameters.dbscanImpl, globalMerger,
        context.self, state.getEpsilonMergeManager, state.getStats, state.getWriter
      )
    }
  }

  private def constructWorkerCellMapping(
                                          graph: DelaunayGraph,
                                          cells: Array[VoronoiCell]
  ): Map[Int, Array[VoronoiCell]] = {
    val numWorkers = SystemConfiguration.get.numWorkers
    val distributor = if (numWorkers == 1) {
      SingleNodeDistributor
    } else {
      KaHIPNodeDistributor
    }
    val nodeAssignment = distributor.assignCellsToNodes(graph, numWorkers)

    val workerBuilders = Array.fill(numWorkers)(collection.mutable.ArrayBuffer.empty[VoronoiCell])

    nodeAssignment.foreach { case (cellIdx, workerIdx) =>
      workerBuilders(workerIdx) += cells(cellIdx)
    }

    workerBuilders.zipWithIndex.map { case (builder, workerIdx) =>
      workerIdx -> builder.toArray
    }.toMap
  }

  private def constructWorkerEpsilonMergeMapping(
                                                  graph: DelaunayGraph,
                                                  cellWorkerMapping: Map[Int, Array[VoronoiCell]]
  ): mutable.Map[Int, mutable.Set[(Int, Int)]] = {
    val numCells       = graph.vertices.size
    val reverseMapping = new Array[Int](numCells)
    cellWorkerMapping.foreach { case (workerIdx, cells) =>
      cells.foreach(cell => reverseMapping(cell.idx) = workerIdx)
    }

    val numWorkers = cellWorkerMapping.size
    val mapping    = mutable.Map.empty[Int, mutable.Set[(Int, Int)]]
    for (i <- 0 until numWorkers)
      mapping(i) = mutable.Set.empty[(Int, Int)]

    val (innerEdges, outerEdges) = graph.edges.partition { case (left, right) =>
      reverseMapping(left) == reverseMapping(right)
    }

    // Populate inner edges
    innerEdges.foreach { case (left, right) =>
      val workerIdx = reverseMapping(left)
      mapping(workerIdx) += ((left, right))
    }

    // Distribute outer edges using load balancing
    outerEdges.foreach { case (left, right) =>
      val leftWorker  = reverseMapping(left)
      val rightWorker = reverseMapping(right)
      val workerIdx = if (mapping(leftWorker).size <= mapping(rightWorker).size) {
        leftWorker
      } else {
        rightWorker
      }
      mapping(workerIdx) += ((left, right))
    }

    mapping
  }

  private def handleSpawnedWorker(
      clusterer: Array[ActorRef[ClustererProtocol.ClustererRequest]],
      epsilonMerger: Map[Edge[Int, Vertex[Int]], ActorRef[EpsilonMergeProtocol.EpsilonMergeRequest]]
  ): Behavior[OrchestratorProtocol.OrchestratorRequest] = {
    state.addClusterers(clusterer)
    state.addEpsilonMergers(epsilonMerger)
    context.log.info("Worker spawned with {} clusterers and {} epsilon mergers", clusterer.length, epsilonMerger.size)

    if (state.allWorkersSpawned) {
      val partitioners = spawnPartitioners()
      state.addPartitioners(partitioners)
      state.getReader ! ReaderProtocol.ReadFile(state.getParameters.inputPath, context.self)
    }
    Behaviors.same
  }

  private def spawnPartitioners(): Array[ActorRef[PartitionerProtocol.PartitionerRequest]] =
    (0 until state.getParameters.numPartitioners).map { i =>
      val ref = context.spawn(
        Partitioner(state.getReader, state.getCells, state.getKdTree, state.getClusterers, state.getStats,
          context.self),
        s"${Partitioner.DEFAULT_NAME}-$i"
      )
      context.watch(ref)
      ref
    }.toArray

  private def handleReaderInitialized(): Behavior[OrchestratorProtocol.OrchestratorRequest] = {
    context.log.info("Reader initialized. Starting to read the input file.")
    state.markReaderInitialized()
    checkAndStartPartitioning()
    Behaviors.same
  }

  private def checkAndStartPartitioning(): Unit =
    if (state.partitioningRequirementsMet) {
      context.log.info("All neighbors distributed and reader is initialized Starting point partitioning.")
      for (partitioner <- state.getPartitioner)
        state.getReader ! ReaderProtocol.RequestBatch(partitioner)
    }

  private def handlePartitionerFinished(
      numPartitionedPoints: Long
  ): Behavior[OrchestratorProtocol.OrchestratorRequest] = {
    state.incrementPartitionerDoneCounter(numPartitionedPoints)
    context.log.debug("Partitioner finished. Total done: {}", state.getPartitionerDoneCounter)

    if (state.allPartitionersFinished) {
      context.log.info("Start epsilon extension")
      state.writer ! WriterProtocol.ReportNumberOfPoints(state.getNumberOfPartitionedPoints)
      state.reportEpsilonExtensionGlobalStart()
      state.getEpsilonMergeManager ! EpsilonExtensionManager.PartitionedPoints()
    }
    Behaviors.same
  }

  private def handleFinishedGlobalMerge(
      globalMergeResult: Map[(Int, Int), Int]
  ): Behavior[OrchestratorProtocol.OrchestratorRequest] = {
    context.log.info("Global merge completed")
    val clusterMap    = addMissingLocalClusters(globalMergeResult)
    val clusterMapDto = ClusterMapDto.fromClusterMap(clusterMap)
    state.getClusterers.foreach(_ ! ClustererProtocol.LabelPoints(clusterMapDto))
    Behaviors.same
  }

  private def addMissingLocalClusters(localClusters: Map[(Int, Int), Int]): Map[(Int, Int), Int] = {
    if (localClusters.isEmpty) return Map.empty

    val result          = mutable.Map[(Int, Int), Int]() ++= localClusters
    val maxClusterId    = localClusters.values.max
    var globalClusterId = maxClusterId + 1

    val clusterCounts = state.getClusterCounts
    val it = clusterCounts.int2IntEntrySet().iterator()
    while (it.hasNext) {
      val entry = it.next()
      val cellIdx = entry.getIntKey
      val numLocalClusters = entry.getIntValue
      var localClusterId = 1
      while (localClusterId <= numLocalClusters) {
        val key = (cellIdx, localClusterId)
        if (!result.contains(key)) {
          result(key) = globalClusterId
          globalClusterId += 1
        }
        localClusterId += 1
      }
    }

    result.toMap
  }

  private def handleShutdown(): Behavior[OrchestratorProtocol.OrchestratorRequest] = {
    state.getStats ! StatsProtocol.ReportTotalEndTime(Instant.now())
    state.getStats ! StatsProtocol.Shutdown()
    Behaviors.same
  }

  final private case class ClusterState(
      master: ActorRef[MasterProtocol.MasterRequest],
      writer: ActorRef[WriterProtocol.WriterRequest]
  ) {

    private val clusterCountMap = new Int2IntOpenHashMap()

    private val epsilonMergers
        : mutable.Map[Edge[Int, Vertex[Int]], ActorRef[EpsilonMergeProtocol.EpsilonMergeRequest]] = mutable.Map.empty

    private val workers: mutable.Set[ActorRef[WorkerProtocol.WorkerRequest]] = mutable.Set.empty

    private val partitioners: mutable.Set[ActorRef[PartitionerProtocol.PartitionerRequest]] = mutable.Set.empty

    private var numOpenCellAssignments: Int = -1

    private var numOpenEpsilonMerges: Int = -1

    private var graph: Option[DelaunayGraph] = None

    private var cells: Option[Array[VoronoiCell]] = None

    private var numDistributeNeighborsAcks: Int = 0

    private var clusterers: Array[ActorRef[ClustererProtocol.ClustererRequest]] = Array.empty

    private var reader: Option[ActorRef[ReaderProtocol.ReaderRequest]] = None

    private var stats: Option[ActorRef[StatsProtocol.StatsRequest]] = None

    private var parameters: Option[InputConfiguration] = None

    private var kdTree: Option[KDTree] = None

    private var partitionerDoneCounter: Int = 0

    private var numberPoints = 0L

    private var epsilonMergeManager: Option[ActorRef[EpsilonExtensionManager.EpsilonExtensionManagerRequest]] = None

    private var readerInitialized = false

    private var epsilonExtensionStartTime: Option[Instant] = None

    def getPartitionerDoneCounter: Int =
      partitionerDoneCounter

    def incrementPartitionerDoneCounter(numPartitionedPoints: Long): Unit = {
      partitionerDoneCounter += 1
      numberPoints           += numPartitionedPoints
    }

    def getNumberOfPartitionedPoints: Long = numberPoints

    def getClusterCounts: Int2IntOpenHashMap =
      clusterCountMap

    def addDistributeNeighborAck(): Unit =
      numDistributeNeighborsAcks += 1

    def addNewCluster(cellIdx: Int, numClusters: Int): Unit =
      clusterCountMap.put(cellIdx, numClusters)

    def completeEpsilonMerge(): Unit =
      numOpenEpsilonMerges -= 1

    def setGraph(graph: DelaunayGraph): Unit = {
      this.graph = Some(graph)
      this.numOpenEpsilonMerges = graph.edges.size
      this.numOpenCellAssignments = graph.vertices.size
    }

    def getNeighbors(cellIdx: Int): Set[Edge[Int, Vertex[Int]]] =
      graph match {
        case Some(g) =>
          g.adjacencyList
            .getOrElse(cellIdx, Set.empty[Int])
            .map(otherCellIdx => Edge[Int, Vertex[Int]](new Vertex(cellIdx), new Vertex(otherCellIdx)))
            .toSet
        case None => throw new IllegalStateException("Graph not initialized")
      }

    def getCells: Array[VoronoiCell] =
      cells match {
        case Some(cells) => cells
        case None        => throw new IllegalStateException("Cells not initialized")
      }

    def setCells(cells: Array[VoronoiCell]): Unit =
      this.cells = Some(cells)

    def getEpsilonMergers: collection.Map[Edge[Int, Vertex[Int]], ActorRef[EpsilonMergeProtocol.EpsilonMergeRequest]] =
      epsilonMergers.size match {
        case size if size > 0 => epsilonMergers
        case _                => throw new IllegalStateException("Epsilon mergers not initialized")
      }

    def addEpsilonMergers(
        epsilonMergers: collection.Map[Edge[Int, Vertex[Int]], ActorRef[EpsilonMergeProtocol.EpsilonMergeRequest]]
    ): Unit =
      this.epsilonMergers ++= epsilonMergers

    def initializeClusterers(numCells: Int): Unit =
      clusterers = Array.fill(numCells)(null)

    def getClusterers: Array[ActorRef[ClustererProtocol.ClustererRequest]] =
      clusterers.length match {
        case size if size > 0 => clusterers
        case _                => throw new IllegalStateException("Clusterers not initialized")
      }

    def addClusterers(clusterers: Array[ActorRef[ClustererProtocol.ClustererRequest]]): Unit = {
      for (clusterer <- clusterers) {
        val cellIdx = clusterer.path.name.split("-").last.toInt
        this.clusterers(cellIdx) = clusterer
      }
    }

    def getPartitioner: Array[ActorRef[PartitionerProtocol.PartitionerRequest]] =
      partitioners.toArray

    def addPartitioners(partitioners: collection.Seq[ActorRef[PartitionerProtocol.PartitionerRequest]]): Unit =
      this.partitioners ++= partitioners

    def getReader: ActorRef[ReaderProtocol.ReaderRequest] =
      reader match {
        case Some(actorRef) => actorRef
        case None           => throw new IllegalStateException("Reader not initialized")
      }

    def setReader(reader: ActorRef[ReaderProtocol.ReaderRequest]): Unit =
      this.reader = Some(reader)

    def getParameters: InputConfiguration =
      parameters match {
        case Some(params) => params
        case None         => throw new IllegalStateException("Parameters not initialized")
      }

    def setParameters(parameters: InputConfiguration): Unit =
      this.parameters = Some(parameters)

    def epsilonMergeCompleted: Boolean = numOpenEpsilonMerges == 0

    def addWorkers(workers: collection.Seq[ActorRef[WorkerProtocol.WorkerRequest]]): Unit =
      this.workers ++= workers

    def getWorkers: Array[ActorRef[WorkerProtocol.WorkerRequest]] =
      if (workers.nonEmpty) {
        workers.toArray
      } else {
        throw new IllegalStateException("Workers not initialized")
      }

    def getKdTree: KDTree =
      kdTree match {
        case Some(tree) => tree
        case None       => throw new IllegalStateException("KDTree not initialized")
      }

    def setKdTree(kdTree: KDTree): Unit =
      this.kdTree = Some(kdTree)

    def getEpsilonMergeManager: ActorRef[EpsilonExtensionManager.EpsilonExtensionManagerRequest] =
      epsilonMergeManager match {
        case Some(manager) => manager
        case None          => throw new IllegalStateException("Epsilon merge manager not initialized")
      }

    def setEpsilonMergeManager(
        epsilonMergeManager: ActorRef[EpsilonExtensionManager.EpsilonExtensionManagerRequest]
    ): Unit =
      this.epsilonMergeManager = Some(epsilonMergeManager)

    def markReaderInitialized(): Unit = readerInitialized = true

    def reportEpsilonExtensionGlobalStart(): Unit =
      epsilonExtensionStartTime = Some(Instant.now())

    def reportEpsilonExtensionGlobalEnd(): Unit = {
      val startTime = epsilonExtensionStartTime.getOrElse(Instant.now())
      getStats ! StatsProtocol.ReportEpsilonExtensionGlobalTime(startTime, Instant.now())
    }

    def partitioningRequirementsMet: Boolean = isReaderInitialized && allNeighborsDistributed

    def allNeighborsDistributed: Boolean =
      graph match {
        case Some(g) => numDistributeNeighborsAcks == g.vertices.size
        case None    => throw new IllegalStateException("Graph not initialized")
      }

    def isReaderInitialized: Boolean = readerInitialized

    def allWorkersSpawned: Boolean = {
      graph match {
        case Some(g) =>
          g.vertices.length == clusterers.length &&
          epsilonMergers.size == g.edges.size
        case None => false
      }
    }

    def allPartitionersFinished: Boolean =
      parameters match {
        case Some(params) => partitionerDoneCounter == params.numPartitioners
        case None         => false
      }

    def getNumberOfOpenMerges: Int = numOpenEpsilonMerges

    def getStats: ActorRef[StatsProtocol.StatsRequest] =
      stats match {
        case Some(ref) => ref
        case None      => throw new IllegalStateException("Stats actor not initialized")
      }

    def setStats(ref: ActorRef[StatsProtocol.StatsRequest]): Unit = stats = Some(ref)

    def getWriter: ActorRef[WriterProtocol.WriterRequest] = writer

  }

}
