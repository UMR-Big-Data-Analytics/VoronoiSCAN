package actors.stats

import akka.actor.typed.Behavior
import akka.actor.typed.scaladsl.{ActorContext, Behaviors, TimerScheduler}
import configuration.{InputConfiguration, SystemConfiguration}
import metrics.MetricWriter
import metrics.entity.{ClusterParameters, DatasetParameters, Measurement}
import protocol.StatsProtocol

import java.io.{File, FileWriter, PrintWriter}
import java.nio.file.Path
import java.time.format.DateTimeFormatter
import java.time.{Duration, Instant, LocalDateTime}
import java.util
import scala.concurrent.duration.DurationInt
import scala.util.{Try, Using}

object Stats {

  final val DEFAULT_NAME = "stats"

  final private val SYSTEM_LOAD_TIMER = "SystemLoadTimer"

  def apply(inputConfiguration: InputConfiguration): Behavior[StatsProtocol.StatsRequest] = Behaviors.setup { context =>
    Behaviors.withTimers { timers =>
      new Stats(context, timers, inputConfiguration).behavior
    }
  }

}

class Stats private (
    context: ActorContext[StatsProtocol.StatsRequest],
    timerScheduler: TimerScheduler[StatsProtocol.StatsRequest],
    inputConfiguration: InputConfiguration
) {

  import Stats._

  private val datasetName = inputConfiguration.inputPath.split("/").last.split("\\.").head

  private val algoName = "VoronoiSCAN-" + inputConfiguration.dbscanImpl

  private val path = Path
    .of(
      inputConfiguration.metricsPath + "/" + datasetName + "/" + algoName + "/" + inputConfiguration.epsilon + "_" + inputConfiguration.minPts
    )
    .toAbsolutePath
    .toString

  private val numNodes: Int = SystemConfiguration.get.numWorkers

  private val partitioningCsvPath = s"$path/partitioning_times.csv"

  private val epsilonExtensionCsvPath = s"$path/epsilon_extension_times.csv"

  private val epsilonExtensionGlobalCsvPath = s"$path/epsilon_extension_global_times.csv"

  private val clusteringCsvPath = s"$path/clustering_times.csv"

  private val epsilonMergingCsvPath = s"$path/epsilon_merging_times.csv"

  private val globalMergingCsvPath = s"$path/global_merging_times.csv"

  private val labelingCsvPath = s"$path/labeling_times.csv"

  private val systemLoad = s"$path/system_load.csv"

  private val totalExecutionCsvPath = s"$path/total_execution_time.csv"

  private val delaunayComputationCsvPath = s"$path/delaunay_computation_times.csv"

  initializeCsvFiles()

  private var totalStartTime: Option[Instant] = None

  private var numEffectivePartitions = 0L

  private var sentNetworkPoints = 0L

  private var sentNetworkEdges = 0L

  def behavior: Behavior[StatsProtocol.StatsRequest] = {
    Behaviors.receiveMessage {
      case StatsProtocol.ReportDelaunayComputationTime(startTime, endTime, numPoints, numDimensions, cpuTimeMs) =>
        val durationMs = Duration.between(startTime, endTime).toMillis
        val commTimeMs = durationMs - cpuTimeMs
        // num_nodes,start_time,end_time,duration_ms,cpu_time_ms,communication_time_ms,num_points,num_dimensions
        appendLine(
          delaunayComputationCsvPath,
          s"$numNodes,$startTime,$endTime,$durationMs,$cpuTimeMs,$commTimeMs,$numPoints,$numDimensions"
        )
        Behaviors.same

      case StatsProtocol.ReportTotalStartTime(startTime) =>
        totalStartTime = Some(startTime)
        Behaviors.same

      case StatsProtocol.ReportTotalEndTime(endTime) =>
        totalStartTime.foreach { startTime =>
          val durationMs = Duration.between(startTime, endTime).toMillis
          // start_time,end_time,duration_ms
          appendLine(totalExecutionCsvPath, s"$startTime,$endTime,$durationMs")

          val path = Path.of(inputConfiguration.metricsPath)
          val writer = new MetricWriter(path)
          val additionalMetrics = new util.HashMap[String, Any]()
          val numPartitionMerges = inputConfiguration.numCells - numEffectivePartitions
          additionalMetrics.put("numEffectivePartitions", numEffectivePartitions)
          additionalMetrics.put("numPartitionMerges", numPartitionMerges)
          additionalMetrics.put("numNodes", numNodes)
          additionalMetrics.put("numDesiredPartitions", inputConfiguration.numCells)
          additionalMetrics.put("exchangedPartitioningPoints", sentNetworkPoints)
          additionalMetrics.put("exchangedMergingEdges", sentNetworkEdges)

          val measurement = new Measurement[ClusterParameters, DatasetParameters](
            algoName,
            durationMs,
            new ClusterParameters(
              inputConfiguration.epsilon,
              inputConfiguration.minPts
            ),
            new DatasetParameters(
              datasetName
            ),
            additionalMetrics
          )
          context.log.info(s"Writing metrics to $path")
          writer.writeMetrics(measurement)
        }
        Behaviors.same

      case StatsProtocol.ReportPartitioningTime(startTime, endTime, numPoints, cpuTimeMs, sentNetworkPoints) =>
        this.sentNetworkPoints += sentNetworkPoints
        writeToCsv(partitioningCsvPath, startTime, endTime, cpuTimeMs, s"$numPoints, $sentNetworkPoints")
        Behaviors.same

      case StatsProtocol.ReportEpsilonExtensionTime(clustererId, startTime, endTime, cpuTimeMs, pointsSent,
        pointsReceived, numIterations, numPointsInCell) =>
        numEffectivePartitions += 1
        if (startTime == null) {
          // When explicitly using grid partitioning (for experiments only), the start time may be null when no points
          // are assigned to a clusterer. In this case, we skip logging.
          Behaviors.same
        } else {
          val durationMs = Duration.between(startTime, endTime).toMillis
          val commTimeMs = durationMs - cpuTimeMs
          val timestamp = getCurrentTimestamp
          // num_nodes,timestamp,clusterer_id,start_time,end_time,cpu_time_ms,duration_ms,communication_time_ms,points_sent,points_received,num_iterations,num_points_in_cell
          appendLine(
            epsilonExtensionCsvPath,
            s"$numNodes,$timestamp,$clustererId,$startTime,$endTime,$cpuTimeMs,$durationMs,$commTimeMs,$pointsSent,$pointsReceived,$numIterations,$numPointsInCell"
          )
          Behaviors.same
        }

      case StatsProtocol.ReportEpsilonExtensionGlobalTime(startTime, endTime) =>
        writeToCsvNoCompute(epsilonExtensionGlobalCsvPath, startTime, endTime, "")
        Behaviors.same

      case StatsProtocol.ReportClusteringTime(clustererId, startTime, endTime, numPoints, cpuTimeMs) =>
        val durationMs = Duration.between(startTime, endTime).toMillis
        val commTimeMs = durationMs - cpuTimeMs
        val timestamp  = getCurrentTimestamp
        // num_nodes,timestamp,clusterer_id,start_time,end_time,cpu_time_ms,duration_ms,communication_time_ms,num_points
        appendLine(
          clusteringCsvPath,
          s"$numNodes,$timestamp,$clustererId,$startTime,$endTime,$cpuTimeMs,$durationMs,$commTimeMs,$numPoints"
        )
        Behaviors.same

      case StatsProtocol.ReportEpsilonMergingTime(
        (clusterA, clusterB),
        startTime,
        endTime,
        cpuTimeMs,
        sentNetworkEdges
      ) =>
        val durationMs = Duration.between(startTime, endTime).toMillis
        val commTimeMs = durationMs - cpuTimeMs
        val timestamp  = getCurrentTimestamp
        this.sentNetworkEdges += sentNetworkEdges
        // num_nodes,timestamp,cluster_a,cluster_b,start_time,end_time,cpu_time_ms,duration_ms,communication_time_ms
        appendLine(
          epsilonMergingCsvPath,
          s"$numNodes,$timestamp,$clusterA,$clusterB,$startTime,$endTime,$cpuTimeMs,$durationMs,$commTimeMs"
        )
        Behaviors.same

      case StatsProtocol.ReportGlobalMergingTime(startTime, endTime, numLocalMerges, cpuTimeMs) =>
        writeToCsv(globalMergingCsvPath, startTime, endTime, cpuTimeMs, s"$numLocalMerges")
        Behaviors.same

      case StatsProtocol.ReportLabelingTime(clustererId, startTime, endTime, cpuTimeMs, sentNetworkPoints) =>
        val durationMs = Duration.between(startTime, endTime).toMillis
        val commTimeMs = durationMs - cpuTimeMs
        val timestamp  = getCurrentTimestamp
        this.sentNetworkPoints += sentNetworkPoints
        // num_nodes,timestamp,clusterer_id,start_time,end_time,cpu_time_ms,duration_ms,communication_time_ms
        appendLine(
          labelingCsvPath,
          s"$numNodes,$timestamp,$clustererId,$startTime,$endTime,$cpuTimeMs,$durationMs,$commTimeMs,$sentNetworkPoints"
        )
        Behaviors.same

      case StatsProtocol.ReportSystemLoad() =>
        val load = SystemLoad.getCurrentSystemLoad
        context.log.debug(
          f"System Load - CPU: ${load.cpuUsage}%.2f%%, Memory Used: ${load.memoryUsed} MB, Total Memory: ${load.memoryTotal} MB, Memory Used Percentage: ${load.memoryUsedPercentage}%.2f%%"
        )
        val timestamp = getCurrentTimestamp
        // timestamp,cpu_usage_percent,memory_used_mb,memory_total_mb,memory_used_percent
        appendLine(
          systemLoad,
          f"$timestamp,${load.cpuUsage}%.2f,${load.memoryUsed},${load.memoryTotal},${load.memoryUsedPercentage}%.2f"
        )
        Behaviors.same

      case StatsProtocol.Shutdown() =>
        timerScheduler.cancel(SYSTEM_LOAD_TIMER)
        Behaviors.stopped
    }
  }

  private def appendLine(filePath: String, line: String): Unit = {
    Try {
      ensureParentDirectory(filePath)
      Using(new PrintWriter(new FileWriter(filePath, true))) { writer =>
        writer.println(line)
      }
    }.recover { case ex =>
      context.log.error(s"Failed to write data to $filePath", ex)
    }
  }

  private def writeToCsv(
                          filePath: String,
                          startTime: Instant,
                          endTime: Instant,
                          cpuTimeMs: Long,
                          additionalData: String
                        ): Unit = {
    val durationMs = Duration.between(startTime, endTime).toMillis
    val commTimeMs = durationMs - cpuTimeMs
    val timestamp = getCurrentTimestamp
    val data =
      if (additionalData.nonEmpty)
        s"$numNodes,$timestamp,$startTime,$endTime,$durationMs,$cpuTimeMs,$commTimeMs,$additionalData"
      else
        s"$numNodes,$timestamp,$startTime,$endTime,$durationMs,$cpuTimeMs,$commTimeMs"
    appendLine(filePath, data)
  }

  private def writeToCsvNoCompute(
                                   filePath: String,
                                   startTime: Instant,
                                   endTime: Instant,
                                   additionalData: String
                                 ): Unit = {
    val durationMs = Duration.between(startTime, endTime).toMillis
    val timestamp  = getCurrentTimestamp
    val data =
      if (additionalData.nonEmpty)
        s"$numNodes,$timestamp,$startTime,$endTime,$durationMs,$additionalData"
      else
        s"$numNodes,$timestamp,$startTime,$endTime,$durationMs"
    appendLine(filePath, data)
  }

  private def getCurrentTimestamp: String =
    LocalDateTime.now().format(DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss.SSS"))

  timerScheduler.startTimerAtFixedRate(SYSTEM_LOAD_TIMER, StatsProtocol.ReportSystemLoad(), 100.milliseconds)

  private def initializeCsvFiles(): Unit = {
    val runDir = new File(path)
    if (!runDir.exists()) {
      runDir.mkdirs()
    } else {
      context.log.info("Stats run directory {} already exists. Overwriting CSV files", runDir.getPath)
    }
    writeHeader(
      partitioningCsvPath,
      "num_nodes,timestamp,start_time,end_time,duration_ms,cpu_time_ms,communication_time_ms,num_points"
    )
    writeHeader(
      epsilonExtensionCsvPath,
      "num_nodes,timestamp,clusterer_id,start_time,end_time,cpu_time_ms,duration_ms,communication_time_ms,points_sent,points_received,num_iterations,num_points_in_cell"
    )
    writeHeader(epsilonExtensionGlobalCsvPath, "num_nodes,timestamp,start_time,end_time,duration_ms")
    writeHeader(
      clusteringCsvPath,
      "num_nodes,timestamp,clusterer_id,start_time,end_time,cpu_time_ms,duration_ms,communication_time_ms,num_points"
    )
    writeHeader(
      epsilonMergingCsvPath,
      "num_nodes,timestamp,cluster_a,cluster_b,start_time,end_time,cpu_time_ms,duration_ms,communication_time_ms"
    )
    writeHeader(
      globalMergingCsvPath,
      "num_nodes,timestamp,start_time,end_time,duration_ms,cpu_time_ms,communication_time_ms,num_local_merges"
    )
    writeHeader(
      labelingCsvPath,
      "num_nodes,timestamp,clusterer_id,start_time,end_time,cpu_time_ms,duration_ms,communication_time_ms"
    )
    writeHeader(systemLoad, "timestamp,cpu_usage_percent,memory_used_mb,memory_total_mb,memory_used_percent")
    writeHeader(totalExecutionCsvPath, "start_time,end_time,duration_ms")
    writeHeader(
      delaunayComputationCsvPath,
      "num_nodes,start_time,end_time,duration_ms,cpu_time_ms,communication_time_ms,num_points,num_dimensions"
    )
  }

  private def writeHeader(filePath: String, header: String): Unit = {
    val file = new File(filePath)
    Try {
      ensureParentDirectory(filePath)
      Using(new PrintWriter(new FileWriter(file))) { writer =>
        writer.println(header)
      }
    }.recover { case ex =>
      context.log.error(s"Failed to write header to $filePath", ex)
    }
  }

  private def ensureParentDirectory(filePath: String): Unit = {
    val parent = new File(filePath).getParentFile
    if (parent != null && !parent.exists()) {
      parent.mkdirs()
    }
  }

}
