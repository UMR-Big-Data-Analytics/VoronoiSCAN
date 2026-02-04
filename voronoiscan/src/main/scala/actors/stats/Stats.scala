package actors.stats

import akka.actor.typed.Behavior
import akka.actor.typed.scaladsl.{ActorContext, Behaviors, TimerScheduler}
import configuration.InputConfiguration
import metrics.MetricWriter
import metrics.entity.{ClusterParameters, DatasetParameters, Measurement}
import protocol.StatsProtocol

import java.io.{File, FileWriter, PrintWriter}
import java.nio.file.Path
import java.time.format.DateTimeFormatter
import java.time.{Duration, Instant, LocalDateTime}
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

  private val path = Path.of(inputConfiguration.metricsPath).toAbsolutePath.toString

  import Stats._

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

  def behavior: Behavior[StatsProtocol.StatsRequest] = {
    Behaviors.receiveMessage {
      case StatsProtocol.ReportDelaunayComputationTime(startTime, endTime, numPoints, numDimensions) =>
        val durationMs = Duration.between(startTime, endTime).toMillis
        // start_time,end_time,duration_ms,num_points,num_dimensions
        appendLine(delaunayComputationCsvPath, s"$startTime,$endTime,$durationMs,$numPoints,$numDimensions")
        Behaviors.same

      case StatsProtocol.ReportTotalStartTime(startTime) =>
        totalStartTime = Some(startTime)
        Behaviors.same

      case StatsProtocol.ReportTotalEndTime(endTime) =>
        totalStartTime.foreach { startTime =>
          val durationMs = Duration.between(startTime, endTime).toMillis
          // start_time,end_time,duration_ms
          appendLine(totalExecutionCsvPath, s"$startTime,$endTime,$durationMs")

          val path        = Path.of(inputConfiguration.metricsPath)
          val writer      = new MetricWriter(path)
          val datasetName = inputConfiguration.inputPath.split("/").last.split("\\.").head
          val measurement = new Measurement[ClusterParameters, DatasetParameters](
            "VoronoiSCAN-" + inputConfiguration.dbscanImpl,
            durationMs,
            new ClusterParameters(
              inputConfiguration.epsilon,
              inputConfiguration.minPts
            ),
            new DatasetParameters(
              datasetName
            )
          )
          writer.writeMetrics(measurement)
        }
        Behaviors.same

      case StatsProtocol.ReportPartitioningTime(startTime, endTime, numPoints, cpuTimeMs) =>
        writeToCsv(partitioningCsvPath, startTime, endTime, s"$numPoints,$cpuTimeMs")
        Behaviors.same

      case StatsProtocol.ReportEpsilonExtensionTime(clustererId, startTime, endTime, cpuTime, pointsSent,
            pointsReceived, numIterations) =>
        if (startTime == null) {
          // When explicitly using grid partitioning (for experiments only), the start time may be null when no points
          // are assigned to a clusterer. In this case, we skip logging.
          Behaviors.same
        } else {
          val durationMs = Duration.between(startTime, endTime).toMillis
          val timestamp = getCurrentTimestamp
          // timestamp,clusterer_id,start_time,end_time,cpu_time,duration_ms,points_sent,points_received,num_iterations
          appendLine(
            epsilonExtensionCsvPath,
            s"$timestamp,$clustererId,$startTime,$endTime,$cpuTime,$durationMs,$pointsSent,$pointsReceived,$numIterations"
          )
          Behaviors.same
        }

      case StatsProtocol.ReportEpsilonExtensionGlobalTime(startTime, endTime) =>
        writeToCsv(epsilonExtensionGlobalCsvPath, startTime, endTime, "")
        Behaviors.same

      case StatsProtocol.ReportClusteringTime(clustererId, startTime, endTime, numPoints, cpuTimeMs) =>
        val durationMs = Duration.between(startTime, endTime).toMillis
        val timestamp  = getCurrentTimestamp
        // timestamp,clusterer_id,start_time,end_time,cpu_time_ms,duration_ms,num_points
        appendLine(clusteringCsvPath, s"$timestamp,$clustererId,$startTime,$endTime,$cpuTimeMs,$durationMs,$numPoints")
        Behaviors.same

      case StatsProtocol.ReportEpsilonMergingTime((clusterA, clusterB), startTime, endTime, cpuTimeMs) =>
        val durationMs = Duration.between(startTime, endTime).toMillis
        val timestamp  = getCurrentTimestamp
        // timestamp,cluster_a,cluster_b,start_time,end_time,cpu_time_ms,duration_ms
        appendLine(
          epsilonMergingCsvPath,
          s"$timestamp,$clusterA,$clusterB,$startTime,$endTime,$cpuTimeMs,$durationMs"
        )
        Behaviors.same

      case StatsProtocol.ReportGlobalMergingTime(startTime, endTime, numLocalMerges, cpuTimeMs) =>
        writeToCsv(globalMergingCsvPath, startTime, endTime, s"$numLocalMerges,$cpuTimeMs")
        Behaviors.same

      case StatsProtocol.ReportLabelingTime(clustererId, startTime, endTime, cpuTimeMs) =>
        val durationMs = Duration.between(startTime, endTime).toMillis
        val timestamp  = getCurrentTimestamp
        // timestamp,clusterer_id,start_time,end_time,cpu_time_ms,duration_ms
        appendLine(labelingCsvPath, s"$timestamp,$clustererId,$startTime,$endTime,$cpuTimeMs,$durationMs")
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
      Using(new PrintWriter(new FileWriter(filePath, true))) { writer =>
        writer.println(line)
      }
    }.recover { case ex =>
      context.log.error(s"Failed to write data to $filePath", ex)
    }
  }

  private def writeToCsv(filePath: String, startTime: Instant, endTime: Instant, additionalData: String): Unit = {
    val durationMs = Duration.between(startTime, endTime).toMillis
    val timestamp  = getCurrentTimestamp
    val data       = s"$timestamp,$startTime,$endTime,$durationMs,$additionalData"
    appendLine(filePath, data)
  }

  private def getCurrentTimestamp: String =
    LocalDateTime.now().format(DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss.SSS"))

  timerScheduler.startTimerAtFixedRate(SYSTEM_LOAD_TIMER, StatsProtocol.ReportSystemLoad(), 100.milliseconds)

  private def initializeCsvFiles(): Unit = {
    val dir = new File(s"${inputConfiguration.metricsPath}")
    if (!dir.exists()) {
      dir.mkdirs()
    } else {
      context.log.info("Stats directory {} already exists. Overwriting", dir.getPath)
      dir.delete()
      dir.mkdirs()
    }
    writeHeader(partitioningCsvPath, "timestamp,start_time,end_time,duration_ms,num_points,cpu_time_ms")
    writeHeader(
      epsilonExtensionCsvPath,
      "timestamp,clusterer_id,start_time,end_time,cpu_time,duration_ms,points_sent,points_received,num_iterations"
    )
    writeHeader(epsilonExtensionGlobalCsvPath, "timestamp,start_time,end_time,duration_ms")
    writeHeader(clusteringCsvPath, "timestamp,clusterer_id,start_time,end_time,cpu_time_ms,duration_ms,num_points")
    writeHeader(epsilonMergingCsvPath, "timestamp,cluster_a,cluster_b,start_time,end_time,cpu_time_ms,duration_ms")
    writeHeader(globalMergingCsvPath, "timestamp,start_time,end_time,duration_ms,num_local_merges,cpu_time_ms")
    writeHeader(labelingCsvPath, "timestamp,clusterer_id,start_time,end_time,cpu_time_ms,duration_ms")
    writeHeader(systemLoad, "timestamp,cpu_usage_percent,memory_used_mb,memory_total_mb,memory_used_percent")
    writeHeader(totalExecutionCsvPath, "start_time,end_time,duration_ms")
    writeHeader(delaunayComputationCsvPath, "start_time,end_time,duration_ms,num_points,num_dimensions")
  }

  private def writeHeader(filePath: String, header: String): Unit = {
    val file = new File(filePath)
    Try {
      Using(new PrintWriter(new FileWriter(file))) { writer =>
        writer.println(header)
      }
    }.recover { case ex =>
      context.log.error(s"Failed to write header to $filePath", ex)
    }
  }

}
