package actors.stats

import java.lang.management.ManagementFactory
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter
import scala.util.Try

object SystemLoad {

  private val osBean = ManagementFactory.getOperatingSystemMXBean

  private val memoryBean = ManagementFactory.getMemoryMXBean

  def getCurrentSystemLoad: SystemLoadData = {
    val timestamp = getCurrentTimestamp

    val cpuUsage = getCpuUsage

    val memoryUsage        = memoryBean.getHeapMemoryUsage
    val nonHeapMemoryUsage = memoryBean.getNonHeapMemoryUsage
    val totalMemoryUsed    = memoryUsage.getUsed + nonHeapMemoryUsage.getUsed
    val totalMemoryMax     = memoryUsage.getMax + nonHeapMemoryUsage.getMax

    val memoryUsedMB         = totalMemoryUsed / (1024 * 1024)
    val memoryTotalMB        = totalMemoryMax / (1024 * 1024)
    val memoryUsedPercentage = if (totalMemoryMax > 0) (totalMemoryUsed.toDouble / totalMemoryMax) * 100 else 0.0

    SystemLoadData(
      cpuUsage = cpuUsage, memoryUsed = memoryUsedMB, memoryTotal = memoryTotalMB,
      memoryUsedPercentage = memoryUsedPercentage, timestamp = timestamp
    )
  }

  private def getCurrentTimestamp: String =
    LocalDateTime.now().format(DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss.SSS"))

  private def getCpuUsage: Double = {
    Try {
      osBean match {
        case bean: com.sun.management.OperatingSystemMXBean =>
          val cpuUsage = bean.getProcessCpuLoad * 100
          if (cpuUsage < 0) 0.0 else cpuUsage
        case _ =>
          val loadAverage = osBean.getSystemLoadAverage
          if (loadAverage < 0) 0.0 else (loadAverage / osBean.getAvailableProcessors) * 100
      }
    }.getOrElse(0.0)
  }

  case class SystemLoadData(
      cpuUsage: Double,
      memoryUsed: Long,
      memoryTotal: Long,
      memoryUsedPercentage: Double,
      timestamp: String
  )

}
