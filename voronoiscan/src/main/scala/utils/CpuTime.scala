package utils

import java.lang.management.ManagementFactory

object CpuTime {

  private val bean = ManagementFactory.getThreadMXBean

  if (bean.isThreadCpuTimeSupported && !bean.isThreadCpuTimeEnabled) {
    try
      bean.setThreadCpuTimeEnabled(true)
    catch {
      case _: SecurityException =>
    }
  }

  def nowNanos: Long =
    if (bean.isThreadCpuTimeSupported && bean.isThreadCpuTimeEnabled) {
      bean.getCurrentThreadCpuTime
    } else {
      0L
    }

  def toMillis(nanos: Long): Long =
    if (nanos <= 0L) {
      0L
    } else {
      nanos / 1000000L
    }

}
