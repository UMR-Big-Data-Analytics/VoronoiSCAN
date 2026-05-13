package actors.dbscan

import java.util.concurrent.atomic.{AtomicInteger, AtomicLong}

object DBSCANMetrics {

  val activeMarkCore = new AtomicInteger(0)

  val activeCoreConnectivity = new AtomicInteger(0)

  val activeBorderPointBatches = new AtomicInteger(0)

  val totalMarkCoreMessages = new AtomicLong(0)

  val totalConnectivityMessages = new AtomicLong(0)

  val totalBorderPointBatchMessages = new AtomicLong(0)

  def snapshot: String =
    s"Metrics(activeMarkCore=${activeMarkCore.get()}, activeConnectivity=${activeCoreConnectivity.get()}, activeBorderBatches=${activeBorderPointBatches.get()}, totalMarkCore=${totalMarkCoreMessages.get()}, totalConnectivity=${totalConnectivityMessages.get()}, totalBorderBatches=${totalBorderPointBatchMessages.get()})"

}
