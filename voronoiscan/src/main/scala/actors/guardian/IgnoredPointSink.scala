package actors.guardian

case class IgnoredPointSink() extends ClusteredPointSink() {

  override def collect(ids: Array[Long], labels: Array[Int]): Unit = {
    // No-op: ignore the collected points
  }

  override def getLabels: Option[Array[Int]] = None

  override def close(): Unit = {
    // No resources to clean up
  }

}
