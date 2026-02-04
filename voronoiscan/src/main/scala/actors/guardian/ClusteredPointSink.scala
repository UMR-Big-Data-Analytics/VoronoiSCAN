package actors.guardian

trait ClusteredPointSink {

  protected var numberOfPoints: Long = -1L

  def getMaxId: Long = numberOfPoints - 1

  def collect(ids: Array[Long], labels: Array[Int]): Unit

  def getLabels: Option[Array[Int]]

  def setNumberOfPoints(numPoints: Long): Unit =
    numberOfPoints = numPoints

  def close(): Unit

}
