package actors.guardian

import it.unimi.dsi.fastutil.longs.Long2IntOpenHashMap

case class MemoryClusteredPointSink() extends ClusteredPointSink {

  var map = new Long2IntOpenHashMap()

  private var maxId = -1L

  override def collect(ids: Array[Long], labels: Array[Int]): Unit =
    for (i <- ids.indices) {
      val id    = ids(i)
      val label = labels(i)
      if (id > maxId) {
        maxId = id
      }
      map.put(id, label)
    }

  override def getLabels: Option[Array[Int]] = {
    val labels = Array.fill(map.size)(-1)
    val iterator = map.long2IntEntrySet().iterator()
    while (iterator.hasNext) {
      val entry = iterator.next()
      val id = entry.getLongKey
      val label = entry.getIntValue
      labels(id.toInt) = label
    }
    Some(labels)
  }

  override def getMaxId: Long = maxId

  override def close(): Unit = {}

}
