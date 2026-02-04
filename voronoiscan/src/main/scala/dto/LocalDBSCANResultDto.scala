package dto

import com.fasterxml.jackson.annotation.JsonIgnore
import data.LocalDBSCANResult
import it.unimi.dsi.fastutil.longs.LongOpenHashSet

case class LocalDBSCANResultDto(
    cellIdx: Int,
    corePoints: Array[Array[Long]],
    borderPoints: Array[Array[Long]],
    labelKeys: Array[Long],
    labelValues: Array[Int]
) {

  @JsonIgnore
  @transient
  lazy val corePointSet: LongOpenHashSet =
    new LongOpenHashSet(corePoints.iterator.flatMap(_.iterator).toArray)

  @JsonIgnore
  @transient
  lazy val borderPointSet: LongOpenHashSet =
    new LongOpenHashSet(borderPoints.iterator.flatMap(_.iterator).toArray)

  def isCore(pointId: Long): Boolean = corePointSet.contains(pointId)

  def isCoreCell: Boolean = corePoints.length == 1 && borderPoints.isEmpty

}

object LocalDBSCANResultDto {

  def fromLocalDBSCANResult(result: LocalDBSCANResult): LocalDBSCANResultDto = {
    val size = result.labels.size()
    val keys = new Array[Long](size)
    val values = new Array[Int](size)
    val entrySet = result.labels.long2IntEntrySet().iterator()
    var i = 0
    while (entrySet.hasNext) {
      val entry = entrySet.next()
      keys(i) = entry.getLongKey
      values(i) = entry.getIntValue
      i += 1
    }
    val corePointsIds = result.corePoints.map(cluster => cluster.map(point => point.id))
    val borderPointsIds = result.borderPoints.map(cluster => cluster.map(point => point.id))
    LocalDBSCANResultDto(result.cellIdx, corePointsIds, borderPointsIds, keys, values)
  }

}
