package data

import com.fasterxml.jackson.annotation.JsonIgnore
import data.Point.Embedding
import it.unimi.dsi.fastutil.longs.{Long2IntOpenHashMap, LongOpenHashSet}
import spatial.index.KDTree

@deprecated("Use LocalDBSCANResult instead")
case class LocalClusteringResult(
    cell: VoronoiCell,
    points: Array[Embedding],
    pointsIds: Array[Long],
    labels: Array[Int],
    kDTree: KDTree
) extends Serializable {

  private val pointIdMap = Map.from(pointsIds.zip(points))

  private val labelIdMap = Map.from(pointsIds.zip(labels))

  require(points.length == labels.length, "Points and labels must have the same length")

  def getPoint(pointId: Long): Embedding = pointIdMap(pointId)

  def getLabel(pointId: Long): Int = labelIdMap(pointId)

  def getPointsIds: Array[Long] = labelIdMap.filter(_._2 != -1).keys.toArray

  def getKdTree: KDTree = kDTree

  override def toString: String =
    cell.idx.toString

}

case class LocalDBSCANResult(
                              cellIdx: Int,
                              corePoints: Array[Array[Point]],
                              borderPoints: Array[Array[Point]],
                              labels: Long2IntOpenHashMap
) {

  @JsonIgnore
  lazy val corePointSet: LongOpenHashSet =
    new LongOpenHashSet(corePoints.iterator.flatMap(_.iterator).map(_.id).toArray)

  @JsonIgnore
  lazy val borderPointSet: LongOpenHashSet =
    new LongOpenHashSet(borderPoints.iterator.flatMap(_.iterator).map(_.id).toArray)

  def subset(marginPoints: collection.Set[Long]): LocalDBSCANResult = {
    val marginCorePoints   = corePoints.map(_.filter(point => marginPoints.contains(point.id)))
    val marginBorderPoints = borderPoints.map(_.filter(point => marginPoints.contains(point.id)))
    val marginLabelsMap = new Long2IntOpenHashMap(marginPoints.size)
    val it = labels.long2IntEntrySet().iterator()
    while (it.hasNext) {
      val entry = it.next()
      if (marginPoints.contains(entry.getLongKey)) {
        marginLabelsMap.put(entry.getLongKey, entry.getIntValue)
      }
    }
    LocalDBSCANResult(cellIdx, marginCorePoints, marginBorderPoints, marginLabelsMap)
  }

  def isCore(pointId: Long): Boolean = corePointSet.contains(pointId)

  def intersect(other: LocalDBSCANResult): LocalDBSCANResult = {
    val corePointsOtherSet = other.corePointSet
    val borderPointsOtherSet = other.borderPointSet
    val corePointsIntersect = this.corePoints.map(_.filter(p => corePointsOtherSet.contains(p.id)))
    val borderPointsIntersect = this.borderPoints.map(_.filter(p => borderPointsOtherSet.contains(p.id)))
    val estimatedIntersection = corePointsIntersect.map(_.length).sum + borderPointsIntersect.map(_.length).sum
    val labelsIntersectMap = new Long2IntOpenHashMap(estimatedIntersection)
    val it = labels.long2IntEntrySet().iterator()
    while (it.hasNext) {
      val entry = it.next()
      val key = entry.getLongKey
      if (corePointsOtherSet.contains(key) || borderPointsOtherSet.contains(key)) {
        labelsIntersectMap.put(key, entry.getIntValue)
      }
    }
    LocalDBSCANResult(cellIdx, corePointsIntersect, borderPointsIntersect, labelsIntersectMap)
  }

  def isCoreCell: Boolean = corePoints.length == 1 && borderPoints.isEmpty

}
