package data

import data.Point.Embedding
import utils.ArrayOps._
import utils.Distances.{euclideanDistanceSquared, norm}

import scala.collection.mutable

class VoronoiCell(val idx: Int, val center: Embedding, neighbors: Set[VoronoiCell], val epsilon: Float)
  extends Serializable {

  private val _neighborsMap: mutable.Map[Int, VoronoiCell] = mutable.Map.from(neighbors.map(n => (n.idx, n)))

  private val _neighborsSet: mutable.Set[VoronoiCell] = mutable.Set.from(neighbors)

  private var _extendedVoronoiCell: Option[ExtendedVoronoiCell] = None

  private var _shrunkVoronoiCell: Option[ShrunkVoronoiCell] = None

  private var _isCore: Boolean = false

  def this(idx: Int, center: Embedding, epsilon: Float) = this(idx, center, Set.empty, epsilon)

  def addNeighbor(neighbor: VoronoiCell): VoronoiCell = {
    _neighborsMap += (neighbor.idx -> neighbor)
    _neighborsSet += neighbor
    this
  }

  def addNeighbors(neighbors: Iterable[VoronoiCell]): VoronoiCell = {
    for (neighbor <- neighbors)
      addNeighbor(neighbor)
    this
  }

  def hasNeighbors: Boolean = _neighborsMap.nonEmpty

  def hasExtendedVoronoiCell: Boolean = _extendedVoronoiCell.nonEmpty

  def hasShrunkVoronoiCell: Boolean = _shrunkVoronoiCell.nonEmpty

  def getNeighbors: mutable.Set[VoronoiCell] = _neighborsSet

  def getNeighbor(idx: Int): VoronoiCell =
    _neighborsMap.getOrElse(idx, throw new NoSuchElementException(s"No neighbor with index $idx found"))

  def extendedVoronoiCell: ExtendedVoronoiCell = _extendedVoronoiCell match {
    case Some(extendedCell) => extendedCell
    case None               => throw new IllegalStateException("Extended Voronoi cell is not initialized")
  }

  def shrunkVoronoiCell: ShrunkVoronoiCell = _shrunkVoronoiCell match {
    case Some(shrunkCell) => shrunkCell
    case None             => throw new IllegalStateException("Shrunk Voronoi cell is not initialized")
  }

  def extend(epsilon: Float): ExtendedVoronoiCell = {
    _extendedVoronoiCell = Some(
      ExtendedVoronoiCell(this, _neighborsSet.map(x => (x.idx, moveNeighborOutwards(x, epsilon))))
    )
    _extendedVoronoiCell.get
  }

  def shrink(epsilon: Float): ShrunkVoronoiCell = {
    _shrunkVoronoiCell = Some(ShrunkVoronoiCell(this, _neighborsSet.map(x => (x.idx, moveNeighborInwards(x, epsilon)))))
    _shrunkVoronoiCell.get
  }

  def contains(point: Embedding): Boolean = {
    val distToCenter = euclideanDistanceSquared(center, point)
    for (neighbor <- _neighborsMap.values) {
      if (euclideanDistanceSquared(neighbor.center, point) < distToCenter) {
        return false
      }
    }
    true
  }

  def getInnerDiameter: Float = shrunkVoronoiCell.innerDiameter

  private def moveNeighborInwards(neighbor: VoronoiCell, epsilon: Float): Embedding = {
    val direction     = neighbor.center - center
    val directionUnit = direction / norm(direction)
    neighbor.center - (directionUnit * (2 * epsilon))
  }

  private def moveNeighborOutwards(neighbor: VoronoiCell, epsilon: Float): Embedding = {
    val direction     = neighbor.center - center
    val directionUnit = direction / norm(direction)
    neighbor.center + (directionUnit * (2 * epsilon))
  }

  override def equals(obj: Any): Boolean = obj match {
    case that: VoronoiCell =>
      this.idx == that.idx
    case _ => false
  }

  override def hashCode(): Int =
    31 * idx + center.hashCode()

}
