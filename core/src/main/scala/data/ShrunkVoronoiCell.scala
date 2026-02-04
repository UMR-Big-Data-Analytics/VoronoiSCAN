package data

import data.Point.Embedding
import utils.Distances.euclideanDistance

case class ShrunkVoronoiCell(regularCell: VoronoiCell, neighbors: collection.Set[(Int, Embedding)]) {

  private val _neighborMap: Map[Int, Embedding] = neighbors.toMap

  // The inner diameter is defined as the minimum distance from the center of the regular cell to any of its inwards moved neighbors
  // divided by 2, which gives the radius of the largest circle that can fit inside the shrunk cell.
  val innerDiameter: Float = _neighborMap.values.map(x => euclideanDistance(regularCell.center, x)).min / 2

  def idx: Int = regularCell.idx

  def getNeighborCenter(idx: Int): Embedding =
    _neighborMap.getOrElse(idx, throw new NoSuchElementException(s"No neighbor with index $idx found"))

  def center: Embedding = regularCell.center

}
