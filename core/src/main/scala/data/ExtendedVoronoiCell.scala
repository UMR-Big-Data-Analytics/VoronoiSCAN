package data

import data.Point.Embedding
import utils.Distances.euclideanDistanceSquared

case class ExtendedVoronoiCell(regularCell: VoronoiCell, neighbors: collection.Set[(Int, Embedding)]) {

  private val movedNeighbors: Map[Int, Embedding] = neighbors.toMap

  def idx: Int = regularCell.idx

  def contains(point: Point): Boolean = {
    val dist = euclideanDistanceSquared(center, point.vector)
    for (neighborCenter <- movedNeighbors.values) {
      if (euclideanDistanceSquared(neighborCenter, point.vector) < dist) {
        return false
      }
    }
    true
  }

  def center: Embedding = regularCell.center

}
