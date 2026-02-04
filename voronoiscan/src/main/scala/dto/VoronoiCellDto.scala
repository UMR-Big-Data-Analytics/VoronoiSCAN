package dto

import data.Point.Embedding
import data.VoronoiCell

object VoronoiCellDto {

  def apply(voronoiCell: VoronoiCell): VoronoiCellDto =
    new VoronoiCellDto(
      idx = voronoiCell.idx,
      center = voronoiCell.center,
      neighbors = voronoiCell.getNeighbors.map(_.idx).toSet,
      epsilon = voronoiCell.epsilon
    )

}

case class VoronoiCellDto(idx: Int, center: Embedding, neighbors: Set[Int], epsilon: Float)
