package dto

import data.Point.Embedding

object ExtendedVoronoiCellDto {

  def apply(regularCellIdx: Int, neighbors: Set[(Int, Embedding)]): ExtendedVoronoiCellDto =
    new ExtendedVoronoiCellDto(regularCellIdx, neighbors)

}

case class ExtendedVoronoiCellDto(regularCellIdx: Int, neighbors: Set[(Int, Embedding)])
