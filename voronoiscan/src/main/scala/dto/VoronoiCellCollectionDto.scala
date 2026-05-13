package dto

import data.VoronoiCell

import scala.collection.mutable

object VoronoiCellCollectionDto {

  def apply(cells: Array[VoronoiCell], subset: Set[Int]): VoronoiCellCollectionDto = {
    val cellDtos = cells.map(c => VoronoiCellDto(c))
    new VoronoiCellCollectionDto(cellDtos, subset)
  }

  def toVoronoiCells(dto: VoronoiCellCollectionDto): Array[VoronoiCell] = {
    val cellsDtos = dto.cellsDtos
    val cells     = mutable.Map[Int, VoronoiCell]()

    for (cellDto <- cellsDtos) {
      cells(cellDto.idx) = new VoronoiCell(
        idx = cellDto.idx,
        center = cellDto.center,
        neighbors = Set.empty,
        epsilon = cellDto.epsilon
      )
    }

    for (cellDto <- cellsDtos) {
      val cell = cells(cellDto.idx)
      cellDto.neighbors.map(cells(_)).foreach { neighbor =>
        cell.addNeighbor(neighbor)
      }
    }

    for ((_, cell) <- cells if cell.hasNeighbors) {
      cell.extend(cell.epsilon)
      cell.shrink(cell.epsilon)
    }

    for ((idx, cell) <- cells if cell.hasNeighbors) {
      for (neighbor <- cell.getNeighbors) {
        if (neighbor.hasNeighbors) {
          neighbor.extend(cell.epsilon)
          neighbor.shrink(cell.epsilon)
        }
      }
    }

    dto.subset.map(cells(_)).toArray
  }

}

case class VoronoiCellCollectionDto(
    cellsDtos: Array[VoronoiCellDto],
    subset: Set[Int]
)
