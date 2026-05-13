package dto

import data.VoronoiCell
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class VoronoiCellCollectionDtoTest extends AnyFunSuite with Matchers {

  test("VoronoiCellCollectionDto should round-trip correctly through conversion methods") {
    val cell1 = new VoronoiCell(1, Array(1.0f, 2.0f), 0.5f)
    val cell2 = new VoronoiCell(2, Array(3.0f, 4.0f), 0.5f)
    val cell3 = new VoronoiCell(3, Array(5.0f, 6.0f), 0.5f)

    cell1.addNeighbors(Set(cell2))
    cell2.addNeighbors(Set(cell1, cell3))
    cell3.addNeighbors(Set(cell2))

    val originalCells = Array(cell1, cell2, cell3)
    val subset        = Set(1, 3)

    val dto                = VoronoiCellCollectionDto(originalCells, subset)
    val reconstructedCells = VoronoiCellCollectionDto.toVoronoiCells(dto)
    val finalDto           = VoronoiCellCollectionDto(originalCells, subset)
    val finalCells         = VoronoiCellCollectionDto.toVoronoiCells(finalDto)

    reconstructedCells.map(_.idx).toSet should equal(subset)
    finalCells.map(_.idx).toSet should equal(subset)
    reconstructedCells.length should equal(finalCells.length)
  }

  test("VoronoiCellCollectionDto should handle empty subset in round-trip") {
    val cell1         = new VoronoiCell(1, Array(1.0f), 0.1f)
    val originalCells = Array(cell1)
    val subset        = Set.empty[Int]

    val dto                = VoronoiCellCollectionDto(originalCells, subset)
    val reconstructedCells = VoronoiCellCollectionDto.toVoronoiCells(dto)

    dto.subset should equal(subset)
    reconstructedCells.length should equal(0)
  }

  test("VoronoiCellCollectionDto should handle empty cell array in round-trip") {
    val originalCells = Array.empty[VoronoiCell]
    val subset        = Set.empty[Int]

    val dto                = VoronoiCellCollectionDto(originalCells, subset)
    val reconstructedCells = VoronoiCellCollectionDto.toVoronoiCells(dto)

    dto.cellsDtos.length should equal(0)
    dto.subset should equal(subset)
    reconstructedCells.length should equal(0)
  }

  test("VoronoiCellCollectionDto should preserve neighbor relationships in round-trip") {
    val cell1 = new VoronoiCell(1, Array(1.0f, 2.0f), 0.5f)
    val cell2 = new VoronoiCell(2, Array(3.0f, 4.0f), 0.5f)
    val cell3 = new VoronoiCell(3, Array(5.0f, 6.0f), 0.5f)

    cell1.addNeighbors(Set(cell2))
    cell2.addNeighbors(Set(cell1, cell3))
    cell3.addNeighbors(Set(cell2))

    val originalCells = Array(cell1, cell2, cell3)
    val subset        = Set(1, 3)

    val dto                = VoronoiCellCollectionDto(originalCells, subset)
    val reconstructedCells = VoronoiCellCollectionDto.toVoronoiCells(dto)

    val cellMap = reconstructedCells.map(c => c.idx -> c).toMap

    cellMap(1).getNeighbors.map(_.idx) should equal(Set(2))
    cellMap(3).getNeighbors.map(_.idx) should equal(Set(2))
  }

  test("VoronoiCellCollectionDto should handle subset with single cell") {
    val cell1 = new VoronoiCell(1, Array(1.0f), 0.1f)
    val cell2 = new VoronoiCell(2, Array(2.0f), 0.1f)

    cell1.addNeighbors(Set(cell2))
    cell2.addNeighbors(Set(cell1))

    val originalCells = Array(cell1, cell2)
    val subset        = Set(1)

    val dto                = VoronoiCellCollectionDto(originalCells, subset)
    val reconstructedCells = VoronoiCellCollectionDto.toVoronoiCells(dto)

    reconstructedCells.length should equal(1)
    reconstructedCells(0).idx should equal(1)
    reconstructedCells(0).getNeighbors.map(_.idx) should equal(Set(2))
  }

  test("VoronoiCellCollectionDto should handle cells with no neighbors") {
    val cell1 = new VoronoiCell(1, Array(1.0f), 0.1f)
    val cell2 = new VoronoiCell(2, Array(2.0f), 0.1f)

    val originalCells = Array(cell1, cell2)
    val subset        = Set(1, 2)

    val dto = VoronoiCellCollectionDto(originalCells, subset)
    val reconstructedCells = VoronoiCellCollectionDto.toVoronoiCells(dto)

    dto.cellsDtos.length should equal(2)
    dto.subset should equal(subset)
    reconstructedCells.map(_.idx).toSet should equal(Set(1, 2))
    reconstructedCells.foreach(_.hasNeighbors should equal(false))
    reconstructedCells.foreach(_.hasExtendedVoronoiCell should equal(false))
    reconstructedCells.foreach(_.hasShrunkVoronoiCell should equal(false))
  }

  test("VoronoiCellCollectionDto should preserve cell properties") {
    val center  = Array(10.5f, 20.7f, 30.9f)
    val epsilon = 2.5f
    val cell    = new VoronoiCell(42, center, epsilon)

    val originalCells = Array(cell)
    val subset        = Set(42)

    val dto = VoronoiCellCollectionDto(originalCells, subset)

    dto.cellsDtos(0).idx should equal(42)
    dto.cellsDtos(0).center should equal(center)
    dto.cellsDtos(0).epsilon should equal(epsilon)
  }

  test("VoronoiCellCollectionDto should handle subset that includes all cells") {
    val cell1 = new VoronoiCell(1, Array(1.0f), 0.1f)
    val cell2 = new VoronoiCell(2, Array(2.0f), 0.1f)
    val cell3 = new VoronoiCell(3, Array(3.0f), 0.1f)

    // Add some neighbors to avoid empty.min issue in shrink method
    cell1.addNeighbors(Set(cell2))
    cell2.addNeighbors(Set(cell1, cell3))
    cell3.addNeighbors(Set(cell2))

    val originalCells = Array(cell1, cell2, cell3)
    val subset        = Set(1, 2, 3)

    val dto                = VoronoiCellCollectionDto(originalCells, subset)
    val reconstructedCells = VoronoiCellCollectionDto.toVoronoiCells(dto)

    reconstructedCells.length should equal(3)
    reconstructedCells.map(_.idx).toSet should equal(Set(1, 2, 3))
  }

  test("VoronoiCellCollectionDto should handle complex neighbor relationships") {
    val cell1 = new VoronoiCell(1, Array(1.0f), 0.1f)
    val cell2 = new VoronoiCell(2, Array(2.0f), 0.1f)
    val cell3 = new VoronoiCell(3, Array(3.0f), 0.1f)
    val cell4 = new VoronoiCell(4, Array(4.0f), 0.1f)

    cell1.addNeighbors(Set(cell2, cell3, cell4))
    cell2.addNeighbors(Set(cell1, cell3))
    cell3.addNeighbors(Set(cell1, cell2, cell4))
    cell4.addNeighbors(Set(cell1, cell3))

    val originalCells = Array(cell1, cell2, cell3, cell4)
    val subset        = Set(1, 2, 4)

    val dto                = VoronoiCellCollectionDto(originalCells, subset)
    val reconstructedCells = VoronoiCellCollectionDto.toVoronoiCells(dto)

    reconstructedCells.length should equal(3)
    val cellMap = reconstructedCells.map(c => c.idx -> c).toMap

    cellMap(1).getNeighbors.map(_.idx) should equal(Set(2, 3, 4))
    cellMap(2).getNeighbors.map(_.idx) should equal(Set(1, 3))
    cellMap(4).getNeighbors.map(_.idx) should equal(Set(1, 3))
  }

  test("VoronoiCellCollectionDto should handle negative cell indices") {
    val cell1 = new VoronoiCell(-1, Array(-1.0f), 0.1f)
    val cell2 = new VoronoiCell(-2, Array(-2.0f), 0.1f)

    cell1.addNeighbors(Set(cell2))
    cell2.addNeighbors(Set(cell1))

    val originalCells = Array(cell1, cell2)
    val subset        = Set(-1, -2)

    val dto                = VoronoiCellCollectionDto(originalCells, subset)
    val reconstructedCells = VoronoiCellCollectionDto.toVoronoiCells(dto)

    reconstructedCells.length should equal(2)
    reconstructedCells.map(_.idx).toSet should equal(Set(-1, -2))
  }

}
