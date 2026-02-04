package dto

import data.VoronoiCell
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class VoronoiCellDtoTest extends AnyFunSuite with Matchers {

  test("VoronoiCellDto should round-trip correctly through DTO conversion") {
    val center  = Array(1.0f, 2.0f, 3.0f)
    val epsilon = 0.5f
    val cell1   = new VoronoiCell(1, center, epsilon)
    val cell2   = new VoronoiCell(2, Array(4.0f, 5.0f, 6.0f), epsilon)
    val cell3   = new VoronoiCell(3, Array(7.0f, 8.0f, 9.0f), epsilon)

    cell1.addNeighbors(Set(cell2, cell3))

    val dto1 = VoronoiCellDto(cell1)
    val dto2 = VoronoiCellDto(cell2)
    val dto3 = VoronoiCellDto(cell3)

    dto1.idx should equal(cell1.idx)
    dto1.center should equal(cell1.center)
    dto1.neighbors should equal(cell1.getNeighbors.map(_.idx))
    dto1.epsilon should equal(cell1.epsilon)
  }

  test("VoronoiCellDto should handle cell with no neighbors") {
    val center  = Array(10.0f, 20.0f)
    val epsilon = 1.0f
    val cell    = new VoronoiCell(42, center, epsilon)

    val dto = VoronoiCellDto(cell)

    dto.idx should equal(cell.idx)
    dto.center should equal(cell.center)
    dto.neighbors should equal(cell.getNeighbors.map(_.idx))
    dto.epsilon should equal(cell.epsilon)
  }

  test("VoronoiCellDto should preserve properties for single neighbor") {
    val cell1 = new VoronoiCell(1, Array(1.0f), 0.1f)
    val cell2 = new VoronoiCell(2, Array(2.0f), 0.1f)

    cell1.addNeighbors(Set(cell2))

    val dto = VoronoiCellDto(cell1)

    dto.idx should equal(cell1.idx)
    dto.neighbors should equal(Set(2))
    dto.epsilon should equal(cell1.epsilon)
  }

  test("VoronoiCellDto should preserve properties for multiple neighbors") {
    val cell1 = new VoronoiCell(1, Array(1.0f), 0.1f)
    val cell2 = new VoronoiCell(2, Array(2.0f), 0.1f)
    val cell3 = new VoronoiCell(3, Array(3.0f), 0.1f)
    val cell4 = new VoronoiCell(4, Array(4.0f), 0.1f)
    val cell5 = new VoronoiCell(5, Array(5.0f), 0.1f)

    cell1.addNeighbors(Set(cell2, cell3, cell4, cell5))

    val dto = VoronoiCellDto(cell1)

    dto.idx should equal(cell1.idx)
    dto.neighbors should equal(Set(2, 3, 4, 5))
    dto.epsilon should equal(cell1.epsilon)
  }

  test("VoronoiCellDto should handle zero epsilon") {
    val cell = new VoronoiCell(0, Array(0.0f, 0.0f), 0.0f)

    val dto = VoronoiCellDto(cell)

    dto.epsilon should equal(cell.epsilon)
    dto.idx should equal(cell.idx)
    dto.center should equal(cell.center)
  }

  test("VoronoiCellDto should handle negative cell index") {
    val cell = new VoronoiCell(-5, Array(-1.0f, -2.0f), 1.5f)

    val dto = VoronoiCellDto(cell)

    dto.idx should equal(cell.idx)
    dto.center should equal(cell.center)
    dto.epsilon should equal(cell.epsilon)
  }

  test("VoronoiCellDto should handle high-dimensional centers") {
    val center = Array(1.0f, 2.0f, 3.0f, 4.0f, 5.0f, 6.0f, 7.0f, 8.0f, 9.0f, 10.0f)
    val cell   = new VoronoiCell(100, center, 2.5f)

    val dto = VoronoiCellDto(cell)

    dto.center should equal(cell.center)
    dto.idx should equal(cell.idx)
    dto.epsilon should equal(cell.epsilon)
  }

  test("VoronoiCellDto should handle large epsilon values") {
    val cell = new VoronoiCell(1, Array(1.0f), Float.MaxValue)

    val dto = VoronoiCellDto(cell)

    dto.epsilon should equal(cell.epsilon)
    dto.idx should equal(cell.idx)
  }

  test("VoronoiCellDto should preserve exact neighbor indices") {
    val cell1 = new VoronoiCell(999, Array(1.0f), 0.1f)
    val cell2 = new VoronoiCell(1000, Array(2.0f), 0.1f)
    val cell3 = new VoronoiCell(0, Array(3.0f), 0.1f)
    val cell4 = new VoronoiCell(-1, Array(4.0f), 0.1f)

    cell1.addNeighbors(Set(cell2, cell3, cell4))

    val dto = VoronoiCellDto(cell1)

    dto.neighbors should equal(Set(1000, 0, -1))
    dto.idx should equal(cell1.idx)
  }

}
