package dto

import data.Point.Embedding
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class ExtendedVoronoiCellDtoTest extends AnyFunSuite with Matchers {

  test("ExtendedVoronoiCellDto should preserve all properties correctly") {
    val regularCellIdx = 42
    val neighbors = Set(
      (1, Array(1.0f, 2.0f, 3.0f)),
      (2, Array(4.0f, 5.0f, 6.0f)),
      (3, Array(7.0f, 8.0f, 9.0f))
    )

    val dto = ExtendedVoronoiCellDto(regularCellIdx, neighbors)

    dto.regularCellIdx should equal(42)
    dto.neighbors should equal(neighbors)
  }

  test("ExtendedVoronoiCellDto should handle empty neighbors") {
    val regularCellIdx = 10
    val neighbors      = Set.empty[(Int, Embedding)]

    val dto = ExtendedVoronoiCellDto(regularCellIdx, neighbors)

    dto.regularCellIdx should equal(10)
    dto.neighbors should equal(Set.empty)
  }

  test("ExtendedVoronoiCellDto should handle single neighbor") {
    val regularCellIdx = 5
    val neighbors      = Set((99, Array(1.1f, 2.2f)))

    val dto = ExtendedVoronoiCellDto(regularCellIdx, neighbors)

    dto.regularCellIdx should equal(5)
    dto.neighbors should equal(neighbors)
  }

  test("ExtendedVoronoiCellDto should handle different array lengths in neighbors") {
    val regularCellIdx = 7
    val neighbors = Set(
      (1, Array(1.0f)),
      (2, Array(2.0f, 3.0f)),
      (3, Array(4.0f, 5.0f, 6.0f, 7.0f))
    )

    val dto = ExtendedVoronoiCellDto(regularCellIdx, neighbors)

    dto.regularCellIdx should equal(7)
    dto.neighbors should equal(neighbors)
  }

  test("ExtendedVoronoiCellDto should handle negative cell indices") {
    val regularCellIdx = -5
    val neighbors      = Set((-1, Array(-1.0f, -2.0f)))

    val dto = ExtendedVoronoiCellDto(regularCellIdx, neighbors)

    dto.regularCellIdx should equal(-5)
    dto.neighbors should equal(neighbors)
  }

  test("ExtendedVoronoiCellDto should handle zero values") {
    val regularCellIdx = 0
    val neighbors      = Set((0, Array(0.0f, 0.0f, 0.0f)))

    val dto = ExtendedVoronoiCellDto(regularCellIdx, neighbors)

    dto.regularCellIdx should equal(0)
    dto.neighbors should equal(neighbors)
  }

  test("ExtendedVoronoiCellDto should preserve neighbor set uniqueness") {
    val regularCellIdx = 15
    val neighbors = Set(
      (3, Array(3.0f)),
      (1, Array(1.0f)),
      (2, Array(2.0f))
    )

    val dto = ExtendedVoronoiCellDto(regularCellIdx, neighbors)

    dto.neighbors should equal(neighbors)
    dto.neighbors.size should equal(3)
  }

  test("ExtendedVoronoiCellDto should handle large neighbor sets") {
    val regularCellIdx = 100
    val neighbors      = (1 to 50).map(i => (i, Array(i.toFloat, (i * 2).toFloat))).toSet

    val dto = ExtendedVoronoiCellDto(regularCellIdx, neighbors)

    dto.regularCellIdx should equal(100)
    dto.neighbors should equal(neighbors)
    dto.neighbors.size should equal(50)
  }

  test("ExtendedVoronoiCellDto should handle high-dimensional neighbor arrays") {
    val regularCellIdx = 25
    val highDimArray   = (1 to 100).map(_.toFloat).toArray
    val neighbors      = Set((1, highDimArray))

    val dto = ExtendedVoronoiCellDto(regularCellIdx, neighbors)

    dto.regularCellIdx should equal(25)
    dto.neighbors should equal(neighbors)
  }

}
