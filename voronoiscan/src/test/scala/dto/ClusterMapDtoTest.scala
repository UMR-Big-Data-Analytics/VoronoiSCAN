package dto

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class ClusterMapDtoTest extends AnyFunSuite with Matchers {

  test("ClusterMapDto should round-trip correctly through toClusterMap and fromClusterMap") {
    val cellIndices      = Array(1, 2, 3, 4)
    val localClusterIds  = Array(10, 20, 30, 40)
    val globalClusterIds = Array(100, 200, 300, 400)

    val originalDto      = ClusterMapDto(cellIndices, localClusterIds, globalClusterIds)
    val clusterMap       = originalDto.toClusterMap
    val reconstructedDto = ClusterMapDto.fromClusterMap(clusterMap)
    val finalClusterMap  = reconstructedDto.toClusterMap

    clusterMap should equal(finalClusterMap)
  }

  test("ClusterMapDto should handle empty arrays in round-trip conversion") {
    val originalDto      = ClusterMapDto(Array.empty, Array.empty, Array.empty)
    val clusterMap       = originalDto.toClusterMap
    val reconstructedDto = ClusterMapDto.fromClusterMap(clusterMap)
    val finalClusterMap  = reconstructedDto.toClusterMap

    clusterMap should equal(finalClusterMap)
    clusterMap should be(empty)
  }

  test("ClusterMapDto should throw exception for mismatched array lengths") {
    val cellIndices      = Array(1, 2)
    val localClusterIds  = Array(10, 20, 30)
    val globalClusterIds = Array(100, 200)

    val dto = ClusterMapDto(cellIndices, localClusterIds, globalClusterIds)

    assertThrows[IllegalArgumentException] {
      dto.toClusterMap
    }
  }

  test("ClusterMapDto should preserve complex cluster mappings in round-trip") {
    val originalMap = Map(
      (1, 10) -> 100,
      (2, 20) -> 200,
      (3, 30) -> 300,
      (1, 11) -> 101,
      (2, 21) -> 201
    )

    val dto              = ClusterMapDto.fromClusterMap(originalMap)
    val reconstructedMap = dto.toClusterMap
    val finalDto         = ClusterMapDto.fromClusterMap(reconstructedMap)
    val finalMap         = finalDto.toClusterMap

    originalMap should equal(reconstructedMap)
    reconstructedMap should equal(finalMap)
  }

  test("ClusterMapDto should handle negative values in round-trip conversion") {
    val originalMap = Map(
      (-1, -10) -> -100,
      (0, 0)    -> 0,
      (1, 10)   -> 100
    )

    val dto              = ClusterMapDto.fromClusterMap(originalMap)
    val reconstructedMap = dto.toClusterMap
    val finalDto         = ClusterMapDto.fromClusterMap(reconstructedMap)
    val finalMap         = finalDto.toClusterMap

    originalMap should equal(reconstructedMap)
    reconstructedMap should equal(finalMap)
  }

  test("ClusterMapDto should handle single mapping in round-trip conversion") {
    val originalMap = Map((42, 24) -> 2424)

    val dto              = ClusterMapDto.fromClusterMap(originalMap)
    val reconstructedMap = dto.toClusterMap
    val finalDto         = ClusterMapDto.fromClusterMap(reconstructedMap)
    val finalMap         = finalDto.toClusterMap

    originalMap should equal(reconstructedMap)
    reconstructedMap should equal(finalMap)
  }

  test("ClusterMapDto should handle large cluster mappings in round-trip conversion") {
    val originalMap = (1 to 1000).map(i => (i, i * 10) -> i * 100).toMap

    val dto              = ClusterMapDto.fromClusterMap(originalMap)
    val reconstructedMap = dto.toClusterMap
    val finalDto         = ClusterMapDto.fromClusterMap(reconstructedMap)
    val finalMap         = finalDto.toClusterMap

    originalMap should equal(reconstructedMap)
    reconstructedMap should equal(finalMap)
  }

}
