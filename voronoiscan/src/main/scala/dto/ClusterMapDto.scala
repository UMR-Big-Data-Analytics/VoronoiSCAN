package dto

case class ClusterMapDto(
    cellIndices: Array[Int],
    localClusterIds: Array[Int],
    globalClusterIds: Array[Int]
) {

  def toClusterMap: Map[(Int, Int), Int] = {
    require(
      cellIndices.length == localClusterIds.length && localClusterIds.length == globalClusterIds.length,
      "All arrays must have the same length"
    )

    cellIndices
      .zip(localClusterIds)
      .zip(globalClusterIds)
      .map { case ((cellIdx, localClusterId), globalClusterId) =>
        (cellIdx, localClusterId) -> globalClusterId
      }
      .toMap
  }

}

object ClusterMapDto {

  def fromClusterMap(clusterMap: Map[(Int, Int), Int]): ClusterMapDto = {
    val (keys, values)                 = clusterMap.toArray.unzip
    val (cellIndices, localClusterIds) = keys.unzip
    ClusterMapDto(cellIndices, localClusterIds, values)
  }

}
