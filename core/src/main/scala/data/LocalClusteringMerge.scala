package data

case class LocalClusteringMerge(
    leftCellIdx: Int,
    rightCellIdx: Int,
    merges: collection.Set[(collection.Set[Int], collection.Set[Int])]
) {

  override def toString: String = {
    val left  = leftCellIdx.toString
    val right = rightCellIdx.toString
    val mergeStr = merges.map { case (leftIds, rightIds) =>
      s"${leftIds.mkString("{", ",", "}")} <-> ${rightIds.mkString("{", ",", "}")}"
    }
      .mkString(", ")
    s"LocalClusteringMerge($left, $right, $mergeStr)"
  }

}
