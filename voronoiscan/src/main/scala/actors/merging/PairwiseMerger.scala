package actors.merging

import components.union_find.StaticUnionFind
import data.LocalClusteringMerge
import dto.LocalDBSCANResultDto
import it.unimi.dsi.fastutil.longs.Long2IntOpenHashMap

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

object PairwiseMerger {

  def merge(left: LocalDBSCANResultDto, right: LocalDBSCANResultDto): LocalClusteringMerge = {
    require(
      left.corePoints.length == left.borderPoints.length,
      s"Left corePoints/borderPoints mismatch: ${left.corePoints.length} != ${left.borderPoints.length}"
    )
    require(
      right.corePoints.length == right.borderPoints.length,
      s"Right corePoints/borderPoints mismatch: ${right.corePoints.length} != ${right.borderPoints.length}"
    )

    val leftLabels = new Long2IntOpenHashMap(left.labelKeys.length)
    for (i <- left.labelKeys.indices)
      leftLabels.put(left.labelKeys(i), left.labelValues(i))

    val rightLabels = new Long2IntOpenHashMap(right.labelKeys.length)
    for (i <- right.labelKeys.indices)
      rightLabels.put(right.labelKeys(i), right.labelValues(i))

    val leftK = left.corePoints.length
    val rightK = right.corePoints.length
    val totalSize = leftK + rightK
    val uf = new StaticUnionFind((0 until totalSize).toSet)

    // Index Left Points
    val leftPointToCluster = new Long2IntOpenHashMap()
    // Add Left Cores
    for (i <- left.corePoints.indices) {
      val pts = left.corePoints(i)
      var k = 0
      while (k < pts.length) {
        leftPointToCluster.put(pts(k), i)
        k += 1
      }
    }
    // Add Left Borders
    for (i <- left.borderPoints.indices) {
      val pts = left.borderPoints(i)
      var k = 0
      while (k < pts.length) {
        leftPointToCluster.putIfAbsent(pts(k), i)
        k += 1
      }
    }

    // Index Right Points
    val rightPointToCluster = new Long2IntOpenHashMap()
    // Add Right Cores
    for (j <- right.corePoints.indices) {
      val pts = right.corePoints(j)
      var k = 0
      while (k < pts.length) {
        rightPointToCluster.put(pts(k), j)
        k += 1
      }
    }
    // Add Right Borders
    for (j <- right.borderPoints.indices) {
      val pts = right.borderPoints(j)
      var k = 0
      while (k < pts.length) {
        rightPointToCluster.putIfAbsent(pts(k), j)
        k += 1
      }
    }

    // Point is Core in LEFT (and exists in Right as Core or Border)
    for (i <- left.corePoints.indices) {
      val pts = left.corePoints(i)
      var k = 0
      while (k < pts.length) {
        val pid = pts(k)
        if (rightPointToCluster.containsKey(pid)) {
          val leftIdx = i
          val rightIdx = rightPointToCluster.get(pid)

          val u = leftIdx
          val v = rightIdx + leftK
          if (uf.find(u) != uf.find(v)) {
            uf.union(u, v)
          }
        }
        k += 1
      }
    }

    // Point is Core in RIGHT (and exists in Left as Core or Border)
    for (j <- right.corePoints.indices) {
      val pts = right.corePoints(j)
      var k = 0
      while (k < pts.length) {
        val pid = pts(k)
        if (leftPointToCluster.containsKey(pid)) {
          val rightIdx = j
          val leftIdx = leftPointToCluster.get(pid)

          val u = leftIdx
          val v = rightIdx + leftK
          if (uf.find(u) != uf.find(v)) {
            uf.union(u, v)
          }
        }
        k += 1
      }
    }

    val rootToComponentIdx = mutable.Map.empty[Int, Int]
    var componentIdx       = 0

    for (i <- 0 until totalSize) {
      val root = uf.find(i)
      if (!rootToComponentIdx.contains(root)) {
        rootToComponentIdx(root) = componentIdx
        componentIdx += 1
      }
    }

    val components = Array.fill(componentIdx)(ArrayBuffer.empty[Int])
    for (i <- 0 until totalSize) {
      val root = uf.find(i)
      components(rootToComponentIdx(root)).addOne(i)
    }

    val merges: collection.Set[(collection.Set[Int], collection.Set[Int])] =
      components.flatMap { component =>
        val leftClusters = mutable.Set.empty[Int]
        val rightClusters = mutable.Set.empty[Int]

        component.foreach { vid =>
          if (vid < leftK) {
            // Check Core first, then Border to find a representative point for the label lookup
            val pts =
              if (left.corePoints(vid).nonEmpty) left.corePoints(vid)
              else left.borderPoints(vid)

            if (pts.nonEmpty) {
              leftClusters += leftLabels(pts.head)
            }
          } else {
            val rid = vid - leftK
            val pts =
              if (right.corePoints(rid).nonEmpty) right.corePoints(rid)
              else right.borderPoints(rid)

            if (pts.nonEmpty) {
              rightClusters += rightLabels(pts.head)
            }
          }
        }

        if (leftClusters.nonEmpty && rightClusters.nonEmpty)
          Some(leftClusters.toSet -> rightClusters.toSet)
        else
          None
      }.toSet

    LocalClusteringMerge(left.cellIdx, right.cellIdx, merges)
  }

}
