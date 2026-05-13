package actors.merging

import components.union_find.{LockFreeUnionFind, StaticUnionFind, UnionFind}
import data.LocalClusteringMerge
import dto.LocalDBSCANResultDto
import it.unimi.dsi.fastutil.longs.Long2IntOpenHashMap

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.collection.parallel.CollectionConverters._

object PairwiseMerger {

  private val parallelEdgeThreshold = 20000

  private val parallelBucketSize = 4096

  sealed trait MergeMode

  private case object Auto extends MergeMode

  private case object Sequential extends MergeMode

  private case object Parallel extends MergeMode

  def merge(left: LocalDBSCANResultDto, right: LocalDBSCANResultDto): LocalClusteringMerge =
    merge(left, right, Auto)

  def merge(
             left: LocalDBSCANResultDto,
             right: LocalDBSCANResultDto,
             mode: MergeMode,
             bucketSize: Int = parallelBucketSize,
             edgeThreshold: Int = parallelEdgeThreshold
           ): LocalClusteringMerge = {
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
    val allVertices = (0 until totalSize).toSet

    val leftPointToCluster = new Long2IntOpenHashMap()
    for (i <- left.corePoints.indices) {
      val pts = left.corePoints(i)
      var k = 0
      while (k < pts.length) {
        leftPointToCluster.put(pts(k), i)
        k += 1
      }
    }
    for (i <- left.borderPoints.indices) {
      val pts = left.borderPoints(i)
      var k = 0
      while (k < pts.length) {
        leftPointToCluster.putIfAbsent(pts(k), i)
        k += 1
      }
    }

    val rightPointToCluster = new Long2IntOpenHashMap()
    for (j <- right.corePoints.indices) {
      val pts = right.corePoints(j)
      var k = 0
      while (k < pts.length) {
        rightPointToCluster.put(pts(k), j)
        k += 1
      }
    }
    for (j <- right.borderPoints.indices) {
      val pts = right.borderPoints(j)
      var k = 0
      while (k < pts.length) {
        rightPointToCluster.putIfAbsent(pts(k), j)
        k += 1
      }
    }

    val mergeEdges = ArrayBuffer.empty[(Int, Int)]

    for (i <- left.corePoints.indices) {
      val pts = left.corePoints(i)
      var k = 0
      while (k < pts.length) {
        val pid = pts(k)
        if (rightPointToCluster.containsKey(pid)) {
          mergeEdges.addOne(i -> (rightPointToCluster.get(pid) + leftK))
        }
        k += 1
      }
    }

    for (j <- right.corePoints.indices) {
      val pts = right.corePoints(j)
      var k = 0
      while (k < pts.length) {
        val pid = pts(k)
        if (leftPointToCluster.containsKey(pid)) {
          mergeEdges.addOne(leftPointToCluster.get(pid) -> (j + leftK))
        }
        k += 1
      }
    }

    val shouldRunParallel = mode match {
      case Parallel => Runtime.getRuntime.availableProcessors() > 1
      case Sequential => false
      case Auto => Runtime.getRuntime.availableProcessors() > 1 && mergeEdges.length >= edgeThreshold
    }

    val uf: UnionFind[Int] =
      if (shouldRunParallel) {
        val parallelUf = new LockFreeUnionFind[Int](allVertices)
        for (batch <- mergeEdges.grouped(bucketSize)) {
          batch.toVector.par.foreach { case (u, v) =>
            val ru = parallelUf.find(u)
            val rv = parallelUf.find(v)
            if (ru != rv) {
              parallelUf.union(ru, rv)
            }
          }
        }
        parallelUf
      } else {
        val sequentialUf = new StaticUnionFind[Int](allVertices)
        mergeEdges.foreach { case (u, v) =>
          if (sequentialUf.find(u) != sequentialUf.find(v)) {
            sequentialUf.union(u, v)
          }
        }
        sequentialUf
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
      components.par.flatMap { component =>
        val leftClusters = mutable.Set.empty[Int]
        val rightClusters = mutable.Set.empty[Int]

        component.foreach { vid =>
          if (vid < leftK) {
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
      }.seq.toSet

    LocalClusteringMerge(left.cellIdx, right.cellIdx, merges)
  }

}
