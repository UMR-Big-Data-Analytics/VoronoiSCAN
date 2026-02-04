package actors.merging

import components.ConnectedComponents
import data._
import extensions.VertexExtensions.TupleToVertex

import scala.collection.mutable

object GlobalClusterMerger {

  def globalMerge(
      pairwiseMergedClustering: Array[LocalClusteringMerge],
      localClusterings: Array[LocalDBSCANResult]
  ): Map[(Int, Int), Int] = {
    val clusterMap: Map[(Int, Int), Int] = globalMerge(pairwiseMergedClustering)

    var incrementer = clusterMap.size

    val localClusters = localClusterings.flatMap { localClustering =>
      val cellIdx = localClustering.cellIdx
      val it = localClustering.labels.long2IntEntrySet().iterator()
      val tuples = mutable.ArrayBuffer.empty[((Int, Int), Int)]
      while (it.hasNext) {
        val entry = it.next()
        val label = entry.getIntValue
        if (label != -1) {
          val localClusterId = (cellIdx, label)
          if (!clusterMap.contains(localClusterId)) {
            incrementer += 1
            tuples += (localClusterId -> incrementer)
          } else {
            tuples += (localClusterId -> clusterMap(localClusterId))
          }
        }
      }
      tuples
    }.toMap

    val toAdd = localClusters.filterNot { case (key, _) => clusterMap.contains(key) }

    clusterMap ++ toAdd
  }

  def globalMerge(pairwiseMergedClustering: Array[LocalClusteringMerge]): Map[(Int, Int), Int] = {
    val edges = pairwiseMergedClustering.flatMap { mergedClustering =>
      val leftCellIdx  = mergedClustering.leftCellIdx
      val rightCellIdx = mergedClustering.rightCellIdx
      mergedClustering.merges.flatMap { merge =>
        val leftInnerEdges = merge._1.toArray.combinations(2).map { case Array(a, b) =>
          Edge[(Int, Int), Vertex[(Int, Int)]]((leftCellIdx, a).toVertex, (leftCellIdx, b).toVertex)
        }
        val rightInnerEdges = merge._2.toArray.combinations(2).map { case Array(a, b) =>
          Edge[(Int, Int), Vertex[(Int, Int)]]((rightCellIdx, a).toVertex, (rightCellIdx, b).toVertex)
        }
        val leftRightEdges = merge._1.flatMap(a =>
          merge._2.map(b => Edge[(Int, Int), Vertex[(Int, Int)]]((leftCellIdx, a).toVertex, (rightCellIdx, b).toVertex))
        )
        leftInnerEdges ++ rightInnerEdges ++ leftRightEdges
      }
    }.toSet

    val communities = new ConnectedComponents[(Int, Int)]().findConnectedComponents(edges)
    communities.zipWithIndex.flatMap { case (community, idx) =>
      community.map(vertex => vertex.value -> idx)
    }.toMap
  }

}
