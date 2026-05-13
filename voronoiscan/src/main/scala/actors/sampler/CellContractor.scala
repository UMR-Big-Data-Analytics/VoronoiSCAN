package actors.sampler

import components.union_find.StaticUnionFind
import data.DelaunayGraph
import data.Point
import delaunay.DelaunayGraphBuilder
import spatial.index.KDTree
import utils.Distances.euclideanDistance

object CellContractor {

  private val log = org.slf4j.LoggerFactory.getLogger(getClass)

  def mergeCloseCells(
      centers: Array[Point],
      graph: DelaunayGraph,
      mergeThreshold: Float
  ): (Array[Point], KDTree, DelaunayGraph) = {
    val n = centers.length
    if (n == 0) return (centers, new KDTree(centers).build(), graph)

    val uf = new StaticUnionFind((0 until n).toSet)

    var merged = 0
    for ((u, v) <- graph.edges) {
      if (euclideanDistance(centers(u).vector, centers(v).vector) < mergeThreshold) {
        uf.union(u, v)
        merged += 1
      }
    }

    if (merged == 0) {
      log.info(s"CellContractor: no cells merged (threshold=${mergeThreshold}f, n=$n)")
      return (centers, new KDTree(centers).build(), graph)
    }

    val components: collection.Set[collection.Set[Int]] = uf.getComponents
    val nGroups                                         = components.size
    val sortedRoots: Array[collection.Set[Int]]         = components.toArray.sortBy(_.min)
    val rootToNew: Map[collection.Set[Int], Int]        = sortedRoots.zipWithIndex.toMap
    val oldToNew: Array[Int] = Array.tabulate(n) { i =>
      val component = components.find(_.contains(i)).get
      rootToNew(component)
    }

    val dim    = centers(0).vector.length
    val sums   = Array.fill(nGroups)(new Array[Float](dim))
    val counts = new Array[Int](nGroups)

    for (i <- 0 until n) {
      val g = oldToNew(i)
      counts(g) += 1
      for (d <- 0 until dim) sums(g)(d) += centers(i).vector(d)
    }

    val newCenters: Array[Point] = Array.tabulate(nGroups) { g =>
      val centroid = Array.tabulate(dim)(d => sums(g)(d) / counts(g))
      Point(centroid, g.toLong)
    }

    val newEdges: Set[(Int, Int)] = graph.edges.flatMap { case (u, v) =>
      val nu = oldToNew(u); val nv = oldToNew(v)
      if (nu != nv) Some((math.min(nu, nv), math.max(nu, nv))) else None
    }

    val newGraph  = DelaunayGraphBuilder.buildDelaunayGraph(newCenters)
    val newKdTree = new KDTree(newCenters).build()

    log.info(
      s"CellContractor: merged $merged edges → reduced $n cells to $nGroups " +
        s"(threshold=${mergeThreshold}f, edges ${graph.edges.size} → ${newEdges.size})"
    )

    (newCenters, newKdTree, newGraph)
  }

}
