package actors.sampler

import actors.sampler.SampleHelper.{getPoints, getRandomSamples}
import cluster.KMeans
import data.{DelaunayGraph, Point}
import spatial.index.KDTree

class KMeansSampler extends CellSampler {

  override def getCenters(
      filePath: String,
      numSamples: Int,
      minDistance: Float
  ): (Array[Point], KDTree, DelaunayGraph) = {
    val lines  = getRandomSamples(filePath, 500 * numSamples)
    val points = getPoints(lines)

    val kmeans = new KMeans(numSamples, maxIterations = 20, tolerance = 1e-4, seed = 42)
    kmeans.fit(points.map(_.vector.map(_.toDouble)))
    val centers = kmeans.getCentroids.zipWithIndex.map { case (c, i) => data.Point(c.map(_.toFloat), i.toLong) }
    val tree    = new KDTree(centers).build()
    val graph   = delaunay.DelaunayGraphBuilder.buildDelaunayGraph(centers)
    (centers, tree, graph)

  }

}
