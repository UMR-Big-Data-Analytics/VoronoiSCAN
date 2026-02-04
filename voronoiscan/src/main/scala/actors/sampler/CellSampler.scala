package actors.sampler

import data.{DelaunayGraph, Point}
import spatial.index.KDTree

trait CellSampler {

  def getCenters(filePath: String, numSamples: Int, minDistance: Float): (Array[Point], KDTree, DelaunayGraph)

}
