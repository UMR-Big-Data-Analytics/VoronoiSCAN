package api

import data.Point.Embedding
import data.{DelaunayGraph, Point, VoronoiCell}
import spatial.index.KDTree
import utils.Utils

trait Transformer {

  def fitTransform(x: Array[Embedding]): Array[Int]

}

abstract class AbstractVoronoiSCAN extends Transformer {

  def sampleCellCenters(pointsWithIds: Array[Point], n: Int, d: Float): (Array[Point], KDTree) =
    Utils.getSeedPoints(pointsWithIds, n, d)

  def buildVoronoiCells(graph: DelaunayGraph, centers: Array[Point], epsilon: Float): Array[VoronoiCell] =
    Utils.constructVoronoiCells(graph, centers, epsilon)

}
