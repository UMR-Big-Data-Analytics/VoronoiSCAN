package utils

import data.Point.Embedding
import data.{DelaunayGraph, Point, VoronoiCell}
import spatial.index.KDTree

import scala.collection.mutable
import scala.util.Random

object Utils {

  private val logger = org.slf4j.LoggerFactory.getLogger(this.getClass)

  def addPointIds(x: Array[Embedding]): Array[Point] =
    x.zipWithIndex.map { case (point, id) => Point(point, id) }

  def getSeedPoints(pointsWithIds: Array[Point], n: Int, d: Float): (Array[Point], KDTree) = {
    require(pointsWithIds.nonEmpty, "Empty points")
    require(n <= pointsWithIds.length, "n > points.length")

    val selectedPoints = mutable.Set.empty[Point]

    while (selectedPoints.size < n) {
      val pointIdx = (Random.nextDouble() * pointsWithIds.length).toInt
      val point = pointsWithIds(pointIdx)
      if (!selectedPoints.contains(point)) {
        selectedPoints += point
      }
    }
    val kdTree = new KDTree(selectedPoints.toArray).build()

    (selectedPoints.toArray, kdTree)
  }

  def constructVoronoiCells(
                             graph: DelaunayGraph,
                             centers: Array[Point],
                             epsilon: Float
  ): Array[VoronoiCell] = {
    require(centers.nonEmpty, "Empty centers")
    require(graph != null, "Delaunay graph is not initialized")

    val cells = centers.zipWithIndex.map { case (center, idx) => new VoronoiCell(idx, center.vector, epsilon) }
    for ((cellIndex, neighbors) <- graph.adjacencyList) {
      val cell          = cells(cellIndex)
      val neighborCells = neighbors.map(cells)
      neighborCells.foreach { neighbor =>
        cell.addNeighbor(neighbor)
        neighbor.addNeighbor(cell)
      }
      cell.extend(epsilon)
      cell.shrink(epsilon)
    }
    cells
  }

}
