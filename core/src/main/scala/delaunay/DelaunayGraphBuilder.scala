package delaunay

import data.{DelaunayGraph, Point}
import qhull.QhullAdapter

import scala.collection.mutable

object DelaunayGraphBuilder {

  def buildDelaunayGraph(points: Array[Point]): DelaunayGraph = {
    require(points.nonEmpty, "Points list cannot be empty")
    val result = QhullAdapter.delaunayTriangulation(points.map(_.vector))
    val edges  = mutable.Set[(Int, Int)]()
    result.adjacency().entrySet().forEach { entry =>
      val left = entry.getKey
      entry.getValue.forEach { right =>
        val edge = if (left < right) (left.toInt, right.toInt) else (right.toInt, left.toInt)
        edges += edge
      }
    }
    DelaunayGraph(points, edges.toSet)
  }

}
