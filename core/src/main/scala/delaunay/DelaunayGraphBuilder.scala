package delaunay

import data.{DelaunayGraph, Point}
import org.slf4j.LoggerFactory
import qhull.QhullAdapter
import utils.Distances.euclideanDistance

import scala.collection.mutable

object DelaunayGraphBuilder {

  private val QHULL_PATH = "qhull"

  private val logger = LoggerFactory.getLogger(getClass)

  // Example usage
  def main(args: Array[String]): Unit = {
    val points = Array(
      Point(Array(0.0f, 0.0f), 1),
      Point(Array(1.0f, 0.0f), 2),
      Point(Array(0.0f, 1.0f), 3),
      Point(Array(1.0f, 1.0f), 4),
      Point(Array(0.5f, 0.5f), 5)
    )

    val graph = buildDelaunayGraph(points)
    logger.info(graph.toString)

    logger.info("\nDelaunay Edges:")
    graph.getEdges.foreach { case (v1, v2) =>
      logger.info(s"Edge: $v1 - $v2 (Distance: ${euclideanDistance(points(v1).vector, points(v2).vector)})")
    }
  }

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
