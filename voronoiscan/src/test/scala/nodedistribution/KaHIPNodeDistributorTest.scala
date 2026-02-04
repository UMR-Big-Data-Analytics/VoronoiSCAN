package nodedistribution

import data.{DelaunayGraph, Point}
import data.Point.Embedding
import delaunay.DelaunayGraphBuilder
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.collection.mutable

class KaHIPNodeDistributorTest extends AnyFlatSpec with Matchers {

  "KaHIPNodeDistributor" should "assign nodes to partitions" in {
    val vertices = IndexedSeq(
      Point(Array(0.0f, 0.0f), 0L),
      Point(Array(1.0f, 0.0f), 1L),
      Point(Array(0.0f, 1.0f), 2L),
      Point(Array(1.0f, 1.0f), 3L)
    )
    val edges = Set(
      (0, 1),
      (0, 2),
      (1, 3),
      (2, 3)
    )
    val graph    = DelaunayGraph(vertices, edges)
    val numNodes = 2

    val assignments = KaHIPNodeDistributor.assignCellsToNodes(graph, numNodes)

    assignments.size should be(vertices.size)
    assignments.values.toSet.size should be(numNodes)
    assignments.keys.toSet should be(vertices.indices.toSet)
  }

  it should "handle a graph with a single vertex" in {
    val vertices = IndexedSeq(Point(Array(0.0f, 0.0f), 0L))
    val edges    = Set.empty[(Int, Int)]
    val graph    = DelaunayGraph(vertices, edges)
    val numNodes = 1

    val assignments = KaHIPNodeDistributor.assignCellsToNodes(graph, numNodes)

    assignments.size should be(1)
    assignments(0) should be(0)
  }

  it should "handle a graph with no vertices" in {
    val vertices = IndexedSeq.empty[Point]
    val edges    = Set.empty[(Int, Int)]
    val graph    = DelaunayGraph(vertices, edges)
    val numNodes = 2

    val assignments = KaHIPNodeDistributor.assignCellsToNodes(graph, numNodes)

    assignments should be(empty)
  }

  it should "assign all vertices to a single partition when numNodes is 1" in {
    val vertices = IndexedSeq(
      Point(Array(0.0f, 0.0f), 0L),
      Point(Array(1.0f, 0.0f), 1L),
      Point(Array(0.0f, 1.0f), 2L),
      Point(Array(1.0f, 1.0f), 3L)
    )
    val edges = Set(
      (0, 1),
      (0, 2),
      (1, 3),
      (2, 3)
    )
    val graph    = DelaunayGraph(vertices, edges)
    val numNodes = 1

    val assignments = KaHIPNodeDistributor.assignCellsToNodes(graph, numNodes)

    assignments.size should be(vertices.size)
    assignments.values.toSet should be(Set(0))
  }

  it should "handle when numNodes is greater than the number of vertices" in {
    val vertices = IndexedSeq(
      Point(Array(0.0f, 0.0f), 0L),
      Point(Array(1.0f, 0.0f), 1L)
    )
    val edges    = Set((0, 1))
    val graph    = DelaunayGraph(vertices, edges)
    val numNodes = 4

    val assignments = KaHIPNodeDistributor.assignCellsToNodes(graph, numNodes)

    assignments.size should be(vertices.size)
    assignments.values.toSet.size should be <= numNodes
  }

  /** Check if nodes in a partition are connected in the graph. Uses BFS to verify connectivity.
    *
    * @param partition
    *   Set of node indices in the partition
    * @param graph
    *   DelaunayGraph containing the edges
    * @return
    *   true if all nodes in the partition are connected, false otherwise
    */
  def isPartitionConnected(partition: Set[Int], graph: DelaunayGraph): Boolean = {
    if (partition.isEmpty) return true

    val startNode = partition.head
    val visited   = mutable.Set[Int](startNode)
    val queue     = mutable.Queue[Int](startNode)

    while (queue.nonEmpty) {
      val current = queue.dequeue()

      // Find all neighbors of current node
      val neighbors = graph.edges.collect {
        case (a, b) if a == current && partition.contains(b) => b
        case (a, b) if b == current && partition.contains(a) => a
      }

      // Add unvisited neighbors to queue
      neighbors.foreach { neighbor =>
        if (!visited.contains(neighbor)) {
          visited.add(neighbor)
          queue.enqueue(neighbor)
        }
      }
    }

    // Check if all nodes in partition were visited
    visited.size == partition.size
  }

  /** Calculate the centroid of a set of points
    *
    * @param partition
    *   Set of node indices in the partition
    * @param points
    *   Array of points
    * @return
    *   Centroid as an array of coordinates
    */
  def calculateCentroid(partition: Set[Int], points: Array[Point]): Embedding = {
    val dimensions = points.head.vector.length
    val centroid   = new Embedding(dimensions)

    partition.foreach { idx =>
      val coords = points(idx).vector
      for (d <- 0 until dimensions)
        centroid(d) += coords(d)
    }

    for (d <- 0 until dimensions)
      centroid(d) /= partition.size

    centroid
  }

  /** Calculate Euclidean distance between two points */
  def distance(p1: Embedding, p2: Embedding): Float = {
    var sum = 0f
    for (i <- p1.indices) {
      val diff = p1(i) - p2(i)
      sum += diff * diff
    }
    Math.sqrt(sum).toFloat
  }

  it should "correctly partition a complex graph" in {
    val points = Array(
      Point(Array(130.24675736446898f, 242.48740426598965f), 178),
      Point(Array(425.2709374325392f, 29.027188442196802f), 208),
      Point(Array(398.11273064949f, 70.35230907715675f), 88),
      Point(Array(106.29540305945665f, 227.76121506886795f), 193),
      Point(Array(290.84146813678046f, 174.1070567757974f), 86),
      Point(Array(264.6741625707876f, 178.30070291331805f), 412),
      Point(Array(349.5480760372931f, 368.51416430279926f), 376),
      Point(Array(558.9308548771616f, 348.2944771527617f), 231),
      Point(Array(817.9877725413276f, 38.06860903334007f), 375),
      Point(Array(88.5128092522011f, 55.98229824098007f), 459),
      Point(Array(439.84076084930086f, 60.396397323153565f), 293),
      Point(Array(231.035680562924f, 154.77123705527129f), 297),
      Point(Array(931.7773139265933f, 24.90831608791541f), 184),
      Point(Array(97.01139487775988f, 117.88465112558612f), 341),
      Point(Array(122.86775458787974f, 294.67063163264595f), 104),
      Point(Array(926.4188717906366f, 333.37159226961455f), 391),
      Point(Array(607.9897266487642f, 306.14047683564036f), 290),
      Point(Array(924.60977786563f, 108.10524571895087f), 154),
      Point(Array(928.5918306070772f, 237.6173121135385f), 408),
      Point(Array(749.3225295355018f, 268.25590837133626f), 218),
      Point(Array(539.4265400900907f, 105.0640634941285f), 138),
      Point(Array(731.9033892635049f, 345.26067345758213f), 332),
      Point(Array(561.922355400692f, 169.20056793353598f), 451),
      Point(Array(772.1765637694886f, 134.22813696394985f), 15),
      Point(Array(299.9408171471196f, 337.8531607259982f), 363)
    )
    val graph    = DelaunayGraphBuilder.buildDelaunayGraph(points)
    val numNodes = 5

    val assignments = KaHIPNodeDistributor.assignCellsToNodes(graph, numNodes)

    assignments.size should be(points.length)
    assignments.values.toSet.size should be <= numNodes
    assignments.keys.toSet should be(points.indices.toSet)

    // Check that nodes within each partition are connected
    val partitions = assignments.groupBy(_._2).map { case (_, nodes) => nodes.keys.toSet }
    partitions.foreach { partition =>
      withClue(s"Partition with nodes $partition is not connected: ") {
        isPartitionConnected(partition, graph) should be(true)
      }
    }

    // Calculate centroids for each partition
    val centroids = partitions.map(partition => calculateCentroid(partition, points)).toSeq

    // Calculate data space dimensions to determine a reasonable minimum distance
    val xMin = points.map(_.vector(0)).min
    val xMax = points.map(_.vector(0)).max
    val yMin = points.map(_.vector(1)).min
    val yMax = points.map(_.vector(1)).max

    val spaceWidth    = xMax - xMin
    val spaceHeight   = yMax - yMin
    val spaceDiagonal = Math.sqrt(spaceWidth * spaceWidth + spaceHeight * spaceHeight).toFloat

    // Minimum expected distance between centroids - at least 15% of the space diagonal
    // This value can be adjusted based on the expected partition shapes
    val minCentroidDistance = spaceDiagonal * 0.15f

    // Check all pairs of centroids are far enough from each other
    for (i <- centroids.indices) {
      for (j <- i + 1 until centroids.size) {
        val dist = distance(centroids(i), centroids(j))
        withClue(s"Centroids of partitions at distance $dist are too close (min: $minCentroidDistance): ") {
          dist should be >= minCentroidDistance
        }
      }
    }

    /*val labels = points.indices.map{
      i => assignments(i)
    }.toArray

    new CSVWriter("kahip_partitioning.csv").write(points, labels)*/
  }

}
