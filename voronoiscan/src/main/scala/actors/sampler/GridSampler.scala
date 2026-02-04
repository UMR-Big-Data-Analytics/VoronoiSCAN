package actors.sampler

import data.{DelaunayGraph, Point}
import spatial.index.KDTree

import scala.io.Source

class GridSampler extends CellSampler {

  import GridSampler._

  override def getCenters(
                           filePath: String,
                           numSamples: Int,
                           minDistance: Float
                         ): (Array[Point], KDTree, DelaunayGraph) = {
    val boundingBox = computeBoundingBox(filePath)
    boundingBox.zipWithIndex.foreach { case ((min, max), dim) =>
      log.info(s"Dimension $dim: Min = $min, Max = $max, Span = ${max - min}")
    }
    val centers = createGrid(boundingBox, numSamples).zipWithIndex.map { case (coords, idx) =>
      // Add a little bit of noise to ensure planes are not coplanar while constructing Delaunay graph
      val coordsModified = coords
      Point(id = idx.toLong, vector = coordsModified.toArray)
    }.toArray
    val kdTree = new KDTree(centers).build()
    val graph = GridSampler.buildAdjacencyGraph(boundingBox, numSamples)
    (centers, kdTree, graph)
  }

}

object GridSampler {

  private val log = org.slf4j.LoggerFactory.getLogger(this.getClass)

  /** Generates an n-dimensional grid of points within a specific bounding box.
   *
   * To create a grid where the step size $s$ is uniform across all dimensions $d$ while satisfying a total target
   * number of points $N$, we proceed as follows:
   *
   * Let $L_i$ be the span (length) of dimension $i$. Let $k_i$ be the number of points along dimension $i$.
   * \begin{enumerate} \item **Uniform Spacing Constraint**: $$ L_i \approx k_i \cdot s \implies k_i = \frac{L_i}{s} $$
   * \item **Total Points Constraint**: $$ N = \prod_{i=1}^{n} k_i $$ \item **Substitution**: Substitute $k_i$ into the
   * product formula: $$ N = \prod_{i=1}^{n} \left( \frac{L_i}{s} \right) = \frac{\prod_{i=0}^n L_i}{s^n} =
   * \frac{\text{Volume}}{s^n} $$ \item **Solving for Spacing ($s$)**: $$ s^n = \frac{\text{Volume}}{N} \implies s =
   * \left( \frac{\text{Volume}}{N} \right)^{\frac{1}{n}} $$ \item **Solving for Points per Dimension ($k_i$)**:
   * Substituting $s$ back into the first equation: $$ k_i = L_i \cdot \frac{1}{s} = L_i \cdot \left(
   * \frac{N}{\text{Volume}} \right)^{\frac{1}{n}} $$ \end{enumerate}
   *
   * @param bounds
   * A sequence of (min, max) tuples representing the bounding box.
   * @param targetN
   * The desired total number of points.
   * @return
   * A sequence of points, where each point is a sequence of coordinates.
   */
  private def createGrid(bounds: Seq[(Float, Float)], targetN: Int): Seq[Seq[Float]] = {
    val nDims = bounds.size

    // 1. Calculate Spans (L_i)
    val spans = bounds.map { case (min, max) => max - min }

    // 2. Calculate Volume
    val volume = spans.product
    if (volume <= 0) throw new IllegalArgumentException("Bounding box volume must be positive.")

    // 3. Calculate Density Factor: (N / Volume)^(1/n)
    // This is equivalent to 1/s (inverse spacing)
    val density = math.pow(targetN.toFloat / volume, 1.0 / nDims)

    // 4. Calculate continuous points per dimension (k_i as double)
    val continuous = spans.map(span => span * density)

    // 5. Initial integer points per dimension (rounded)
    var pointsPerDim = continuous.map(c => math.max(1, math.round(c).toInt)).toArray

    // Cap any per-dimension count to targetN (can't have more points in one dim than total)
    pointsPerDim = pointsPerDim.map(p => math.min(p, targetN))

    var prod = pointsPerDim.product

    // Adjust counts if product doesn't match targetN.
    if (prod != targetN) {
      log.info(s"Initial continuous per-dim: ${continuous.mkString(", ")}")
      log.info(s"Initial integer per-dim (pre-adjust, capped): ${pointsPerDim.mkString(" x ")}")

      val maxIters = 100000
      var iters = 0

      while (prod > targetN && pointsPerDim.exists(_ > 1) && iters < maxIters) {
        val idx = pointsPerDim.zipWithIndex.filter(_._1 > 1).maxBy(_._1)._2
        pointsPerDim(idx) -= 1
        prod = pointsPerDim.product
        iters += 1
      }

      while (prod < targetN && iters < maxIters) {
        val candidates = pointsPerDim.zipWithIndex.map { case (p, i) =>
          val newProd = prod / p * (p + 1)
          val diff = math.abs(newProd - targetN)
          (diff, i, newProd)
        }
        val best = candidates.minBy(_._1)
        val bestIdx = best._2
        if (pointsPerDim(bestIdx) >= targetN) {
          iters = maxIters
        } else {
          pointsPerDim(bestIdx) += 1
          prod = pointsPerDim.product
          iters += 1
        }
      }

      if (iters >= maxIters) {
        log.warn(s"Reached iteration limit while adjusting grid dimensions (iters=$iters). Target: $targetN, Current product: $prod")
      }

      log.info(s"Adjusted integer per-dim: ${pointsPerDim.mkString(" x ")}")
      log.info(s"Theoretical Total: $targetN, Actual Total: $prod")
    } else {
      log.info(s"Calculated grid dimensions: ${pointsPerDim.mkString(" x ")}")
      log.info(s"Theoretical Total: $targetN, Actual Total: $prod")
    }

    // 6. Generate ranges (linspace) for each dimension
    val axes = bounds.zip(pointsPerDim).map { case ((min, max), count) =>
      linspace(min, max, count)
    }

    // 7. Cartesian Product to create the N-dimensional grid
    cartesianProduct(axes)
  }

  /** Generates `count` evenly spaced points between `start` and `end`. */
  private def linspace(start: Float, end: Float, count: Int): Seq[Float] = {
    if (count == 1) return Seq(start)
    val step = (end - start) / (count - 1)
    (0 until count).map(i => start + (i * step))
  }

  /** Computes the Cartesian product of an arbitrary number of sequences.
   *
   * Example: Input: Seq(Seq(1, 2), Seq(3, 4)) Output: Seq(Seq(1, 3), Seq(1, 4), Seq(2, 3), Seq(2, 4))
   */
  private def cartesianProduct[T](seqs: Seq[Seq[T]]): Seq[Seq[T]] = {
    seqs.foldLeft(Seq(Seq.empty[T])) { (acc, currentDim) =>
      for {
        existingPoint <- acc
        newCoord <- currentDim
      } yield existingPoint :+ newCoord
    }
  }

  private def computeBoundingBox(filePath: String): Seq[(Float, Float)] = {
    // Read points from file and compute min and max coordinates
    val source = Source.fromFile(filePath)
    try {
      val lines      = source.getLines()
      val firstPoint = lines.next().split(",")
      val dim        = firstPoint.length
      val minCoords  = Array.fill(dim)(Float.MaxValue)
      val maxCoords  = Array.fill(dim)(Float.MinValue)

      lines.foreach { line =>
        val coords = line.split(",").map(_.toFloat)
        for (i <- coords.indices) {
          if (coords(i) < minCoords(i)) minCoords(i) = coords(i)
          if (coords(i) > maxCoords(i)) maxCoords(i) = coords(i)
        }
      }
      minCoords.zip(maxCoords).toSeq
    } finally
      source.close()
  }

  /** Build an undirected graph whose vertices are the grid cell centers and whose edges connect adjacent grid cells
   * (differing by 1 in exactly one dimension).
   *
   * The graph uses the existing [[data.DelaunayGraph]] structure, with vertex indices matching the order of the
   * Cartesian product produced by [[createGrid]].
   */
  def buildAdjacencyGraph(bounds: Seq[(Float, Float)], targetN: Int): DelaunayGraph = {
    val centers: Seq[Seq[Float]] = createGrid(bounds, targetN)

    // Infer per-dimension counts (shape) from the way we constructed the grid.
    // We reuse the same logic as in createGrid but without logging/side effects
    // to obtain pointsPerDim deterministically.

    val nDims = bounds.size
    val spans = bounds.map { case (min, max) => max - min }
    val volume = spans.product
    if (volume <= 0) throw new IllegalArgumentException("Bounding box volume must be positive.")

    val density = math.pow(targetN.toFloat / volume, 1.0 / nDims)
    val continuous = spans.map(span => span * density)

    var pointsPerDim = continuous.map(c => math.max(1, math.round(c).toInt)).toArray
    pointsPerDim = pointsPerDim.map(p => math.min(p, targetN))

    var prod = pointsPerDim.product
    val maxIters = 100000
    var iters = 0

    while (prod > targetN && pointsPerDim.exists(_ > 1) && iters < maxIters) {
      val idx = pointsPerDim.zipWithIndex.filter(_._1 > 1).maxBy(_._1)._2
      pointsPerDim(idx) -= 1
      prod = pointsPerDim.product
      iters += 1
    }

    while (prod < targetN && iters < maxIters) {
      val candidates = pointsPerDim.zipWithIndex.map { case (p, i) =>
        val newProd = prod / p * (p + 1)
        val diff = math.abs(newProd - targetN)
        (diff, i, newProd)
      }
      val best = candidates.minBy(_._1)
      val bestIdx = best._2
      if (pointsPerDim(bestIdx) >= targetN) {
        iters = maxIters
      } else {
        pointsPerDim(bestIdx) += 1
        prod = pointsPerDim.product
        iters += 1
      }
    }

    val shape = pointsPerDim.toVector

    // Helpers for n-D indexing <-> linear index (row-major)
    def toLinear(coords: Vector[Int]): Int = {
      require(coords.length == shape.length)
      val strides = {
        val s = new Array[Int](shape.length)
        var acc = 1
        var d = shape.length - 1
        while (d >= 0) {
          s(d) = acc
          acc *= shape(d)
          d -= 1
        }
        s
      }
      var res = 0
      var i = 0
      while (i < shape.length) {
        val c = coords(i)
        require(c >= 0 && c < shape(i))
        res += c * strides(i)
        i += 1
      }
      res
    }

    def fromLinear(linear: Int): Vector[Int] = {
      require(linear >= 0 && linear < shape.product)
      val coords = new Array[Int](shape.length)
      var rem = linear
      var d = shape.length - 1
      while (d >= 0) {
        val base = shape(d)
        coords(d) = rem % base
        rem = rem / base
        d -= 1
      }
      coords.toVector
    }

    def neighborCoords(idx: Vector[Int]): Vector[Vector[Int]] = {
      val buf = Vector.newBuilder[Vector[Int]]
      var d = 0
      while (d < shape.length) {
        val c = idx(d)
        if (c > 0) {
          buf += idx.updated(d, c - 1)
        }
        if (c < shape(d) - 1) {
          buf += idx.updated(d, c + 1)
        }
        d += 1
      }
      buf.result()
    }

    val numVertices = centers.size

    // Build edges between adjacent cells, using linear indices as vertex ids
    val edgeSet = scala.collection.mutable.Set.empty[(Int, Int)]
    var i = 0
    while (i < numVertices) {
      val idx = fromLinear(i)
      val neighbors = neighborCoords(idx).map(toLinear)
      neighbors.foreach { j =>
        if (i < j) edgeSet += ((i, j)) else if (j < i) edgeSet += ((j, i))
      }
      i += 1
    }

    val vertices: Seq[Point] = centers.zipWithIndex.map { case (coords, vid) =>
      Point(id = vid.toLong, vector = coords.toArray)
    }

    DelaunayGraph(vertices, edgeSet.toSet)
  }

  def main(args: Array[String]): Unit = {
    // Dim 1: 0 to 3  (Span 3)
    // Dim 2: 0 to 10 (Span 10) -> Should have roughly 3.3x more points than Dim 1
    val bounds = Seq(
      (0.0f, 3.0f),
      (0.0f, 10.0f)
    )

    val targetPoints = 100
    val grid = createGrid(bounds, targetPoints)

    println(s"Generated ${grid.size} points.")
    println("First 5 points:")
    grid.take(5).foreach(println)
  }

}
