package utils

import data.Point.Embedding
import data.{DelaunayGraph, Point, VoronoiCell}
import spatial.index.KDTree
import utils.Distances.euclideanDistance

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

    /*val random           = new Random(42)
    val idx              = (random.nextDouble() * pointsWithIds.length).toInt
    val firstPoint       = pointsWithIds(idx)
    val kdTree           = new KDTree(Array(firstPoint)).build()
    val selectedPoints   = mutable.Set(firstPoint)
    var iterationCounter = 0

    while (selectedPoints.size < n) {
      if (iterationCounter > 1000) {
        logger.warn(s"Could not find $n points with min distance $d after $iterationCounter iterations. Falling back to multi-restart farthest point sampling (will relax constraint if infeasible).")
        return farthestPointSeedsMulti(pointsWithIds, n, d, restarts = 8)
      }
      val pointIdx      = (random.nextDouble() * pointsWithIds.length).toInt
      val point         = pointsWithIds(pointIdx)
      val (_, distance) = kdTree.nearestNeighbor(point)
      if (distance > d) {
        selectedPoints += point
        kdTree.insert(point)
      }
      iterationCounter += 1
    }*/
    (selectedPoints.toArray, kdTree)
  }

  /** Multi-restart wrapper around farthest point sampling. Tries several random initial seeds; if any run achieves min
    * pairwise distance >= d it is returned immediately. Otherwise returns the run with the largest achieved minimum
    * distance (closest to target) and logs a warning.
    */
  def farthestPointSeedsMulti(
      points: Array[Point],
      n: Int,
      d: Float,
      restarts: Int,
      abortIfUnderD: Boolean = false
  ): (Array[Point], KDTree) = {
    require(restarts >= 1, "restarts must be >= 1")

    var bestCenters: Array[Point] = null
    var bestAchieved: Float       = Float.NegativeInfinity

    var r = 0
    while (r < restarts) {
      val (centers, achieved) = farthestPointSeedsWithAchieved(points, n, d, abortIfUnderD = false)
      if (achieved >= d) {
        logger.info(
          s"Multi-restart farthest sampling succeeded on restart #${r + 1} achieving min distance $achieved >= $d"
        )
        return (centers, new KDTree(centers).build())
      }
      if (achieved > bestAchieved) { bestAchieved = achieved; bestCenters = centers }
      r += 1
    }

    logger.warn(
      s"Requested min distance $d not achievable after $restarts restarts. Best achieved minimum distance = $bestAchieved (gap=${d - bestAchieved}). Returning best attempt."
    )
    (bestCenters, new KDTree(bestCenters).build())
  }

  // Internal variant returning also achieved minimum distance
  private def farthestPointSeedsWithAchieved(
      points: Array[Point],
      n: Int,
      d: Float,
      abortIfUnderD: Boolean
  ): (Array[Point], Float) = {
    val (centers, _) = farthestPointSeeds(points, n, d, abortIfUnderD)
    // Compute achieved minimum distance explicitly
    val m         = centers.length
    var i         = 0
    var globalMin = Float.PositiveInfinity
    while (i < m) {
      var j = i + 1
      while (j < m) {
        val dist = euclideanDistance(centers(i).vector, centers(j).vector)
        if (dist < globalMin) globalMin = dist
        j += 1
      }
      i += 1
    }
    if (globalMin.isPosInfinity) globalMin = 0f
    (centers, globalMin)
  }

  def farthestPointSeeds(
      points: Array[Point],
      n: Int,
      d: Float,
      abortIfUnderD: Boolean = false
  ): (Array[Point], KDTree) = {
    require(points.nonEmpty, "Empty points")
    require(n <= points.length, "n > points.length")

    val rnd      = new Random(42)
    val firstIdx = rnd.nextInt(points.length)
    val selected = Array.ofDim[Point](n)
    selected(0) = points(firstIdx)

    // Maintain minimal distance to any selected point
    val minDist = Array.fill(points.length)(Float.PositiveInfinity)

    // Track the minimum achieved pairwise distance among selected centers.
    var achievedMinDistance = Float.PositiveInfinity

    // Update distances after adding a new center
    def updateDistances(newP: Point): Unit = {
      var i = 0
      while (i < points.length) {
        val candidate = points(i)
        val dcur      = euclideanDistance(candidate.vector, newP.vector)
        if (dcur < minDist(i)) minDist(i) = dcur
        i += 1
      }
    }

    updateDistances(selected(0))

    var k = 1
    while (k < n) {
      // Find the farthest remaining point
      var bestIdx = -1
      var bestVal = -1.0f
      var i       = 0
      while (i < points.length) {
        val v = minDist(i)
        if (v > bestVal) { bestVal = v; bestIdx = i }
        i += 1
      }

      // bestVal is the distance of the chosen point to its nearest already-selected center
      if (bestVal < achievedMinDistance) achievedMinDistance = bestVal

      if (abortIfUnderD && bestVal < d) {
        throw new IllegalStateException(s"Infeasible: cannot maintain distance d=$d. Best achievable=$bestVal")
      }

      if (bestVal < d && !abortIfUnderD) {
        // We continue filling but log once (when first violation detected)
        if (achievedMinDistance < d && k == 1) { // first violation happens on first addition beyond initial point
          logger.warn(s"Farthest point sampling relaxing constraint: requested min distance $d but current best achievable for next point is $bestVal. Will proceed; final achieved min distance may be lower.")
        }
      }

      selected(k) = points(bestIdx)
      updateDistances(selected(k))
      k += 1
    }

    // After filling, compute actual achieved minimum distance between selected centers.
    // achievedMinDistance currently stores the minimum over insertion distances; ensure it's updated for completeness
    // (already tracked during loop). If still Infinity (n == 1), set to 0.
    if (achievedMinDistance.isPosInfinity) achievedMinDistance = 0f

    if (achievedMinDistance < d) {
      logger.warn(s"Requested min distance $d could not be achieved; final achieved minimum pairwise distance among ${selected.length} centers is $achievedMinDistance.")
    } else {
      logger.info(s"Achieved requested min distance $d with minimum spacing $achievedMinDistance among ${selected.length} centers.")
    }

    val kd = new KDTree(selected).build()
    (selected, kd)
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
