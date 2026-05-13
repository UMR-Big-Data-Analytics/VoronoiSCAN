package actors.sampler

import actors.sampler.SampleHelper.{getPoints, getRandomSamples}
import configuration.{InputConfiguration, SystemConfiguration}
import data.{DelaunayGraph, Point}
import delaunay.DelaunayGraphBuilder
import spatial.index.KDTree
import utils.Utils

class RandomSampler(seed: Option[Int] = None, inputConfiguration: InputConfiguration) extends CellSampler {

  override def getCenters(
                           filePath: String,
                           numSamples: Int,
                           minDistance: Float
                         ): (Array[Point], KDTree, DelaunayGraph) = {
    val lines = getRandomSamples(filePath, 500 * numSamples, seed)
    extractSamples(lines, numSamples, minDistance)
  }

  private def extractSamples(
                              lines: Array[String],
                              numSamples: Int,
                              minDistance: Float
                            ): (Array[Point], KDTree, DelaunayGraph) = {
    val numWorkers = SystemConfiguration.get.numWorkers
    val points = getPoints(lines)
    require(points.nonEmpty, "Cannot extract centers from an empty point set")

    val qhullMinCells = points.head.dim + 1
    val requiredCells = math.max(numWorkers, qhullMinCells)
    val effectiveNumSamples = math.max(numSamples, requiredCells)

    var iterationCounter = 0
    var bestResult: Option[(Array[Point], KDTree, DelaunayGraph)] = None

    while (iterationCounter < 10) {
      iterationCounter += 1
      val (rawCenters, _) = Utils.getSeedPoints(points, effectiveNumSamples, minDistance)

      if (rawCenters.length < qhullMinCells) {
        // qhull needs at least (dimension + 1) points to construct a simplex.
      } else {
        val rawGraph = DelaunayGraphBuilder.buildDelaunayGraph(rawCenters)

        val (centers, tree, graph) = CellContractor.mergeCloseCells(
          rawCenters,
          rawGraph,
          inputConfiguration.minCellDistanceFactor * inputConfiguration.epsilon
        )
        bestResult match {
          case Some((bestCenter, _, _)) =>
            if (centers.length > bestCenter.length) {
              bestResult = Some((centers, tree, graph))
            }
          case None => bestResult = Some((centers, tree, graph))
        }
        if (centers.length >= requiredCells) {
          return (centers, tree, graph)
        }
      }
    }

    iterationCounter = 0
    var minCellDistance = inputConfiguration.minCellDistanceFactor * inputConfiguration.epsilon
    while (iterationCounter < 5) {
      iterationCounter += 1
      val (bestCenters, bestTree, bestGraph) =
        bestResult.getOrElse(throw new RuntimeException("This should never happen"))
      minCellDistance = minCellDistance * 0.95f
      if (minCellDistance <= 2 * inputConfiguration.epsilon) {
        return (bestCenters, bestTree, bestGraph)
      }
      val (centers, tree, graph) = CellContractor.mergeCloseCells(
        bestCenters,
        bestGraph,
        minCellDistance
      )
      if (centers.length >= requiredCells) {
        return (centers, tree, graph)
      }
    }

    val bestCount = bestResult.map(_._1.length).getOrElse(0)
    throw new RuntimeException(
      s"Failed to produce enough cells for qhull/workers: required=$requiredCells (qhullMin=$qhullMinCells, workers=$numWorkers), best=$bestCount"
    )
  }

}
