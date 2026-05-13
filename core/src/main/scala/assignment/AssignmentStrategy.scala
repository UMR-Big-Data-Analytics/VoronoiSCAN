package assignment

import data.Point
import data.VoronoiCell
import spatial.index.KDTree

trait AssignmentStrategy {

  def assign(cell: Array[VoronoiCell], points: Array[Point], kdTree: KDTree): VectorAssignment

}
