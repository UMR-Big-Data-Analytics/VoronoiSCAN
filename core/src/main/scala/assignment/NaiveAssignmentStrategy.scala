package assignment

import data.{Point, VoronoiCell}
import spatial.index.KDTree

class NaiveAssignmentStrategy extends AssignmentStrategy {

  override def assign(cell: Array[VoronoiCell], points: Array[Point], kdTree: KDTree): VectorAssignment = {
    val nCells           = cell.length
    val vectorAssignment = new VectorAssignment(nCells)
    for (point <- points) {
      for (i <- cell.indices) {
        val exCell = cell(i).extendedVoronoiCell
        if (exCell.contains(point)) {
          vectorAssignment.assign(point, i)
        }
      }

    }
    vectorAssignment
  }

}
