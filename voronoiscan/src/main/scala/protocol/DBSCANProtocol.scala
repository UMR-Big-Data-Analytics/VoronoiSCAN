package protocol

import cluster.GridCell
import data.Point

sealed trait DBSCANMessage

@SerialVersionUID(202507020009L)
final case class MarkCoreTask(cell: GridCell, from: Int, until: Int, dense: Boolean, estimatedCost: Long)
    extends DBSCANMessage

@SerialVersionUID(202507020010L)
final case class MarkCoreResult(cell: GridCell, coreIds: Array[Long]) extends DBSCANMessage

@SerialVersionUID(202507020011L)
final case class ClusterCoreTask(cell: GridCell) extends DBSCANMessage

@SerialVersionUID(202507020012L)
final case class BorderTask(cell: GridCell) extends DBSCANMessage

@SerialVersionUID(202507020013L)
final case class BorderResult(assignments: Array[(Point, Int)]) extends DBSCANMessage
