package protocol

import cluster.GridCell
import data.Point

import scala.collection.mutable

sealed trait DBSCANControllerMessage

@SerialVersionUID(202507020014L)
case object Start extends DBSCANControllerMessage

@SerialVersionUID(202507020015L)
final case class MarkCoreDone(coreCells: mutable.Set[GridCell], coreFlags: mutable.LongMap[Boolean])
    extends DBSCANControllerMessage

@SerialVersionUID(202507020016L)
final case class BorderDone(borderPointsMap: mutable.Map[Int, mutable.Set[Point]]) extends DBSCANControllerMessage

@SerialVersionUID(202507020017L)
final case class ClusterCoreDone(
    clusters: mutable.LongMap[Int],
    corePointsMap: mutable.Map[Int, mutable.Set[Point]],
    numClusters: Int
) extends DBSCANControllerMessage
