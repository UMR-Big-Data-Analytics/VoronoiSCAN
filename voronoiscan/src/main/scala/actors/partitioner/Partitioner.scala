package actors.partitioner

import akka.actor.typed.scaladsl.{ActorContext, Behaviors}
import akka.actor.typed.{ActorRef, Behavior}
import data.{Point, VoronoiCell}
import protocol.ClustererProtocol.ClustererRequest
import protocol.OrchestratorProtocol.OrchestratorRequest
import protocol._
import spatial.index.KDTree
import utils.ArrayOps.NumericArrayOps
import utils.CpuTime

import java.time.Instant

object Partitioner {

  final val DEFAULT_NAME: String = "Partitioner"

  def apply(
      reader: ActorRef[ReaderProtocol.ReaderRequest],
      cells: Array[VoronoiCell],
      kDTree: KDTree,
      clusterers: Array[ActorRef[ClustererRequest]],
      stats: ActorRef[StatsProtocol.StatsRequest],
      orchestrator: ActorRef[OrchestratorRequest]
  ): Behavior[PartitionerProtocol.PartitionerRequest] =
    Behaviors.setup { context =>
      new Partitioner(context, reader, cells, kDTree, clusterers, stats, orchestrator).behavior
    }

}

class Partitioner private(
    context: ActorContext[PartitionerProtocol.PartitionerRequest],
    reader: ActorRef[ReaderProtocol.ReaderRequest],
    cells: Array[VoronoiCell],
    kDTree: KDTree,
    clusterers: Array[ActorRef[ClustererRequest]],
    stats: ActorRef[StatsProtocol.StatsRequest],
    orchestrator: ActorRef[OrchestratorRequest]
) {

  // Hash the centers to ints because map will use the array objects' reference equality
  // and we want to use the values of the centers for equality.
  private val centerToCellIdx = cells.map(_.center.hash()).zipWithIndex.toMap

  private var num = 0L

  private var sendNetworkPoints = 0L

  private var totalCpuTimeMs = 0L

  private val remoteClusterer = clusterers.zipWithIndex.map { case (ref, idx) =>
    (idx, ref.path.address.hasGlobalScope)
  }.toMap

  private def behavior: Behavior[PartitionerProtocol.PartitionerRequest] = {
    var phaseStart: Option[Instant] = None
    Behaviors.receiveMessage {
      case PartitionerProtocol.PartitionPoints(points) =>
        context.log.debug(s"Partitioner received a batch of size ${points.length}")
        if (phaseStart.isEmpty) {
          phaseStart = Some(Instant.now())
        }
        val startCpu = CpuTime.nowNanos
        num += points.length
        handleBatchAssignToVoronoiCell(points)
        val endCpu = CpuTime.nowNanos
        totalCpuTimeMs += CpuTime.toMillis(endCpu - startCpu)
        Behaviors.same
      case PartitionerProtocol.EndOfFile() =>
        context.log.debug(s"Partitioner received EndOfFile after processing $num points")
        val endTime = Instant.now()
        orchestrator ! OrchestratorProtocol.PartitionerFinished(num)
        stats ! StatsProtocol.ReportPartitioningTime(
          phaseStart.getOrElse(endTime), endTime, num, totalCpuTimeMs, sendNetworkPoints
        )
        Behaviors.stopped
      case _ => Behaviors.same
    }
  }

  private def handleBatchAssignToVoronoiCell(
      points: Array[Point]
  ): Behavior[PartitionerProtocol.PartitionerRequest] = {
    context.log.debug(s"RegularCellAssignment received a batch of size ${points.length}")
    val labels = assignLabels(points)
    labels.zip(points).groupBy(_._1).foreach { case (label, points) =>
      val pointsInCell = points.map(_._2).toArray
      clusterers(label) ! ClustererProtocol.AddPoints(pointsInCell)
      if (remoteClusterer(label)) {
        sendNetworkPoints += pointsInCell.length
      }
    }
    context.log.debug(s"RegularCellAssignment sent back the assigned batch")
    reader ! ReaderProtocol.RequestBatch(context.self)
    Behaviors.same
  }

  private def assignLabels(points: Array[Point]): Seq[Int] =
    points.map { point =>
      val (nearestCellCenter, distance) = kDTree.nearestNeighbor(point)
      point.distanceToCenter = distance
      centerToCellIdx(nearestCellCenter.vector.hash())
    }

}
