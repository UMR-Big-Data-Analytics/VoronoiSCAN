package actors.clusterer

import akka.actor.typed.{ActorRef, Behavior}
import akka.actor.typed.scaladsl.Behaviors
import data.VoronoiCell
import protocol.ClustererProtocol.ClustererRequest
import protocol.Message.CborMessage
import protocol.{ClustererProtocol, OrchestratorProtocol}
import protocol.OrchestratorProtocol.OrchestratorRequest

import scala.collection.mutable

object EpsilonExtensionManager {

  final val DEFAULT_NAME: String = "EpsilonExtensionManager"

  def apply(
      orchestrator: ActorRef[OrchestratorRequest],
      cells: Array[VoronoiCell]
  ): Behavior[EpsilonExtensionManagerRequest] =
    new EpsilonExtensionManager(orchestrator, cells).behavior

  sealed trait EpsilonExtensionManagerRequest extends CborMessage

  case class IterationDone(hasStolenPoints: Boolean) extends EpsilonExtensionManagerRequest

  case class PartitionedPoints() extends EpsilonExtensionManagerRequest

  case class RegisterEpsilonExtensionActor(actorRef: ActorRef[ClustererRequest], cellIdx: Int)
      extends EpsilonExtensionManagerRequest

}

class EpsilonExtensionManager(orchestrator: ActorRef[OrchestratorRequest], cells: Array[VoronoiCell]) {

  import EpsilonExtensionManager._

  private val clustererActors: mutable.Map[Int, ActorRef[ClustererRequest]] = mutable.Map.empty

  private var numAcknowledgements = 0

  private var pointStolen = false

  def behavior: Behavior[EpsilonExtensionManagerRequest] =
    Behaviors.setup[EpsilonExtensionManagerRequest] { context =>
      Behaviors.receiveMessage {
        case RegisterEpsilonExtensionActor(ref, cellIdx) =>
          // Register the epsilon extension actor for the corresponding cell index
          clustererActors(cellIdx) = ref
          if (clustererActors.size == cells.length) {
            exchangeClustererActors()
          }
          Behaviors.same
        case PartitionedPoints() =>
          // Start the first iteration
          for (epsilonExtension <- clustererActors.values)
            epsilonExtension ! ClustererProtocol.StartIteration()
          Behaviors.same
        case IterationDone(hasStolenPoints) =>
          numAcknowledgements += 1
          pointStolen ||= hasStolenPoints
          if (numAcknowledgements == cells.length) {
            if (pointStolen) {
              pointStolen = false
              numAcknowledgements = 0
              for (epsilonExtension <- clustererActors.values)
                epsilonExtension ! ClustererProtocol.StartIteration()
              Behaviors.same
            } else {
              orchestrator ! OrchestratorProtocol.EpsilonExtensionCompleted()
              Behaviors.stopped
            }
          } else {
            Behaviors.same
          }
      }
    }

  private def exchangeClustererActors(): Unit = {
    // Check validity of the actor references
    require(
      cells.length == clustererActors.size,
      s"Number of cells (${cells.length}) must match number of epsilon extension actors (${clustererActors.size})"
    )
    clustererActors.foreach { case (cellIdx, ref) =>
      val refId = ref.path.name.split("-").last.toInt
      require(refId == cellIdx, s"Actor reference ID ($refId) must match cell index ($cellIdx)")
    }

    // Map each cell index to a map of its neighbor indices and their corresponding epsilon extension actor references
    val refs = cells.map { cell =>
      cell.idx -> cell.getNeighbors.flatMap { neighbor =>
        clustererActors.get(neighbor.idx).map((neighbor.idx, _))
      }.toArray
    }.toMap
    for (cell <- cells) {
      val cellIdx      = cell.idx
      val neighborRefs = refs(cellIdx)
      val actorRef     = clustererActors(cellIdx)
      actorRef ! ClustererProtocol.ExchangeClustererActors(neighborRefs)
    }
  }

}
