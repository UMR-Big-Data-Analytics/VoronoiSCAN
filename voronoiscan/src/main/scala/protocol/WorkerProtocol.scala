package protocol

import actors.clusterer.EpsilonExtensionManager.EpsilonExtensionManagerRequest
import akka.actor.typed.ActorRef
import dto.VoronoiCellCollectionDto
import protocol.Message.CborMessage

object WorkerProtocol {

  sealed trait WorkerRequest extends CborMessage

  @SerialVersionUID(202507070001L)
  final case class SpawnWorker(
      replyTo: ActorRef[OrchestratorProtocol.OrchestratorRequest],
      cellsDto: VoronoiCellCollectionDto,
      epsilonMergeEdges: Array[(Int, Int)],
      epsilon: Float,
      minPts: Int,
      dbscanImpl: String,
      globalMerger: ActorRef[GlobalMergeProtocol.GlobalMergeRequest],
      orchestrator: ActorRef[OrchestratorProtocol.OrchestratorRequest],
      epsilonMergeManager: ActorRef[EpsilonExtensionManagerRequest],
      stats: ActorRef[StatsProtocol.StatsRequest],
      writer: ActorRef[WriterProtocol.WriterRequest]
  ) extends WorkerRequest

  @SerialVersionUID(202506020001L)
  final case class ShutdownMessage() extends WorkerRequest

}
