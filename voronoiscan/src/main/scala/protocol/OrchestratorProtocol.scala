package protocol

import akka.actor.NoSerializationVerificationNeeded
import akka.actor.typed.ActorRef
import com.fasterxml.jackson.databind.annotation.{JsonDeserialize, JsonSerialize}
import configuration.InputConfiguration
import data._
import protocol.Message.{CborMessage, ProtobufMessage}
import spatial.index.KDTree

object OrchestratorProtocol {

  sealed trait OrchestratorRequest

  @SerialVersionUID(202506020008L)
  final case class ExecuteVoronoiSCAN(
      parameters: InputConfiguration,
      workers: Array[ActorRef[WorkerProtocol.WorkerRequest]]
  ) extends OrchestratorRequest
      with CborMessage

  @SerialVersionUID(202506020009L)
  final case class EpsilonMergeCompleted() extends OrchestratorRequest with CborMessage

  @SerialVersionUID(202506020010L)
  final case class ExecutedDBSCAN(
      actorRef: ActorRef[ClustererProtocol.ClustererRequest],
      cellIdx: Int,
      numClusters: Int
  ) extends OrchestratorRequest
      with CborMessage

  @SerialVersionUID(202506020011L)
  final case class EpsilonExtensionCompleted() extends OrchestratorRequest with CborMessage

  @SerialVersionUID(202506020012L)
  final case class DistributedVoronoiCellNeighbors() extends OrchestratorRequest with CborMessage

  @SerialVersionUID(202506020013L)
  final case class ExtractedSamples(samples: Array[Point], kdTree: KDTree, graph: DelaunayGraph)
      extends OrchestratorRequest
        with CborMessage
        with NoSerializationVerificationNeeded

  @SerialVersionUID(202506020014L)
  final case class FinishedGlobalMerge(globalMergeResult: Map[(Int, Int), Int])
      extends OrchestratorRequest
      with ProtobufMessage

  @SerialVersionUID(202507070001L)
  final case class SpawnedWorker(
      clusterer: Array[ActorRef[ClustererProtocol.ClustererRequest]],
      @JsonSerialize(keyUsing = classOf[EdgeKeySerializer])
      @JsonDeserialize(keyUsing = classOf[EdgeKeyDeserializer])
      epsilonMerger: Map[Edge[Int, Vertex[Int]], ActorRef[EpsilonMergeProtocol.EpsilonMergeRequest]]
  ) extends OrchestratorRequest
      with CborMessage

  @SerialVersionUID(202507070002L)
  case class PartitionerFinished(numPartitionedPoints: Long) extends OrchestratorRequest with CborMessage

  @SerialVersionUID(202507070003L)
  case class ReaderInitialized() extends OrchestratorRequest with CborMessage

  @SerialVersionUID(202508280004L)
  case class Shutdown() extends OrchestratorRequest with CborMessage

}
