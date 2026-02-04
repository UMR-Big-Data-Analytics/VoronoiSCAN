package protocol

import akka.actor.typed.ActorRef
import data.Point
import dto.ClusterMapDto
import protocol.Message.{CborMessage, ProtobufMessage}

object ClustererProtocol {

  sealed trait ClustererRequest

  @SerialVersionUID(202507220001L)
  final case class AddPoints(points: Array[Point]) extends ClustererRequest with ProtobufMessage

  @SerialVersionUID(202507220002L)
  case class DistributePoints(points: Array[Point], senderIdx: Int, replyTo: ActorRef[ClustererRequest])
      extends ClustererRequest
      with ProtobufMessage

  @SerialVersionUID(202507220003L)
  case class DistributedPoints() extends ClustererRequest with CborMessage

  @SerialVersionUID(202507220004L)
  case class StartIteration() extends ClustererRequest with CborMessage

  @SerialVersionUID(202507220005L)
  case class ExchangeClustererActors(refs: Array[(Int, ActorRef[ClustererRequest])])
      extends ClustererRequest
      with CborMessage

  @SerialVersionUID(202507220006L)
  case class ExecuteDBSCAN() extends ClustererRequest with CborMessage

  case class SendClusteringResultToEpsilonMerger(
      actorRef: ActorRef[EpsilonMergeProtocol.EpsilonMergeRequest],
      otherCellIdx: Int
  ) extends ClustererRequest
      with CborMessage

  @SerialVersionUID(202507220007L)
  case class LabelPoints(clusterMapDto: ClusterMapDto) extends ClustererRequest with ProtobufMessage

}
