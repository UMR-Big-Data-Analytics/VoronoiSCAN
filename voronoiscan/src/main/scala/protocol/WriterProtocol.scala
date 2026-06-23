package protocol

import akka.actor.typed.ActorRef
import configuration.InputConfiguration
import dto.ClusterMapDto
import protocol.GuardianProtocol.StopVoronoiSCAN
import protocol.Message.{CborMessage, ProtobufMessage}

object WriterProtocol {

  sealed trait WriterRequest

  @SerialVersionUID(202508260001L)
  final case class InitializeWriter(
      askRef: Option[ActorRef[StopVoronoiSCAN]],
      guardian: ActorRef[GuardianProtocol.GuardianRequest],
      parameters: InputConfiguration
                                   ) extends WriterRequest
    with CborMessage

  @SerialVersionUID(202511240002L)
  final case class ReportNumberOfCells(numCells: Int) extends WriterRequest with CborMessage

  @SerialVersionUID(202511240001L)
  final case class ReportNumberOfPoints(numPoints: Long) extends WriterRequest with CborMessage

  @SerialVersionUID(202508260005L)
  final case class LabeledPointsChunk(
                                       sourceCellIdx: Int,
                                       ids: Array[Long],
                                       labels: Array[Int],
                                       chunkIdx: Int,
                                       totalChunks: Int,
                                       replyTo: ActorRef[ClustererProtocol.ClustererRequest]
                                     ) extends WriterRequest
      with ProtobufMessage

  @SerialVersionUID(202508260003L)
  final case class StartLabeling(
                                  clusterMapDto: ClusterMapDto,
                                  clusterers: Array[ActorRef[ClustererProtocol.ClustererRequest]]
                                ) extends WriterRequest
    with CborMessage

  @SerialVersionUID(202508260004L)
  final case class ShutdownWriter() extends WriterRequest with CborMessage

  @SerialVersionUID(202508260006L)
  final case class LabelingStuckCheck() extends WriterRequest with CborMessage

  final val LabelingStuckKey = "labeling-stuck-check"

}
