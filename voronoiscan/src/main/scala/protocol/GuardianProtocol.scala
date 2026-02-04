package protocol

import akka.actor.typed.ActorRef
import akka.actor.typed.receptionist.Receptionist.Listing
import protocol.Message.{CborMessage, ProtobufMessage}

object GuardianProtocol {

  sealed trait GuardianRequest

  sealed trait GuardianResponse extends GuardianRequest

  @SerialVersionUID(202506030025L)
  final case class ShutdownMessage(initiator: Option[ActorRef[GuardianRequest]] = None)
      extends GuardianRequest
      with CborMessage

  @SerialVersionUID(202506020019L)
  final case class ExecuteVoronoiSCAN(ref: Option[ActorRef[StopVoronoiSCAN]] = None)
      extends GuardianRequest
      with CborMessage

  @SerialVersionUID(202506030026L)
  final case class ReceptionistListingMessage(listing: Listing) extends GuardianRequest with CborMessage

  @SerialVersionUID(202506020022L)
  final case class StopVoronoiSCAN(labels: Option[Array[Int]]) extends GuardianResponse with ProtobufMessage

}
