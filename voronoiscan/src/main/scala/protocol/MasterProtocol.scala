package protocol

import akka.actor.typed.ActorRef
import akka.actor.typed.receptionist.Receptionist
import configuration.InputConfiguration
import protocol.GuardianProtocol.StopVoronoiSCAN
import protocol.Message.CborMessage

object MasterProtocol {

  sealed trait MasterRequest

  @SerialVersionUID(202506020017L)
  final case class StartMessage(ref: Option[ActorRef[StopVoronoiSCAN]], parameters: InputConfiguration)
      extends MasterRequest
      with CborMessage

  @SerialVersionUID(202506020018L)
  final case class ShutdownMessage() extends MasterRequest with CborMessage

  case class ReceptionistListingMessage(listing: Receptionist.Listing) extends MasterRequest with CborMessage

}
