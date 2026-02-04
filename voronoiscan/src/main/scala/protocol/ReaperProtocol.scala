package protocol

import akka.actor.typed.ActorRef
import protocol.Message.CborMessage

object ReaperProtocol {

  sealed trait ReaperRequest extends CborMessage

  @SerialVersionUID(202506020003L)
  final case class WatchMeMessage[T](actorRef: ActorRef[T]) extends ReaperRequest

}
