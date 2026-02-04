package protocol

import akka.actor.typed.ActorRef
import protocol.Message.CborMessage

object ReaderProtocol {

  sealed trait ReaderRequest extends CborMessage

  @SerialVersionUID(202506020004L)
  final case class ReadFile(filePath: String, replyTo: ActorRef[OrchestratorProtocol.OrchestratorRequest])
      extends ReaderRequest

  @SerialVersionUID(202506020005L)
  final case class RequestBatch(partitioner: ActorRef[PartitionerProtocol.PartitionerRequest]) extends ReaderRequest

}
