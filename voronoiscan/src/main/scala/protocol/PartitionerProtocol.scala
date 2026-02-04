package protocol

import data.Point
import protocol.Message.{CborMessage, ProtobufMessage}

object PartitionerProtocol {

  sealed trait PartitionerRequest

  @SerialVersionUID(202506020006L)
  final case class PartitionPoints(points: Array[Point]) extends PartitionerRequest with ProtobufMessage

  @SerialVersionUID(202506020007L)
  final case class EndOfFile() extends PartitionerRequest with CborMessage

}
