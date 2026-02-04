package protocol

import data.LocalClusteringMerge
import protocol.Message.ProtobufMessage

object GlobalMergeProtocol {

  sealed trait GlobalMergeRequest extends ProtobufMessage

  @SerialVersionUID(202506020024L)
  final case class SendPairwiseMergeResult(localClusteringMerge: LocalClusteringMerge) extends GlobalMergeRequest

}
