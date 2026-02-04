package protocol

import dto.LocalDBSCANResultDto
import protocol.Message.ProtobufMessage

object EpsilonMergeProtocol {

  sealed trait EpsilonMergeRequest

  @SerialVersionUID(202506020025L)
  final case class PushDBSCANResult(clusteringResult: LocalDBSCANResultDto)
      extends EpsilonMergeRequest
      with ProtobufMessage

  @SerialVersionUID(202508190001L)
  final case class PushDBSCANResultChunk(chunk: Array[Byte], chunkIdx: Int, totalChunks: Int, cellIdx: Int)
      extends EpsilonMergeRequest
      with ProtobufMessage

}
