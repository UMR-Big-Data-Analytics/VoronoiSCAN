package protocol

import protocol.Message.CborMessage

object SamplerProtocol {

  sealed trait SamplerRequest extends CborMessage

  @SerialVersionUID(202506020002L)
  final case class ExtractSample(numSamples: Int, minDistance: Float, filePath: String) extends SamplerRequest

}
