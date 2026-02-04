package protocol

import serialization.{CborSerializable, ProtobufSerializable}

object Message {

  trait CborMessage extends CborSerializable

  trait ProtobufMessage extends ProtobufSerializable

}
