package com.computationalcognitivescience.coherencecommunication

import io.circe.{Decoder, Encoder}

object Understandings extends Enumeration {
  type Understanding = Value

  val YesConfirmed, YesLiteral, YesPerceived, NoPerceived, No = Value

  implicit val genderDecoder: Decoder[Understandings.Value] =
    Decoder.decodeEnumeration(Understandings)
  implicit val genderEncoder: Encoder[Understandings.Value] =
    Encoder.encodeEnumeration(Understandings)
}
