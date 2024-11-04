package com.computationalcognitivescience.coherencecommunication

import upickle.default.{macroRW, ReadWriter => RW}
case class Parameters(
    id: Long,
    beliefNetworkSize: Int,
    beliefNetworkConstraintsRatios: Double,
    beliefNetworkPCRatio: Double,
    intentionRatios: Double,
    initiatorPriorRatio: Double,
    responderPriorRatio: Double,
    priorsOverlapRatio: Double,
    priorsAsymmetryRatio: Double,
    maxUtteranceLength: Int,
    maxRoundLength: Int
)

object Parameters {
  implicit val rw: RW[Parameters] = macroRW
}