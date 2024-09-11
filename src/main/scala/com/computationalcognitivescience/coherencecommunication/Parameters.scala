package com.computationalcognitivescience.coherencecommunication

case class Parameters(
    id: Long,
    beliefNetworkSize: Int,
    beliefNetworkPCRatio: Double,
    intentionRatios: Double,
    initiatorPriorRatio: Double,
    responderPriorRatio: Double,
    maxUtteranceLength: Int,
    maxRoundLength: Int
)
