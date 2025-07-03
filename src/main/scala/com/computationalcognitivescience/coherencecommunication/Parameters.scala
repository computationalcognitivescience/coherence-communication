package com.computationalcognitivescience.coherencecommunication


case class Parameters(
    id: Long,
    beliefNetworkSize: Int,
    beliefNetworkConstraintsRatio: Double,
    beliefNetworkPCRatio: Double,
    initiatorCommunicativeIntentRatio: Double,
    initiatorPriorRatio: Double,
    responderPriorRatio: Double,
    priorsOverlapRatio: Double,
    priorsAsymmetryRatio: Double,
    maxUtteranceLength: Int,
    maxRoundLength: Int
)