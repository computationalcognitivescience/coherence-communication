package com.computationalcognitivescience.coherencecommunication

import com.computationalcognitivescience.coherencecommunication.coherence.BeliefNetwork

case class ConversationData(
    initiatorNetwork: BeliefNetwork,
    responderNetwork: BeliefNetwork,
    round: Int,
    utteranceLengthsInitiator: Option[Int],
    utteranceLengthsResponder: Option[Int],
    similarityAllBeliefs: Int,
    similarityIntentionBeliefs: Int,
    similarityCommunicatedBeliefs: Int
)
