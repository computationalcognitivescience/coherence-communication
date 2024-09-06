package com.computationalcognitivescience.coherencecommunication

import com.computationalcognitivescience.coherencecommunication.coherence.BeliefNetwork

case class ConversationData(
    initiatorNetwork: BeliefNetwork,
    responderNetwork: BeliefNetwork,
    round: Int,
    utteranceLengthsInitiator: Map[Int, Int],
    utteranceLengthsResponder: Map[Int, Int],
    similarityIntention: Double,
    similarityInferredBeliefs: Double,
    similarityCommunicatedBeliefs: Double
)

case object ConversationData {
  def init(initiatorNetwork: BeliefNetwork, responderNetwork: BeliefNetwork): ConversationData =
    ConversationData(
      initiatorNetwork,
      responderNetwork,
      round = 0,
      utteranceLengthsInitiator = Map.empty,
      utteranceLengthsResponder = Map.empty,
      similarityIntention = -1,
      similarityInferredBeliefs = -1,
      similarityCommunicatedBeliefs = -1
    )
}
