package com.computationalcognitivescience.coherencecommunication

import mathlib.graph.Node

import scala.annotation.tailrec

case class Conversation(
    initiator: Initiator,
    responder: Responder,
    maxRounds: Int
) {

  private val preFirstRoundConversationData = ConversationData(
    initiatorNetwork = initiator.beliefNetwork,
    responderNetwork = responder.beliefNetwork,
    round = 0,
    utteranceLengthsInitiator = None,
    utteranceLengthsResponder = None,
    similarityAllBeliefs = initiator.structuralSimilarity(responder),
    similarityIntentionBeliefs =
      initiator.structuralSimilarity(responder, initiator.communicativeIntent.keySet),
    similarityCommunicatedBeliefs =
      initiator.structuralSimilarity(responder, initiator.communicatedBeliefs.keySet)
  )
  def simulate(): Seq[ConversationData] = simulateRound()
  @tailrec
  private def simulateRound(
      repairRequest: Option[Map[Node[String], Boolean]] = None,
      data: Seq[ConversationData] = Seq(preFirstRoundConversationData)
  ): Seq[ConversationData] = {
    if (data.length > maxRounds) data    // Stop conversation if it takes more than maxRounds
    else if (repairRequest.isEmpty) data // Stop conversation if no repair request was made
    else {
      // Stop if repair request was made and
      // the initiator believes the responder inferred the intention
      if (initiator.endConversation(repairRequest.get)) data
      else {
        val utterance        = initiator.repairSolution(repairRequest.get)
        val updatedInitiator = initiator.addCommunicatedBeliefs(utterance)
        val updatedResponder = responder.addCommunicatedBeliefs(utterance)
        val newRepairRequest = updatedResponder.troubleIdentification(responder)
        val updatedConversationData = ConversationData(
          initiatorNetwork = initiator.beliefNetwork,
          responderNetwork = responder.beliefNetwork,
          round = data.last.round + 1,
          utteranceLengthsInitiator = Some(utterance.size),
          utteranceLengthsResponder = Some(newRepairRequest.size),
          similarityAllBeliefs = updatedInitiator.structuralSimilarity(updatedResponder),
          similarityIntentionBeliefs = updatedInitiator
            .structuralSimilarity(updatedResponder, updatedInitiator.communicativeIntent.keySet),
          similarityCommunicatedBeliefs = updatedInitiator.structuralSimilarity(
            updatedResponder,
            updatedInitiator.communicatedBeliefs.keySet
          )
        ) +: data
        simulateRound(Some(newRepairRequest), updatedConversationData)
      }
    }
  }
}
