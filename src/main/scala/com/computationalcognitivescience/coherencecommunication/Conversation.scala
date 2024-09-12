package com.computationalcognitivescience.coherencecommunication

import mathlib.graph.Node

import scala.annotation.tailrec

case class Conversation(
    initiator: Initiator,
    responder: Responder,
    maxRounds: Int
) {

  private val preFirstRoundConversationData = ConversationData(
    initiatorState = initiator,
    responderState = responder,
    round = 0,
    utteranceLengthsInitiator = None,
    repairLengthsResponder = None,
    utterance = None,
    repair = None,
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
    // Stop conversation if it takes more than maxRounds
    if (data.length > maxRounds) data
    // Stop conversation if no repair request was made after the first round
    else if (repairRequest.isEmpty && data.length > 1) data
    // Stop conversation if the repair request makes the initiator belief the intention is understood
    else if (repairRequest.isDefined && initiator.endConversation(repairRequest.get)) data
    // Start or continue conversation
    else {
      val utterance: Map[Node[String], Boolean] =
        if(repairRequest.isEmpty)
          initiator.produceUtterance()                // First round, no repair produce initial utterance
        else
          initiator.repairSolution(repairRequest.get) // Subsequent rounds, produce reply to repair request

        val updatedInitiator = initiator.addCommunicatedBeliefs(utterance)
        val updatedResponder = responder.addCommunicatedBeliefs(utterance)
        val newRepairRequest = updatedResponder.troubleIdentification(responder)
        val updatedConversationData = ConversationData(
          initiatorState = updatedInitiator,
          responderState = updatedResponder,
          round = data.last.round + 1,
          Some(utterance),
          Some(newRepairRequest),
          utteranceLengthsInitiator = Some(utterance.size),
          repairLengthsResponder = Some(newRepairRequest.size),
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
