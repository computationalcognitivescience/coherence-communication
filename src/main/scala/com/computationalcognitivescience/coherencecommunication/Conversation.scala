package com.computationalcognitivescience.coherencecommunication

import mathlib.graph.Node

case class Conversation(
    initiator: Initiator,
    responder: Responder,
    data: ConversationData,
    maxRounds: Int
) {

  def simulateRound(
      repairRequest: Option[Map[Node[String], Boolean]] = None
  ): ConversationData = {
    if (data.round > maxRounds) data // Stop conversation if it takes more than maxRounds

    if (repairRequest.isEmpty) data // Stop conversation if no repair request was made
    else {
      // Stop if repair request was made and
      // the initiator believes the responder inferred the intention
      if (initiator.endConversation(repairRequest.get)) data
      else {
        val utterance        = initiator.repairSolution(repairRequest.get)
        val updatedInitiator = initiator.addCommunicatedBeliefs(utterance)
        val updatedResponder = responder.addCommunicatedBeliefs(utterance)
        val newRepairRequest = updatedResponder.troubleIdentification(responder)
        simulateRound(Some(newRepairRequest))
      }
    }
  }
}

case object Conversation {
  def init(initiator: Initiator, responder: Responder, maxRounds: Int): Conversation =
    Conversation(
      initiator,
      responder,
      ConversationData.init(initiator.beliefNetwork, responder.beliefNetwork),
      maxRounds
    )
}
