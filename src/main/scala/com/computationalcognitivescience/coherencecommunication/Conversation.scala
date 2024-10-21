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
    println("[Conversation.run] Round " + (data.length -1))
    if (data.length > maxRounds) {
      // Stop conversation if it takes more than maxRounds
      println("[Conversation.run] Max round "+maxRounds+" length reached.")
      data
    } else {
      // Start or continue conversation
      val utterance: Map[Node[String], Boolean] =
        if (repairRequest.isEmpty)
          initiator.produceUtterance() // No repair request, produce utterance
        else
          initiator.repairSolution(
            repairRequest.get
          ) // Repair request made, produce repair solution


      println("[Conversation.run] initiator says: "+ utterance)
      // Update the interlocutors
      val updatedInitiator = initiator.addCommunicatedBeliefs(utterance)
      val updatedResponder = responder.addCommunicatedBeliefs(utterance)
      println("[Conversation.run] "+updatedInitiator.inferredBeliefs.keySet.toList.sortBy(_.label).map(b => b.label + "i(" + updatedInitiator.inferredBeliefs(b) + ") r(" + updatedResponder.inferredBeliefs(b)+")").mkString(" "))

      // See if responder has a repair request
      val newRepairRequest = updatedResponder.troubleIdentification(responder)

      val updatedConversationData = ConversationData(
        initiatorState = updatedInitiator,
        responderState = updatedResponder,
        round = data.head.round + 1,
        Some(utterance),
        newRepairRequest,
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

      if (repairRequest.isDefined && initiator.endConversation(repairRequest)) {
        println(s"Initiator believes that they are understood.")
        // Stop conversation if the repair request makes the initiator belief the intention is understood
        updatedConversationData
      } else {
        // Continue conversation
        simulateRound(newRepairRequest, updatedConversationData)
      }
    }
  }
}
