package com.computationalcognitivescience.coherencecommunication

import com.computationalcognitivescience.coherencecommunication.Understandings._
import com.computationalcognitivescience.coherencecommunication.coherence.TruthValueAssignment

import scala.annotation.tailrec

case class Conversation(
    initialInitiator: Initiator,
    initialResponder: Responder,
    maxRounds: Int
) {

  private val preFirstRoundConversationData = TurnData(
    initiatorState = initialInitiator,
    responderState = initialResponder,
    round = 0,
    initiatorPerceivedMutualUnderstanding = Understandings.No,
    utterance = None,
    restrictedOffer = None
  )
  def simulate(): List[TurnData] = simulateRound(initialInitiator, initialResponder)
  @tailrec
  private def simulateRound(
      initiator: Initiator,
      responder: Responder,
      restrictedOffer: Option[TruthValueAssignment] = None,
      data: List[TurnData] = List(preFirstRoundConversationData)
  ): List[TurnData] = {
//    println("[Conversation.run] Round " + (data.length - 1))
    if (data.length > maxRounds) {
      // Stop conversation if it takes more than maxRounds
//      println("[Conversation.run] Max round " + maxRounds + " length reached.")
      data
    } else {
      // Figure Step 5 (or 0)
      val perceivedMutualUnderstanding = initiator.perceivedMutualUnderstanding(restrictedOffer)
      if (perceivedMutualUnderstanding == Yes) {
        // Conversation is finished.
        TurnData(
          initiatorState = initiator,
          responderState = responder,
          round = data.head.round + 1,
          initiatorPerceivedMutualUnderstanding = perceivedMutualUnderstanding,
          utterance = None,
          restrictedOffer = restrictedOffer
        ) +: data
      } else {
        val (utterance, nextInitiator) =
          if (perceivedMutualUnderstanding == NotYet) {
            // No offer was given, Figure Step 1
            initiator.produceUtterance()
          } else {
            // Offer was given, not yet perceived mutual understanding
            val reply: TruthValueAssignment =
              initiator.repairSolution(restrictedOffer.get) // Figure Step 6
            val (additionalUtterance, nextInitiator) = initiator.produceUtterance() // Figure Step 7
            (
              Some(reply ++ additionalUtterance.getOrElse(TruthValueAssignment.emtpy)),
              nextInitiator
            )
          }
        if (utterance.isEmpty) {
          // No reply or utterance was produced, end the conversation.
          TurnData(
            initiatorState = nextInitiator,
            responderState = responder,
            round = data.head.round + 1,
            initiatorPerceivedMutualUnderstanding = perceivedMutualUnderstanding,
            utterance = None,
            restrictedOffer = restrictedOffer
          ) +: data
        } else {
          val (trouble, nextResponder) = responder.troubleIdentification(utterance.get)
          val restrictedOfferOption =
            if (trouble) nextResponder.repairFormulation(utterance.get)
            else None
          val roundData = TurnData(
            initiatorState = nextInitiator,
            responderState = nextResponder,
            round = data.head.round + 1,
            initiatorPerceivedMutualUnderstanding = perceivedMutualUnderstanding,
            utterance = utterance,
            restrictedOffer = restrictedOfferOption
          )
          simulateRound(nextInitiator, nextResponder, restrictedOfferOption, roundData +: data)
        }
      }

//      // Start or continue conversation
//      val utterance: Map[Node[String], Boolean] =
//        if (repairRequest.isEmpty)
//          initiator.produceUtterance() // No repair request, produce utterance
//        else
//          initiator.repairSolution(
//            repairRequest.get
//          ) // Repair request made, produce repair solution
//
////      println("[Conversation.run] initiator says: " + utterance)
//      // Update the interlocutors
//      val updatedInitiator = initiator.addCommunicatedBeliefs(utterance)
//      val updatedResponder = responder.addCommunicatedBeliefs(utterance)
//////      println("[Conversation.run] "+updatedInitiator.inferredBeliefs.keySet.toList.sortBy(_.label).map(b => b.label + "i(" + updatedInitiator.inferredBeliefs(b) + ") r(" + updatedResponder.inferredBeliefs(b)+")").mkString(" "))
////      println(
//        "[Conversation.run] Initiator's communicated beliefs: " + updatedInitiator.sharedBeliefs
////      )
////      println(
//        "[Conversation.run] Responder's communicated beliefs: " + updatedResponder.sharedBeliefs
////      )
//
//      // See if responder has a repair request
//      val newRepairRequest = updatedResponder.troubleIdentification(responder)
//
//      val updatedConversationData = ConversationData(
//        initiatorState = updatedInitiator,
//        responderState = updatedResponder,
//        round = data.head.round + 1,
//        Some(utterance),
//        communicatedBeliefs = updatedInitiator.sharedBeliefs,
//        newRepairRequest,
//        utteranceLengthsInitiator = Some(utterance.size),
//        repairLengthsResponder = Some(newRepairRequest.size),
//      ) +: data
//
//      if (repairRequest.isDefined && initiator.endConversation(repairRequest)) {
////        println(s"Initiator believes that they are understood.")
//        // Stop conversation if the repair request makes the initiator belief the intention is understood
//        updatedConversationData
//      } else {
//        // Continue conversation
//        simulateRound(updatedInitiator, updatedResponder, newRepairRequest, updatedConversationData)
//      }
    }
  }
}
