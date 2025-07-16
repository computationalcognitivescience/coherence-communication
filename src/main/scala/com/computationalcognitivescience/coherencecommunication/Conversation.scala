package com.computationalcognitivescience.coherencecommunication

import com.computationalcognitivescience.coherencecommunication.ConversationData.ConversationData
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
    utterance = TruthValueAssignment.empty,
    restrictedOffer = None
  )
  def simulate(): ConversationData = simulateRound(initialInitiator, initialResponder)
  @tailrec
  private def simulateRound(
      initiator: Initiator,
      responder: Responder,
      restrictedOffer: Option[TruthValueAssignment] = None,
      data: ConversationData = List(preFirstRoundConversationData)
  ): ConversationData = {
    if (data.length > maxRounds) {
      // Stop conversation if it takes more than maxRounds
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
          utterance = TruthValueAssignment.empty,
          restrictedOffer = restrictedOffer
        ) +: data
      } else {
        val utterance: TruthValueAssignment =
          if (perceivedMutualUnderstanding == NotYet) {
            // No offer was given
            initiator.produceUtterance() // Figure Step 1
          } else {
            // Offer was given, not yet perceived mutual understanding
            val repairSolution = initiator.repairSolution(restrictedOffer.get) // Figure Step 6
            val utterance = initiator
              .addSharedBeliefs(repairSolution)
              .produceUtterance() // Figure Step 7
            repairSolution ++ utterance
          }
        val nextInitiator = initiator.addSharedBeliefs(utterance)
        val nextResponder = responder.addSharedBeliefs(utterance)
        val trouble       = nextResponder.troubleIdentification
        val restrictedOfferOption =
          if (trouble) nextResponder.repairFormulation
          else None
        val turnData = TurnData(
          initiatorState = nextInitiator,
          responderState = nextResponder,
          round = data.head.round + 1,
          initiatorPerceivedMutualUnderstanding = perceivedMutualUnderstanding,
          utterance = utterance,
          restrictedOffer = restrictedOfferOption
        )
        simulateRound(nextInitiator, nextResponder, restrictedOfferOption, turnData +: data)
      }
    }
  }

}
