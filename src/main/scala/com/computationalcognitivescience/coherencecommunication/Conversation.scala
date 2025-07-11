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
    utterance = None,
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
        val (reply, utterance) =
          if (perceivedMutualUnderstanding == NotYet) {
            // No offer was given
            (TruthValueAssignment.emtpy, initiator.produceUtterance()) // Figure Step 1
          } else {
            // Offer was given, not yet perceived mutual understanding
            val repairSolution = initiator.repairSolution(restrictedOffer.get)
            (
              repairSolution,       // Figure Step 6
              initiator.addSharedBeliefs(repairSolution).produceUtterance() // Figure Step 7
            )

          }
        if (utterance.isEmpty) {
          // No utterance was produced, end the conversation.
          TurnData(
            initiatorState = initiator.addSharedBeliefs(reply),
            responderState = responder.addSharedBeliefs(reply),
            round = data.head.round + 1,
            initiatorPerceivedMutualUnderstanding = perceivedMutualUnderstanding,
            utterance = None,
            restrictedOffer = restrictedOffer
          ) +: data
        } else {
          val nextInitiator = initiator.addSharedBeliefs(reply ++ utterance.get)
          val nextResponder = responder.addSharedBeliefs(reply ++ utterance.get)
          val trouble       = nextResponder.troubleIdentification
          val restrictedOfferOption =
            if (trouble) nextResponder.repairFormulation
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
    }
  }
}
