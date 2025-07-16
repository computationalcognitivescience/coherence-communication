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
    if (data.head.round == maxRounds) data
    else {
      val perceivedMutualUnderstanding = initiator.perceivedMutualUnderstanding(restrictedOffer)
      perceivedMutualUnderstanding match {
        case YesLiteral | YesPerceived => {
          /* No offer made and either literally have communicated all intent beliefs or
        inferred perceived understanding via perspective taking. End the conversation. */
          val turnData = TurnData(
            initiatorState = initiator,
            responderState = responder,
            round = data.head.round + 1,
            initiatorPerceivedMutualUnderstanding = perceivedMutualUnderstanding,
            utterance = None,
            restrictedOffer = None
          )
//          println(turnData)
          turnData +: data
        }
        case NoPerceived => {
          // No offer made and did not perceive understanding via perspective taking. Continue.
          val utterance = initiator.produceUtterance()
          val nextInitiator = initiator.addSharedBeliefs(utterance)
          val nextResponder = responder.addSharedBeliefs(utterance)
          val nextOffer =
            if (nextResponder.troubleIdentification) nextResponder.repairFormulation
            else None

          val turnData = TurnData(
            initiatorState = nextInitiator,
            responderState = nextResponder,
            round = data.head.round + 1,
            initiatorPerceivedMutualUnderstanding = perceivedMutualUnderstanding,
            utterance = Some(utterance),
            restrictedOffer = restrictedOffer
          )
          if (utterance.isEmpty) {
            println("EMPTY??!?!?")
            (turnData +: data).foreach(println)
          }

//                    println(turnData)
          simulateRound(
            initiator = nextInitiator,
            responder = nextResponder,
            restrictedOffer = nextOffer,
            data = turnData +: data
          )
        }
        case No => {
          // Offer was made, but not all beliefs in the offer match the communicative intent. Reply and continue.
          val reply         = initiator.repairSolution(restrictedOffer.get)
          val utterance     = initiator.addSharedBeliefs(reply).produceUtterance()
          val nextInitiator = initiator.addSharedBeliefs(reply ++ utterance)
          val nextResponder = responder.addSharedBeliefs(reply ++ utterance)
          val nextOffer =
            if (nextResponder.troubleIdentification) nextResponder.repairFormulation
            else None

          val turnData = TurnData(
            initiatorState = nextInitiator,
            responderState = nextResponder,
            round = data.head.round + 1,
            initiatorPerceivedMutualUnderstanding = perceivedMutualUnderstanding,
            utterance = Some(reply ++ utterance),
            restrictedOffer = nextOffer
          )
//          println(turnData)
          simulateRound(
            initiator = nextInitiator,
            responder = nextResponder,
            restrictedOffer = nextOffer,
            data = turnData +: data
          )
        }
        case YesConfirmed => {
          // Offer was made, and it matches the communicative intent. Confirm and then end the conversation.
          val turnData = TurnData(
            initiatorState = initiator.addSharedBeliefs(restrictedOffer.get),
            responderState = responder.addSharedBeliefs(restrictedOffer.get),
            round = data.head.round + 1,
            initiatorPerceivedMutualUnderstanding = perceivedMutualUnderstanding,
            utterance = Some(restrictedOffer.get),
            restrictedOffer = None
          )
//          println(turnData)
          turnData +: data
        }
        case _ => {
          // Something went wrong
          data
        }
      }
    }

//    if (data.length > maxRounds) {
//      // Stop conversation if it takes more than maxRounds
//      data
//    } else {
//      // Figure Step 5 (or 0)
//      val perceivedMutualUnderstanding = initiator.perceivedMutualUnderstanding(restrictedOffer)
//      if (perceivedMutualUnderstanding == Yes) {
//        // Offer made sense, or all intention beliefs were explicitly communicated, ending the conversation.
//        TurnData(
//          initiatorState = initiator,
//          responderState = responder,
//          round = data.head.round + 1,
//          initiatorPerceivedMutualUnderstanding = perceivedMutualUnderstanding,
//          utterance = TruthValueAssignment.empty,
//          restrictedOffer = None
//        ) +: data
//      } else if(perceivedMutualUnderstanding == NotYet) {
//        // No offer was made
//        val utterance = initiator.produceUtterance() // Figure Step 1
//        val nextInitiator = initiator.addSharedBeliefs(utterance)
//        val nextResponder = responder.addSharedBeliefs(utterance)
//        val trouble       = nextResponder.troubleIdentification
//        val restrictedOfferOption =
//          if (trouble) nextResponder.repairFormulation
//          else None
//        val turnData = TurnData(
//          initiatorState = nextInitiator,
//          responderState = nextResponder,
//          round = data.head.round + 1,
//          initiatorPerceivedMutualUnderstanding = perceivedMutualUnderstanding,
//          utterance = utterance,
//          restrictedOffer = restrictedOfferOption
//        )
//        simulateRound(nextInitiator, nextResponder, restrictedOfferOption, turnData +: data)
//      } else {
//        // Offer was given
//        val repairSolution = initiator.repairSolution(restrictedOffer.get) // Figure Step 6
//        val utterance = initiator
//          .addSharedBeliefs(repairSolution)
//          .produceUtterance() // Figure Step 7
//        val nextInitiator = initiator.addSharedBeliefs(repairSolution ++ utterance)
//        val nextResponder = responder.addSharedBeliefs(repairSolution ++ utterance)
//        val trouble       = nextResponder.troubleIdentification
//        val restrictedOfferOption =
//          if (trouble) nextResponder.repairFormulation
//          else None
//        val turnData = TurnData(
//          initiatorState = nextInitiator,
//          responderState = nextResponder,
//          round = data.head.round + 1,
//          initiatorPerceivedMutualUnderstanding = perceivedMutualUnderstanding,
//          utterance = utterance,
//          restrictedOffer = restrictedOfferOption
//        )
//        simulateRound(nextInitiator, nextResponder, restrictedOfferOption, turnData +: data)
//      }
//    }
  }

}
