package com.computationalcognitivescience.coherencecommunication

import com.computationalcognitivescience.coherencecommunication.datastructures.ConversationData.ConversationData
import com.computationalcognitivescience.coherencecommunication.datastructures.Understandings._
import com.computationalcognitivescience.coherencecommunication.coherence.TruthValueAssignment
import com.computationalcognitivescience.coherencecommunication.coherence.TruthValueAssignment.ImplMap
import com.computationalcognitivescience.coherencecommunication.datastructures.{TurnData, Understandings}
import mathlib.graph.{WUnDiEdge, WUnDiGraph}
import mathlib.set.SetTheory._

import scala.annotation.tailrec
import scala.util.Random

case class Conversation(
    initialInitiator: Initiator,
    initialResponder: Responder,
    maxRounds: Int
) {

  private val preFirstRoundConversationData = TurnData(
    initiatorState = initialInitiator,
    responderState = initialResponder,
    round = 0,
    initiatorPerceivedMutualUnderstanding = Understandings.NotYet,
    utterance = None,
    reply = None,
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
      val possibleReply =
        if (restrictedOffer.isDefined) initiator.repairSolution(restrictedOffer.get)
        else TruthValueAssignment.empty
      val perceivedMutualUnderstanding = initiator.perceivedMutualUnderstanding(possibleReply)
      perceivedMutualUnderstanding match {
        case YesLiteral | YesPerceived =>
          /* All beliefs have been literally shared, possibly after reply to restricted offer. */
          val turnData = TurnData(
            initiatorState = initiator.addSharedBeliefs(possibleReply),
            responderState = responder.addSharedBeliefs(possibleReply),
            round = data.head.round + 1,
            initiatorPerceivedMutualUnderstanding = perceivedMutualUnderstanding,
            utterance = None,
            reply = if (restrictedOffer.isDefined) Some(possibleReply) else None,
            restrictedOffer = None
          )
//          println(turnData)
          turnData +: data
        case NotYet =>
          // Did not perceive understanding via perspective taking. Continue with possibly reply.
          val utterance     = initiator.addSharedBeliefs(possibleReply).produceUtterance()
          val nextInitiator = initiator.addSharedBeliefs(possibleReply ++ utterance)
          val nextResponder = responder.addSharedBeliefs(possibleReply ++ utterance)
          val nextOffer =
            if (nextResponder.troubleIdentification) nextResponder.repairFormulation
            else None

          val turnData = TurnData(
            initiatorState = nextInitiator,
            responderState = nextResponder,
            round = data.head.round + 1,
            initiatorPerceivedMutualUnderstanding = Understandings.NotYet,
            utterance = Some(utterance),
            reply =  if (restrictedOffer.isDefined) Some(possibleReply) else None,
            restrictedOffer = nextOffer
          )
//                    println(turnData)
          simulateRound(
            initiator = nextInitiator,
            responder = nextResponder,
            restrictedOffer = nextOffer,
            data = turnData +: data
          )
        case _ =>
          // Something went wrong
          data
      }
    }
  }
}

object Conversation {
  def random(
      beliefNetworkSize: Int,
      preferentialAttachementM: Int,
      beliefNetworkPCRatio: Double,
      initiatorPriorRatio: Double,
      initiatorCommunicativeIntentRatio: Double,
      maxUtteranceLength: Int,
      priorsOverlapRatio: Double,
      priorsAsymmetryRatio: Double,
      responderPriorRatio: Double,
      maxRoundLength: Int
  ): Conversation = {
    val randomGraph =
      WUnDiGraph.preferentialAttachment(beliefNetworkSize + 2, preferentialAttachementM, 1.0)
    val randomGraph1 = WUnDiGraph(
      randomGraph.vertices,
      randomGraph.edges.map(edge => WUnDiEdge(edge.left, edge.right, 1.0))
    )
    //          WUnDiGraph.uniform(
    //            n = parameters.beliefNetworkSize,
    //            numberEdges =
    //              (parameters.beliefNetworkSize * parameters.beliefNetworkConstraintsRatio).intValue
    //          )
    val negativeConstraints = scala.util.Random
      .shuffle(randomGraph.edges.toSeq)
      .take((randomGraph.edges.size * beliefNetworkPCRatio).intValue)
      .toSet

    val initiatorOwnBeliefs = Random
      .shuffle(randomGraph.vertices.toSeq)
      .take((randomGraph.vertices.size * initiatorPriorRatio).intValue)
      .map(belief => (belief, Random.nextBoolean()))
      .toMap
      .toTruthValueAssignment

    val initiatorCommunicativeIntent = Random
      .shuffle((randomGraph.vertices \ initiatorOwnBeliefs.beliefs).toSeq)
      .take(
        (randomGraph.vertices.size * initiatorCommunicativeIntentRatio).intValue
      )
      .map(belief => (belief, Random.nextBoolean()))
      .toMap
      .toTruthValueAssignment

    val initiator = Initiator(
      randomGraph,
      negativeConstraints,
      initiatorOwnBeliefs,
      sharedBeliefs = TruthValueAssignment.empty,
      initiatorCommunicativeIntent,
      maxUtteranceLength = Some(maxUtteranceLength)
    )

    val initiatorPriorVertices = initiatorOwnBeliefs.beliefs.toSeq

    val nrResponderPriorBeliefs = (randomGraph.vertices.size * responderPriorRatio).intValue
    val nrResponderOverlappingPriorBeliefs = math.min(
      (initiatorPriorVertices.size * priorsOverlapRatio).intValue,
      nrResponderPriorBeliefs
    )
    val nrReponderOwnPriorBeliefs = nrResponderPriorBeliefs - nrResponderOverlappingPriorBeliefs

    val responderOverlappingPriorVertices = Random
      .shuffle(initiatorPriorVertices)
      .take((initiatorPriorVertices.size * priorsOverlapRatio).intValue)

    val responderOverlappingSymmetricOwnBeliefs = Random
      .shuffle(responderOverlappingPriorVertices)
      .take(
        (responderOverlappingPriorVertices.size * priorsAsymmetryRatio).intValue
      )
      .map(belief => (belief, initiatorOwnBeliefs(belief).get))
      .toMap
      .toTruthValueAssignment

    val responderOverlappingAsymmetricOwnBeliefs = Random
      .shuffle(initiatorPriorVertices.toSet \ responderOverlappingSymmetricOwnBeliefs.beliefs)
      .take(
        (responderOverlappingPriorVertices.size * (1 - priorsAsymmetryRatio)).intValue
      )
      .map(belief => (belief, !initiatorOwnBeliefs(belief).get))
      .toMap
      .toTruthValueAssignment

    val responderNonOverlappingOwnBeliefs = Random
      .shuffle((randomGraph.vertices \ responderOverlappingPriorVertices.toSet).toSeq)
      .take(nrReponderOwnPriorBeliefs)
      .map(belief => (belief, Random.nextBoolean()))
      .toMap
      .toTruthValueAssignment
    val responderOwnBeliefs = responderOverlappingSymmetricOwnBeliefs ++
      responderOverlappingAsymmetricOwnBeliefs ++
      responderNonOverlappingOwnBeliefs

    val responder = Responder(
      randomGraph,
      negativeConstraints,
      responderOwnBeliefs,
      sharedBeliefs = TruthValueAssignment.empty,
      maxUtteranceLength = Some(maxUtteranceLength)
    )

    Conversation(
      initiator,
      responder,
      maxRoundLength
    )
  }
}
