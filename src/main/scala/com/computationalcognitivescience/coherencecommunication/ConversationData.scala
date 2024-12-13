package com.computationalcognitivescience.coherencecommunication

import mathlib.graph.{Node, WUnDiEdge}
import mathlib.set.SetTheory._
import upickle.default.{macroRW, ReadWriter => RW}

case class ConversationData(
    initiatorState: Initiator,
    responderState: Responder,
    round: Int,
    utterance: Option[Map[Node[String], Boolean]],
    communicatedBeliefs: Map[Node[String], Boolean],
    repair: Option[Map[Node[String], Boolean]],
    utteranceLengthsInitiator: Option[Int],
    repairLengthsResponder: Option[Int]
) {

  def asymmetryAllBeliefs: Double =
    1.0 - initiatorState.structuralSimilarity(
      responderState
    ) / initiatorState.beliefNetwork.vertices.size.doubleValue

  def asymmetryIntentionBeliefs: Double = {
    val communicativeIntentBeliefs = initiatorState.communicativeIntent.keySet
    1.0 - initiatorState.structuralSimilarity(
      responderState,
      communicativeIntentBeliefs
    ) / communicativeIntentBeliefs.size.doubleValue
  }

  def priorOverlap: Double = {
    1.0 - (initiatorState.priorBeliefs.keySet /\ responderState.priorBeliefs.keySet).size
  }

  def priorAsymmetry: Double = {
    val overlappingPriors =
      initiatorState.priorBeliefs.keySet /\ responderState.priorBeliefs.keySet
    1.0 - initiatorState.structuralSimilarity(
      responderState,
      overlappingPriors
    ) / overlappingPriors.size.doubleValue
  }

  /*
  id: Int
  networkSize: Int
  networkConstraints: Int
  networkPCRatio: Double
  initiatorIntentSize: Int
  initiatiorPriorSize: Int
  responderPriorSize: Int
  round: Int
  asymmetryAllBeliefs: Double
  asymmetryIntentionBeliefs: Double
  priorOverlap: Double
  priorAsymmtery: Double

   */

  def toPicklableConversationData: PicklableConversationData = PicklableConversationData(
    initiatorState.beliefNetwork.vertices.map(_.label),
    initiatorState.beliefNetwork.positiveConstraints.map(e =>
      PicklableWeightedEdge(e.left.label, e.right.label, e.weight)
    ),
    initiatorState.beliefNetwork.negativeConstraints.map(e =>
      PicklableWeightedEdge(e.left.label, e.right.label, e.weight)
    ),
    initiatorState.priorBeliefs.map(kv => kv._1.label -> kv._2),
    initiatorState.communicativeIntent.map(kv => kv._1.label -> kv._2),
    initiatorState.inferredBeliefs.map(kv => kv._1.label -> kv._2),
    responderState.priorBeliefs.map(kv => kv._1.label -> kv._2),
    responderState.inferredBeliefs.map(kv => kv._1.label -> kv._2),
    round,
    utterance match {
      case Some(utt) => Some(utt.map(kv => kv._1.label -> kv._2))
      case None      => None
    },
    communicatedBeliefs.map(kv => kv._1.label -> kv._2),
    repair match {
      case Some(rep) => Some(rep.map(kv => kv._1.label -> kv._2))
      case None      => None
    },
    utteranceLengthsInitiator,
    repairLengthsResponder
  )
}

case class PicklableWeightedEdge(
    left: String,
    right: String,
    weight: Double
) {
  def toWUnDiEdge: WUnDiEdge[Node[String]] =
    WUnDiEdge(Node(left), Node(right), weight)
}
object PicklableWeightedEdge {
  implicit val rw: RW[PicklableWeightedEdge] = macroRW
}

case class PicklableConversationData(
    beliefs: Set[String],
    positiveConstraints: Set[PicklableWeightedEdge],
    negativeConstraints: Set[PicklableWeightedEdge],
    initiatorPrior: Map[String, Boolean],
    initiatorIntent: Map[String, Boolean],
    initiatorInferred: Map[String, Boolean],
    responderPrior: Map[String, Boolean],
    responderInferred: Map[String, Boolean],
    round: Int,
    utterance: Option[Map[String, Boolean]],
    communicatedBeliefs: Map[String, Boolean],
    repair: Option[Map[String, Boolean]],
    utteranceLengthsInitiator: Option[Int],
    repairLengthsResponder: Option[Int]
)

object PicklableConversationData {
  implicit val rw: RW[PicklableConversationData] = macroRW
}
