package com.computationalcognitivescience.coherencecommunication

import com.computationalcognitivescience.coherencecommunication.coherence.TruthValueAssignment
import mathlib.graph.{Node, WUnDiEdge}
import mathlib.set.SetTheory._
import upickle.default.{macroRW, ReadWriter => RW}

case class ConversationData(
                             initiatorState: Initiator,
                             responderState: Responder,
                             round: Int,
                             utterance: Option[TruthValueAssignment],
                             restrictedOffer: Option[TruthValueAssignment],
) {

  val asymmetryAllBeliefs: Double = {
    1.0 - (initiatorState.allBeliefs ~ responderState.allBeliefs / initiatorState.graph.vertices.size.doubleValue)
  }

  val asymmetryIntentionBeliefs: Double =
    1.0 - initiatorState.allBeliefs.structuralSimilarity(
      responderState.allBeliefs,
      initiatorState.communicativeIntent.beliefs
    )

  val priorOverlap: Double =
    1.0 - (initiatorState.ownBeliefs.beliefs /\ responderState.ownBeliefs.beliefs).size

  val priorAsymmetry: Double =
    1.0 - (initiatorState.ownBeliefs ~ responderState.ownBeliefs
      / (initiatorState.ownBeliefs.beliefs /\ responderState.ownBeliefs.beliefs).size)

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

//  def toPicklableConversationData: PicklableConversationData = PicklableConversationData(
//    initiatorState.beliefNetwork.vertices.map(_.label),
//    initiatorState.beliefNetwork.positiveConstraints.map(e =>
//      PicklableWeightedEdge(e.left.label, e.right.label, e.weight)
//    ),
//    initiatorState.beliefNetwork.negativeConstraints.map(e =>
//      PicklableWeightedEdge(e.left.label, e.right.label, e.weight)
//    ),
//    initiatorState.priorBeliefs.map(kv => kv._1.label -> kv._2),
//    initiatorState.communicativeIntent.map(kv => kv._1.label -> kv._2),
//    initiatorState.inferredBeliefs.map(kv => kv._1.label -> kv._2),
//    responderState.priorBeliefs.map(kv => kv._1.label -> kv._2),
//    responderState.inferredBeliefs.map(kv => kv._1.label -> kv._2),
//    round,
//    utterance match {
//      case Some(utt) => Some(utt.map(kv => kv._1.label -> kv._2))
//      case None      => None
//    },
//    communicatedBeliefs.map(kv => kv._1.label -> kv._2),
//    repair match {
//      case Some(rep) => Some(rep.map(kv => kv._1.label -> kv._2))
//      case None      => None
//    },
//    utteranceLengthsInitiator,
//    repairLengthsResponder
//  )
}

//case class PicklableWeightedEdge(
//    left: String,
//    right: String,
//    weight: Double
//) {
//  def toWUnDiEdge: WUnDiEdge[Node[String]] =
//    WUnDiEdge(Node(left), Node(right), weight)
//}
//object PicklableWeightedEdge {
//  implicit val rw: RW[PicklableWeightedEdge] = macroRW
//}
//
//case class PicklableConversationData(
//    beliefs: Set[String],
//    positiveConstraints: Set[PicklableWeightedEdge],
//    negativeConstraints: Set[PicklableWeightedEdge],
//    initiatorPrior: Map[String, Boolean],
//    initiatorIntent: Map[String, Boolean],
//    initiatorInferred: Map[String, Boolean],
//    responderPrior: Map[String, Boolean],
//    responderInferred: Map[String, Boolean],
//    round: Int,
//    utterance: Option[Map[String, Boolean]],
//    communicatedBeliefs: Map[String, Boolean],
//    repair: Option[Map[String, Boolean]],
//    utteranceLengthsInitiator: Option[Int],
//    repairLengthsResponder: Option[Int]
//)
//
//object PicklableConversationData {
//  implicit val rw: RW[PicklableConversationData] = macroRW
//}
