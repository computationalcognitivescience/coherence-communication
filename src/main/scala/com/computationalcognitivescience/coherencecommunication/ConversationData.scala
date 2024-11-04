package com.computationalcognitivescience.coherencecommunication

import mathlib.graph.Node
import upickle.default.{macroRW, ReadWriter => RW}

case class ConversationData(
    initiatorState: Initiator,
    responderState: Responder,
    round: Int,
    utterance: Option[Map[Node[String], Boolean]],
    repair: Option[Map[Node[String], Boolean]],
    utteranceLengthsInitiator: Option[Int],
    repairLengthsResponder: Option[Int],
    similarityAllBeliefs: Int,
    similarityIntentionBeliefs: Int,
    similarityCommunicatedBeliefs: Int
) {
  def toPicklableConversationData: PicklableConversationData = PicklableConversationData(
    initiatorState.beliefNetwork.vertices.map(_.label),
    initiatorState.priorBeliefs.map(kv => kv._1.label -> kv._2),
    initiatorState.communicativeIntent.map(kv => kv._1.label -> kv._2),
    initiatorState.inferredBeliefs.map(kv => kv._1.label -> kv._2),
    responderState.priorBeliefs.map(kv => kv._1.label -> kv._2),
    responderState.inferredBeliefs.map(kv => kv._1.label -> kv._2),
    round,
    utterance match {
        case Some(utt) => Some(utt.map(kv => kv._1.label -> kv._2))
        case None => None
    },
    repair match {
      case Some(rep) => Some(rep.map(kv => kv._1.label -> kv._2))
      case None => None
    },
    utteranceLengthsInitiator,
    repairLengthsResponder,
    similarityAllBeliefs,
    similarityIntentionBeliefs,
    similarityCommunicatedBeliefs
  )
}

case class PicklableConversationData(
    beliefs: Set[String],
    initiatorPrior: Map[String, Boolean],
    initiatorIntent: Map[String, Boolean],
    initiatorInferred: Map[String, Boolean],
    responderPrior: Map[String, Boolean],
    responderInferred: Map[String, Boolean],
    round: Int,
    utterance: Option[Map[String, Boolean]],
    repair: Option[Map[String, Boolean]],
    utteranceLengthsInitiator: Option[Int],
    repairLengthsResponder: Option[Int],
    similarityAllBeliefs: Int,
    similarityIntentionBeliefs: Int,
    similarityCommunicatedBeliefs: Int
)

object PicklableConversationData {
  implicit val rw: RW[PicklableConversationData] = macroRW
}
