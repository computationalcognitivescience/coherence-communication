package com.computationalcognitivescience.coherencecommunication

import com.computationalcognitivescience.coherencecommunication.coherence.BeliefNetwork
import mathlib.graph.Node

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
)
