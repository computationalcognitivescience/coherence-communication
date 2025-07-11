package com.computationalcognitivescience.coherencecommunication

import com.computationalcognitivescience.coherencecommunication.Understandings.Understanding
import com.computationalcognitivescience.coherencecommunication.coherence.TruthValueAssignment
import util.SetTheoryDev._
import mathlib.set.SetTheory._

case class TurnData(
    initiatorState: Initiator,
    responderState: Responder,
    round: Int,
    initiatorPerceivedMutualUnderstanding: Understanding,
    utterance: Option[TruthValueAssignment],
    restrictedOffer: Option[TruthValueAssignment]
) {

  val allBeliefsAsymmetry: Double = {
    1.0 - (initiatorState.allBeliefs ~ responderState.allBeliefs / initiatorState.graph.vertices.size.doubleValue)
  }

  val intentionBeliefAsymmetry: Double =
    1.0 - (initiatorState.communicativeIntent ~ responderState.allBeliefs /
      initiatorState.communicativeIntent.beliefs.size.doubleValue)

  val ownBeliefsOverlap: Double = {
    val ownBeliefsMinSize =
      scala.math.min(initiatorState.ownBeliefs.beliefs.size, responderState.ownBeliefs.beliefs.size)
    if (ownBeliefsMinSize == 0) 0.0
    else
      (initiatorState.ownBeliefs.beliefs /\ responderState.ownBeliefs.beliefs).size / ownBeliefsMinSize.doubleValue
  }

  val ownBeliefAsymmetry: Double = {
    if (ownBeliefsOverlap == 0.0) 0.0
    else
      1.0 - (initiatorState.ownBeliefs ~ responderState.ownBeliefs
        / (initiatorState.ownBeliefs.beliefs /\ responderState.ownBeliefs.beliefs).size.doubleValue)
  }

  def ownBeliefsChangedResponder(otherOwnBeliefs: TruthValueAssignment): Double = {
    if (responderState.ownBeliefs.size == 0.0) 0.0
    else
      1.0 - (responderState.ownBeliefs ~ otherOwnBeliefs / responderState.ownBeliefs.size.doubleValue)
  }

  val factualUnderstanding: Boolean =
    initiatorState.communicativeIntent ~ responderState.allBeliefs == 1

  def networkSize: Int = initiatorState.graph.size

  def networkConstraints: Int = initiatorState.graph.vertices.size

  def networkPCRatio: Double =
    initiatorState.negativeConstraints.size.doubleValue / networkConstraints

  def initiatorIntentSize: Int = initiatorState.communicativeIntent.size
}

object TurnData {}
