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
    utterance: TruthValueAssignment,
    restrictedOffer: Option[TruthValueAssignment]
) {

  val allBeliefsAsymmetry: Double = initiatorState.allBeliefs.asymmetry(responderState.allBeliefs)

  val intentionBeliefAsymmetry: Double =
    initiatorState.communicativeIntent.asymmetry(responderState.allBeliefs)

  val ownBeliefsOverlap: Double = {
    val ownBeliefsMinSize =
      scala.math.min(initiatorState.ownBeliefs.beliefs.size, responderState.ownBeliefs.beliefs.size)
    if (ownBeliefsMinSize == 0) 0.0
    else
      (initiatorState.ownBeliefs.beliefs /\ responderState.ownBeliefs.beliefs).size / ownBeliefsMinSize.doubleValue
  }

  val ownBeliefAsymmetry: Double = initiatorState.ownBeliefs.asymmetry(responderState.ownBeliefs)

  def allBeliefsChangedResponder(otherAllBeliefs: TruthValueAssignment): Double =
    responderState.allBeliefs.asymmetry(otherAllBeliefs)

  def ownBeliefsChangedResponder(otherOwnBeliefs: TruthValueAssignment): Double =
    responderState.ownBeliefs.asymmetry(otherOwnBeliefs)

  val factualUnderstanding: Boolean =
    initiatorState.communicativeIntent ~ responderState.allBeliefs == 1

  def networkSize: Int = initiatorState.graph.size

  def networkConstraints: Int = initiatorState.graph.vertices.size

  def networkPCRatio: Double =
    initiatorState.negativeConstraints.size.doubleValue / networkConstraints

  def initiatorIntentSize: Int = initiatorState.communicativeIntent.size
}

object TurnData {}
