package computationalcognitivescience.coherencecommunication.datastructures

import Understandings.Understanding
import computationalcognitivescience.coherencecommunication.{Initiator, Responder}
import computationalcognitivescience.coherencecommunication.coherence.TruthValueAssignment
import mathlib.set.SetTheory._

case class TurnData(
    initiatorState: Initiator,
    responderState: Responder,
    round: Int,
    initiatorPerceivedMutualUnderstanding: Understanding,
    utterance: Option[TruthValueAssignment],
    reply: Option[TruthValueAssignment],
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

  def networkConstraints: Int = initiatorState.graph.edges.size

  def networkPCRatio: Double =
    initiatorState.negativeConstraints.size.doubleValue / networkConstraints

  def initiatorIntentSize: Int = initiatorState.communicativeIntent.size

  override def toString: String =
    s"""$round
       |utterance $utterance
       |reply $reply
       |offer $restrictedOffer
       |perceived $initiatorPerceivedMutualUnderstanding
       |i_sha ${initiatorState.sharedBeliefs}
       |r_sha ${responderState.sharedBeliefs}
       |i_own ${initiatorState.ownBeliefs}
       |r_own ${responderState.ownBeliefs}
       |i_inf ${initiatorState.inferredBeliefs}
       |r_inf ${responderState.inferredBeliefs}
       |intent ${initiatorState.communicativeIntent}
       |""".stripMargin
}

object TurnData {}
