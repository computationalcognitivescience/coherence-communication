package com.computationalcognitivescience.coherencecommunication

import com.computationalcognitivescience.coherencecommunication.Understandings._
import com.computationalcognitivescience.coherencecommunication.coherence.Belief.Belief
import com.computationalcognitivescience.coherencecommunication.util.SetTheoryDev._
import com.computationalcognitivescience.coherencecommunication.coherence.{
  BeliefNetwork,
  FoundationalBeliefNetwork,
  TruthValueAssignment
}
import mathlib.set.SetTheory._
import mathlib.graph._

case class Initiator(
    override val graph: WUnDiGraph[String],
    override val negativeConstraints: Set[WUnDiEdge[Belief]],
    override val ownBeliefs: TruthValueAssignment,
    override val sharedBeliefs: TruthValueAssignment,
    communicativeIntent: TruthValueAssignment,
    override val previousState: Option[Initiator] = None,
    override val maxUtteranceLength: Option[Int] = None
) extends Interlocutor {
  override lazy val allBeliefs: TruthValueAssignment = if(previousState.isDefined) previousState.get.allBeliefs
  else beliefInference()

  assert(
    communicativeIntent.beliefs.forall(graph.vertices.contains),
    "Communicative intent contains beliefs not present in the belief network."
  )

  /** Computes a belief inference for an utterance from the other agents perspective by ignoring the
    * [[Initiator]]s own beliefs.
    * @param beliefs
    *   The utterance to compute the belief inference for.
    * @return
    */
  private def perspectiveTaking(beliefs: TruthValueAssignment): TruthValueAssignment = {
    val perspective = Initiator(
      graph = graph,
      negativeConstraints = negativeConstraints,
      ownBeliefs = TruthValueAssignment.empty,
      sharedBeliefs = sharedBeliefs ++ beliefs,
      communicativeIntent = TruthValueAssignment.empty,
      previousState = Some(this),
      maxUtteranceLength = maxUtteranceLength
    )
    perspective.allBeliefs
  }

  /** Computes <span style="font-variant-caps: normal;">Produce Utterance</span> for this
    * [[Initiator]].
    *
    * TODO Include updated LaTeX definition.
    * @return
    */
  def produceUtterance(): TruthValueAssignment = {
    val allPossibleUtteranceBeliefs: Set[TruthValueAssignment] =
      if (maxUtteranceLength.isDefined)
        (powersetUp(graph.vertices \ sharedBeliefs.beliefs, maxUtteranceLength.get) - Set.empty)
          .map(beliefSet => allBeliefs.subAssignment(beliefSet)) // Map the belief set to a tva
      else
        (powerset(graph.vertices \ sharedBeliefs.beliefs) - Set.empty)
          .map(beliefSet => allBeliefs.subAssignment(beliefSet)) // Map the belief set to a tva

    def relativeStructuralSimilarity(utterance: TruthValueAssignment): Double = {
      val similarity = communicativeIntent ~ perspectiveTaking(utterance)
      if (similarity == 0) 0.0
      else (1.0 / utterance.size) * similarity
    }

    argMax(allPossibleUtteranceBeliefs, relativeStructuralSimilarity)
      .random
      .getOrElse(TruthValueAssignment.empty)
  }

  /** Computes <span style="font-variant-caps: normal;">Perceived Mutual Understanding</span> for
    * this [[Initiator]], relative to an optional offer from the [[Responder]].
    *
    * TODO Include updated LaTeX definition.
    *
    * @param offer
    *   An optional restricted offer from a [[Responder]].
    * @return
    *   Yes, NotYet, or No understanding.
    */
  def perceivedMutualUnderstanding(offer: Option[TruthValueAssignment]): Understanding = {
    if (offer.isEmpty) {
      if (forall(allBeliefs.beliefs, (belief: Belief) => sharedBeliefs.contains(belief)))
        Understandings.Yes
      else Understandings.NotYet
    } else {
      val perspective                      = perspectiveTaking(offer.get)
      def compare(belief: Belief): Boolean = allBeliefs(belief) == perspective(belief)
      if (forall(communicativeIntent.beliefs, compare)) Understandings.Yes
      else Understandings.No
    }
  }

  def repairSolution(offer: TruthValueAssignment): TruthValueAssignment = {
    assert(
      offer.beliefs /\ sharedBeliefs.beliefs == Set.empty,
      "Restricted offer contains previously communicated beliefs, something went wrong."
    )
    allBeliefs.subAssignment(offer.beliefs)
  }

  override def addSharedBeliefs(utterance: TruthValueAssignment): Initiator = Initiator(
    graph = graph,
    negativeConstraints = negativeConstraints,
    ownBeliefs = ownBeliefs,
    sharedBeliefs = sharedBeliefs ++ utterance,
    communicativeIntent = communicativeIntent,
    previousState = Some(this),
    maxUtteranceLength = maxUtteranceLength
  )
}
