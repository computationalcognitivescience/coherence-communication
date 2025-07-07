package com.computationalcognitivescience.coherencecommunication

import coherence._
import com.computationalcognitivescience.coherencecommunication.coherence.Belief.Belief
import mathlib.graph._
import mathlib.set.SetTheory._
import coherence.TruthValueAssignment._

case class Responder(
    override val graph: WUnDiGraph[String],
    override val negativeConstraints: Set[WUnDiEdge[Belief]],
    override val ownBeliefs: TruthValueAssignment,
    override val sharedBeliefs: TruthValueAssignment,
    override val previousState: Option[Responder] = None,
    override val maxUtteranceLength: Option[Int] = None
) extends Interlocutor {

  /** Evaluates to `true` iff. the coherence of the current truth-value assignment is lower than of
    * the previous truth-value assignment.
    * @return
    */
  def troubleIdentification(utterance: TruthValueAssignment): (Boolean, Responder) = {
    val nextResponder = this.addSharedBeliefs(utterance)
    (nextResponder.coherence < this.coherence, nextResponder)
  }

  /** Computes <span style="font-variant-caps: normal;">Repair Formulation</span> for this
    * [[Responder]].
    *
    * TODO Include updated LaTeX definition.
    *
    * @param utterance
    *   The utterance to integrate into beliefs and formulate a restricted offer for.
    * @return
    *   A restricted offer or None.
    */
  def repairFormulation(utterance: TruthValueAssignment): Option[TruthValueAssignment] = {
    val allPossibleOfferBeliefs: Set[Set[Belief]] =
      if (maxUtteranceLength.isDefined)
        powersetUp(graph.vertices \ sharedBeliefs.beliefs, maxUtteranceLength.get) \ Set(Set.empty)
      else
        powerset(graph.vertices \ sharedBeliefs.beliefs) \ Set(Set.empty)

    val allTOffers: Set[TruthValueAssignment] = allPossibleOfferBeliefs
      .flatMap((offers: Set[Belief]) => offers.allMappings(Set(true, false)))
      .map(_.toTruthValueAssignment)

    def relativeTOfferCoherence(offer: TruthValueAssignment): Double = {
      addSharedBeliefs(offer).coherence / offer.size
    }

    val allOptimalTOffers = argMax(allTOffers, relativeTOfferCoherence)

    if (previousState.isDefined) {
      val tPrev: TruthValueAssignment = previousState.get.allBeliefs
      allOptimalTOffers.argMax(tva => tva ~ tPrev).random
    } else {
      allOptimalTOffers.random
    }
  }

  override protected def addSharedBeliefs(utterance: TruthValueAssignment): Responder = Responder(
    graph = graph,
    negativeConstraints = negativeConstraints,
    ownBeliefs = ownBeliefs,
    sharedBeliefs = sharedBeliefs ++ utterance,
    previousState = Some(this),
    maxUtteranceLength = maxUtteranceLength
  )
}
