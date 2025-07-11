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
  def troubleIdentification: Boolean =
    if (previousState.isDefined) this.coherence < previousState.get.coherence
    else false

  /** Computes <span style="font-variant-caps: normal;">Repair Formulation</span> for this
    * [[Responder]].
    *
    * TODO Include updated LaTeX definition.
    *
    * @return
    *   A restricted offer or None.
    */
  def repairFormulation: Option[TruthValueAssignment] = {
    val allPossibleOfferBeliefs: Set[Set[Belief]] =
      if (maxUtteranceLength.isDefined)
        powersetUp(graph.vertices \ sharedBeliefs.beliefs, maxUtteranceLength.get) - Set.empty
      else
        powerset(graph.vertices \ sharedBeliefs.beliefs) - Set.empty

    val allPossibleOffers: Set[TruthValueAssignment] = allPossibleOfferBeliefs
      .flatMap((offers: Set[Belief]) => offers.allMappings(Set(true, false)))
      .map(_.toTruthValueAssignment)

    def relativeTOfferCoherence(offer: TruthValueAssignment): Double = {
      addSharedBeliefs(offer).coherence / offer.size
    }

    val allOptimalOffers = argMax(
      argMax(allPossibleOffers, relativeTOfferCoherence),
      (tOffer: TruthValueAssignment) => tOffer ~ allBeliefs
    )
    if(allOptimalOffers.isEmpty) None
    else Some(allOptimalOffers.random.get)
  }

  override def addSharedBeliefs(utterance: TruthValueAssignment): Responder = Responder(
    graph = graph,
    negativeConstraints = negativeConstraints,
    ownBeliefs = (ownBeliefs ++ sharedBeliefs ++ utterance).subAssignment(
      ownBeliefs.beliefs
    ), // When own beliefs are overruled by the shared beliefs.
    sharedBeliefs = sharedBeliefs ++ utterance,
    previousState = Some(this),
    maxUtteranceLength = maxUtteranceLength
  )
}
