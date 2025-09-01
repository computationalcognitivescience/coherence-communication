package com.computationalcognitivescience.coherencecommunication

import coherence._
import com.computationalcognitivescience.coherencecommunication.coherence.Belief.Belief
import mathlib.graph._
import mathlib.set.SetTheory._
import coherence.TruthValueAssignment._

/** Class that implements all functions and data structures related to simulating the responder.
  *
  * @param graph
  *   The graph representing the belief network.
  * @param negativeConstraints
  *   The set of negative constraints.
  * @param ownBeliefs
  *   The set of the interlocutors own beliefs $T_{own}$.
  * @param sharedBeliefs
  *   The set of the shared (communicated) beliefs $T_{shared}$.
  * @param previousState
  *   An optional previous state of the responder to provide access to the responder's previous
  *   truth-value assignment $T_{prev}$.
  * @param maxUtteranceLength
  *   The maximum number of beliefs that can be communicated in one utterance $k$.
  */
case class Responder(
    override val graph: WUnDiGraph[String],
    override val negativeConstraints: Set[WUnDiEdge[Belief]],
    override val ownBeliefs: TruthValueAssignment,
    override val sharedBeliefs: TruthValueAssignment,
    override val previousState: Option[Responder] = None,
    override val maxUtteranceLength: Option[Int] = None
) extends Interlocutor {

  /** Evaluates to `true` if the coherence of the current truth-value assignment is lower than of
    * the previous truth-value assignment and `false` otherwise.
    * @return
    */
  def troubleIdentification: Boolean = if (previousState.isDefined)
    this.coherence < previousState.get.coherence
  else false

  /** Computes <span style="font-variant-caps: small-caps;">Repair Formulation</span>
    *
    * A truth-value assignment over a subset of beliefs excluding the shared beliefs $T_{offer}:
    * V_{offer} \rightarrow \{true, false\}$, where $V_{offer}\in\mathcal{P}(V\setminus V_{shared})$
    * and $|V_{offer}|\leq k$. The restricted offer is one of all possible requests (i.e., all
    * possible truth-value assignments to all possible subsets $V_{offer}$)
    * $T_{offer}\in\mathbf{T_{offer}}$. The restricted offer is maximally coherent relative to the
    * utterance length, and conforms to the prior as defined by:
    *
    * $$\mathbf{T_{optimal}}=\bigcup_{T_{offer}\in\mathbf{T_{offer}}}Believe
    * Inference(G,C&#94;+,C&#94;-,T_{own}, T_{shared} ++ T_{offer},T_{prev})$$
    *
    * and
    * $$\mathbf{T_{relative}}=\arg\!\max_{T_{relative}\in\mathbf{T_{optimal}}}coh(T_{relative})/|V_{relative}|$$
    *
    * and is maximally structurally similar to previously inferred truth values $$T=\arg\!\max_{T\in
    * \mathbf{T_{relative}}}T\sim T_{prev}$$
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

    def relativeTOfferCoherence(offer: TruthValueAssignment): Double =
      addSharedBeliefs(offer).coherence / offer.size

    val tOptimal         = argMax(allPossibleOffers, relativeTOfferCoherence)
    val allOptimalOffers = argMax(tOptimal, (tOffer: TruthValueAssignment) => tOffer ~ allBeliefs)
    if (allOptimalOffers.isEmpty) None
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
