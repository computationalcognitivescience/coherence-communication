package com.computationalcognitivescience.coherencecommunication

import mathlib.set.SetTheory._
import coherence.Belief.Belief
import coherence.{FoundationalBeliefNetwork, TruthValueAssignment}
import mathlib.graph.{WUnDiEdge, WUnDiGraph}

trait Interlocutor {
  val graph: WUnDiGraph[String]
  val negativeConstraints: Set[WUnDiEdge[Belief]]
  val ownBeliefs: TruthValueAssignment
  val sharedBeliefs: TruthValueAssignment = TruthValueAssignment.emtpy
  val previousState: Option[Interlocutor] = None
  val maxUtteranceLength: Option[Int]     = None

  protected val foundationalBeliefNetwork: FoundationalBeliefNetwork = FoundationalBeliefNetwork(
    graph = graph,
    negativeConstraints = negativeConstraints,
    priorBeliefs = ownBeliefs.beliefs \/ sharedBeliefs.beliefs,
    priorBeliefsAssignment = ownBeliefs ++ sharedBeliefs
  )
  lazy val allBeliefs: TruthValueAssignment      = beliefInference()
  lazy val inferredBeliefs: TruthValueAssignment = allBeliefs \ ownBeliefs \ sharedBeliefs
  lazy val coherence: Double                = foundationalBeliefNetwork.coh(allBeliefs)

  /** Computes the belief revision for `ownBeliefs` as defined in <span style="font-variant-caps:
    * normal;">Belief Revision</span>.
    *
    * A truth-value assignment over all beliefs, $T$ that is maximally coherent and conforms to the
    * prior as defined by the set:
    * $$\mathbf{T_{max}}=\textsc{F-Coherence}(G,C^+,C^-,T_{shared}\oplus T_{own}) $$ and is
    * maximally structurally similar to previously inferred truth values $T=\argmax_{T\in
    * \mathbf{T_{max}}}T\sim T_{prev}$.
    *
    * @return
    */
  private def beliefInference(): TruthValueAssignment = {
    val tMax: Set[TruthValueAssignment] = foundationalBeliefNetwork.coherenceSolutions()

    if (previousState.isDefined) {
      val tPrev: TruthValueAssignment = previousState.get.allBeliefs
      tMax.argMax(tva => tva ~ tPrev).random.get
    } else {
      tMax.random.get
    }
  }

  /** Returns a new interlocutor (either [[Initiator]] or [[Responder]]) with the new utterance
    * added to the shared beliefs.
    * @param utterance The beliefs to add to shared beliefs.
    * @return
    */
  def addSharedBeliefs(utterance: TruthValueAssignment): Interlocutor
}
