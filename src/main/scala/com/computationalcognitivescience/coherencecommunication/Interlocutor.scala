package com.computationalcognitivescience.coherencecommunication

import mathlib.set.SetTheory._
import coherence.Belief.Belief
import coherence.{FoundationalBeliefNetwork, TruthValueAssignment}
import mathlib.graph.{WUnDiEdge, WUnDiGraph}

/** Trait that both the [[Initiator]] and [[Responder]] extend. It contains the overlapping
  * datastructure and `beliefInference()` function as defined in <span style="font-variant-caps: *
  * small-caps;">Belief Revision</span>.
  */
trait Interlocutor {

  /** The graph representing the belief network. */
  val graph: WUnDiGraph[String]

  /** The set of negative constraints. */
  val negativeConstraints: Set[WUnDiEdge[Belief]]

  /** The set of the interlocutors own beliefs $T_{own}. */
  val ownBeliefs: TruthValueAssignment

  /** The set of the shared (communicated) beliefs $T_{shared}. */
  val sharedBeliefs: TruthValueAssignment = TruthValueAssignment.empty

  /** An optional previous state of the interlocutor to provide access to the interlocutor's
    * previous truth-value assignmend $T_{prev}$.
    */
  val previousState: Option[Interlocutor] = None

  /** The maximum number of beliefs that can be communicated in one utterance $k$. */
  val maxUtteranceLength: Option[Int] = None

  /** The foundational belief network of the interlocutor. */
  val foundationalBeliefNetwork: FoundationalBeliefNetwork = FoundationalBeliefNetwork(
    graph = graph,
    negativeConstraints = negativeConstraints,
    priorBeliefs = ownBeliefs.beliefs \/ sharedBeliefs.beliefs,
    priorBeliefsAssignment = ownBeliefs ++ sharedBeliefs
  )

  /** The truth-value assignment to all the beliefs in the network. */
  lazy val allBeliefs: TruthValueAssignment = beliefInference()

  /** The truth-value assignment to the subset of beliefs that are neither own or shared beliefs. */
  lazy val inferredBeliefs: TruthValueAssignment = allBeliefs \ ownBeliefs \ sharedBeliefs

  /** The coherence value of the truth-value assignment `allBeliefs`. */
  lazy val coherence: Double = foundationalBeliefNetwork.coh(allBeliefs)

  /** Computes the belief revision for `ownBeliefs` as defined in <span style="font-variant-caps:
    * small-caps;">Belief Inference</span>.
    *
    * The belief network $B$ with constraints $C&#94;+,C&#94;-$ is represented by
    * `foundationalBeliefNetwork`. This function evaluates to the maximally coherent truth-value
    * assignment that is both consistent with the own and shared beliefs:
    *
    * $$\mathbf{T_{max}}=F-Coherence(G,C&#94;+,C&#94;-,T_{own} ++ T_{shared})$$
    *
    * and is maximally structurally similar to previously inferred truth values:
    * $$T=\arg\!\max_{T\in \mathbf{T_{max}}}T\sim T_{prev}$$
    *
    * @return
    */
  protected def beliefInference(): TruthValueAssignment = {
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
    * @param utterance
    *   The beliefs to add to shared beliefs.
    * @return
    */
  def addSharedBeliefs(utterance: TruthValueAssignment): Interlocutor
}
