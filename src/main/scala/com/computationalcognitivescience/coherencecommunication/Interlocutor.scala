package com.computationalcognitivescience.coherencecommunication

import mathlib.set.SetTheory._
import mathlib.graph._
import coherence.{BeliefNetwork, FoundationalBeliefNetwork}

abstract class Interlocutor(
    val beliefNetwork: FoundationalBeliefNetwork,
    val priorBeliefs: Map[Node[String], Boolean],
    val previousState: Option[Interlocutor] = None,
    val communicatedBeliefs: Map[Node[String], Boolean] = Map.empty,
    maxUtteranceLength: Option[Int] = None
) {

  lazy val inferredBeliefs: Map[Node[String], Boolean] = {
    val allPossibleMaximumCoherenceInferences = beliefNetwork.coherenceSolutions()
    val currentInferredBeliefSet =
      (beliefNetwork.vertices \ priorBeliefs.keySet) \ communicatedBeliefs.keySet

    if (previousState.isEmpty)
      allPossibleMaximumCoherenceInferences.random.get
    else {
      val overlapPreviousCurrentInferredBeliefs =
        previousState.get.inferredBeliefs.keySet /\ currentInferredBeliefSet

      def compare(
          a: Node[String],
          condidateInference: Map[Node[String], Boolean],
          previousBeliefs: Map[Node[String], Boolean]
      ): Int =
        if (condidateInference(a) == previousBeliefs(a)) 1 else 0

      // Infer the beliefs that are structurally most similar to the previous inferred beliefs
      allPossibleMaximumCoherenceInferences
        .argMax(inference => {
          sum(
            overlapPreviousCurrentInferredBeliefs,
            compare(_, inference, previousState.get.allBeliefTruthValueAssignments)
          )
        })
        .random
        .get
    }
  }

  lazy val allBeliefTruthValueAssignments: Map[Node[String], Boolean] =
    priorBeliefs ++ communicatedBeliefs ++ inferredBeliefs

  val utteranceLengthLimit: Int = {
    if (maxUtteranceLength.isDefined)
      maxUtteranceLength.get
    else beliefNetwork.vertices.size
  }

  def addCommunicatedBeliefs(utterance: Map[Node[String], Boolean]): Interlocutor

  /** Calculate structural similarity between the truth-value assignments of two interlocutors for
    * the given subset.
    *
    * @param subset
    *   Optional (sub)set of beliefs to calculate similarity over. If subset contains a node not in
    *   any of the belief networks, an error will occur. If left to None (default), it calculates
    *   the similarity of the entire network.
    * @return
    *   The number of beliefs that have the same truth-value assignment.
    */
  def structuralSimilarity(
      that: Interlocutor,
      subset: Set[Node[String]] = this.beliefNetwork.vertices
  ): Int = {
    subset.toList
      .map(belief =>
        if (
          this.allBeliefTruthValueAssignments(belief) == that
            .allBeliefTruthValueAssignments(belief)
        ) 1
        else 0
      )
      .sum
  }
}
