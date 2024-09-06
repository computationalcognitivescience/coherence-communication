package com.computationalcognitivescience.coherencecommunication

import mathlib.set.SetTheory._
import mathlib.graph._
import coherence.{BeliefNetwork, FoundationalBeliefNetwork}

abstract class Interlocutor(
  val beliefNetwork: FoundationalBeliefNetwork,
  val priorBeliefs: Map[Node[String], Boolean],
  val previousInferredBeliefs: Map[Node[String], Boolean] = Map.empty,
  val communicatedBeliefs: Map[Node[String], Boolean] = Map.empty,
  maxUtteranceLength: Option[Int] = None
) {

  lazy val inferredBeliefs: Map[Node[String], Boolean] = {
    val allPossibleMaximumCoherenceInferences = beliefNetwork.coherenceSolutions()
    val currentInferredBeliefSet =
      (beliefNetwork.vertices \ priorBeliefs.keySet) \ communicatedBeliefs.keySet
    val overlapPreviousCurrentInferredBeliefs =
      previousInferredBeliefs.keySet /\ currentInferredBeliefSet

    // Infer the beliefs that are structurally most similar to the previous inferred beliefs
    allPossibleMaximumCoherenceInferences
      .argMax(structuralSim(_, previousInferredBeliefs, overlapPreviousCurrentInferredBeliefs))
      .random
      .get
  }

  lazy val allBeliefTruthValueAssignments: Map[Node[String], Boolean] = priorBeliefs ++ communicatedBeliefs ++ inferredBeliefs

  val utteranceLengthLimit: Int = {
    if (maxUtteranceLength.isDefined)
      maxUtteranceLength.get
    else beliefNetwork.vertices.size
  }

  def addCommunicatedBeliefs(utterance: Map[Node[String], Boolean]): Interlocutor

  /** Calculate structural similarity between two truth-value assignments
   *
   * @param assignment
   * The first truth-value assignment
   * @param otherAssignment
   * The second truth-value assignment
   * @param V
   * The set of Nodes to calculate similarity over
   * if V contains a node not in assignment or otherAssignment, an error will occur
   * @return
   * The number of nodes in V which have the same truth-value in both assignments
   */
  protected def structuralSim(
                     assignment: Map[Node[String], Boolean],
                     otherAssignment: Map[Node[String], Boolean],
                     V: Set[Node[String]]
                   ): Int =
    V.count((v: Node[String]) => assignment(v) == otherAssignment(v))
}
