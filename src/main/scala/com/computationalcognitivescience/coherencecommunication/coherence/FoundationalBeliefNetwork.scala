package com.computationalcognitivescience.coherencecommunication.coherence

import com.computationalcognitivescience.coherencecommunication.coherence.Belief.Belief
import com.computationalcognitivescience.coherencecommunication.coherence.TruthValueAssignment.ImplMap
import mathlib.graph._
import mathlib.set.SetTheory._

case class FoundationalBeliefNetwork(
    override val graph: WUnDiGraph[String],
    override val negativeConstraints: Set[WUnDiEdge[Belief]],
    priorBeliefs: Set[Belief],
    priorBeliefsAssignment: TruthValueAssignment
) extends BaseBeliefNetwork {

  private def addFoundationalAssignment(
      assignment: TruthValueAssignment
  ): FoundationalBeliefNetwork =
    FoundationalBeliefNetwork(
      graph,
      negativeConstraints,
      priorBeliefs ++ assignment.beliefs,
      priorBeliefsAssignment ++ assignment
    )

  override def coherence(): TruthValueAssignment =
    coherenceSolutions().random.get // Return the truth-value assignment that maximizes coherence value
  /** Calculate the optimal truth-value assignment of this FoundationalBeliefNetwork
    *
    * Based on Blokpoel, M. & van Rooij, I. (2021). Theoretical modeling for cognitive science and
    * psychology Chapter 5
    *
    * @return
    *   A truth-value assignment over vertices that results in maximum coherence If multiple maximal
    *   truth-value assignments exists, get a random maximal one.
    */
  override def coherenceSolutions(): Set[TruthValueAssignment] = {
    // Get truth-value assignment over non-foundational nodes
    val nonPriorBeliefs: Set[Belief] = graph.vertices -- priorBeliefs
    val allPossibleNonPriorAssignments: Set[TruthValueAssignment] =
      (nonPriorBeliefs allMappings Set(true, false)).map(_.toTruthValueAssignment)

    // Add foundational truth-value assignments
    val allAssignments: Set[TruthValueAssignment] =
      allPossibleNonPriorAssignments.map(_ ++ priorBeliefsAssignment)

    // Get highest coherence solutions
    allAssignments.argMax(coh)
  }

  /** Check if truth-value assignment is valid (i.e. all foundational vertices have their required
    * truth-value)
    *
    * @param assignment
    *   A truth-value assignment over vertices
    * @return
    *   True if all foundational nodes have their required truth-value assignment as given in
    *   foundationalAssignment, False otherwise
    */
  private def isValidAssignment(assignment: TruthValueAssignment): Boolean = {

    // Check if foundational vertex has its required truth-value
    def isSatisfied(vertex: Belief): Boolean = {
      priorBeliefsAssignment(vertex) == assignment(vertex)
    }

    priorBeliefs.forall(isSatisfied)
  }

  /** Generate all possible truth-value assignments over nodes incident to a negative constraint
    * O(pow(2,unassignedMinus))
    *
    * Branching rule Observation: for an optimal partition, any vertex that is connected by a
    * negative constraint must either be accepted or rejected Therefore, branch on unassigned
    * vertices incident to a negative constraint such that we have 2 graphs On graph where the
    * vertex is accepted, and one where it is rejected Effectively this generates all possible
    * truth-value assignments over vertices incident to a negative edge
    *
    * @param unassignedMinus
    *   Set of nodes incident to a negative constraint
    * @return
    *   All possible truth value assignments over unassignedMinus [PLUS the foundational assignment
    *   set]
    */
  override def ac1(
      unassignedMinus: Set[Belief] // All nodes incident to a negative constraint
  ): Set[TruthValueAssignment] =
    unassignedMinus
      .allMappings(Set(true, false))
      .map(tva => TruthValueAssignment(tva.keySet, tva.toSet))
      .map(_ ++ priorBeliefsAssignment)
}

case object FoundationalBeliefNetwork {

}
