package com.computationalcognitivescience.coherencecommunication.coherence

import com.computationalcognitivescience.coherencecommunication.coherence.Belief.Belief
import mathlib.graph._
import mathlib.set.SetTheory._

import scala.annotation.tailrec

trait BaseBeliefNetwork extends CMinusAlgorithm {

  val graph: WUnDiGraph[String]
  val negativeConstraints: Set[WUnDiEdge[Belief]]
  require(
    negativeConstraints isSubsetEqTo graph.edges,
    "The set of negative constraints is not a subset of or equal to the edges in the graph."
  )
  override val positiveConstraints: Set[WUnDiEdge[Belief]] = graph.edges \ negativeConstraints

  def vertices: Set[Belief]         = graph.vertices
  def edges: Set[WUnDiEdge[Belief]] = graph.edges
  def size: Int                     = graph.size

  /** Check if in the given truth-value assignment a positive constraint is satisfied
    *
    * @param assignment
    *   The truth-value assignment over vertices
    * @param edge
    *   A positive constraint
    * @return
    *   True if the constraint is satisfied, false otherwise
    */
  override protected def isSatisfiedPositiveConstraint(assignment: TruthValueAssignment)(
      edge: WUnDiEdge[Belief]
  ): Boolean = assignment(edge.left) == assignment(edge.right)

  /** Check if in the given truth-value assignment a negative constraint is satisfied
    *
    * @param assignment
    *   The truth-value assignment over vertices
    * @param edge
    *   A negative constraint
    * @return
    *   True if the constraint is satisfied, false otherwise
    */
  override protected def isSatisfiedNegativeConstraint(assignment: TruthValueAssignment)(
      edge: WUnDiEdge[Belief]
  ): Boolean = assignment(edge.left) != assignment(edge.right)

  /** Check if in the given truth-value assignment a positive constraint is determined
    *
    * @param assignment
    *   The truth-value assignment over vertices
    * @param edge
    *   A constraint
    * @return
    *   True if both endpoints of the edge have been assigned, false otherwise
    */
  override protected def isDeterminedConstraint(assignment: TruthValueAssignment)(
      edge: WUnDiEdge[Belief]
  ): Boolean = assignment.contains(edge.left) && assignment.contains(edge.right)

  /** Calculate the coherence-value from positive constraints with a given truth-value assignment
    *
    * @param assignment
    *   A truth-value assignment over vertices
    * @return
    *   The weighted sum over satisfied positive constraints
    */
  protected def cohPlus(assignment: TruthValueAssignment): Double = {
    sum(
      { positiveConstraints | isSatisfiedPositiveConstraint(assignment) _ },
      (edge: WUnDiEdge[Belief]) => edge.weight
    )
  }

  /** Calculate the coherence-value from negative constraints with a given truth-value assignment
    *
    * @param assignment
    *   A truth-value assignment over vertices
    * @return
    *   The weighted sum over satisfied negative constraints
    */
  protected def cohMin(assignment: TruthValueAssignment): Double = {
    val satisfiedNegativeConstraints: Set[WUnDiEdge[Belief]] =
      negativeConstraints.filter(isSatisfiedNegativeConstraint(assignment))

    satisfiedNegativeConstraints.toList
      .map((edge: WUnDiEdge[Belief]) => edge.weight) // Get weights
      .sum                                           // Sum weights

  }

  /** Calculate the coherence-value from all constraints with given truth-value assignment
    *
    * @param assignment
    *   A truth-value assignment over vertices
    * @return
    *   The weighted sum over all satisfied constraints
    */
  def coh(assignment: TruthValueAssignment): Double =
    cohPlus(assignment) + cohMin(assignment)

  def coherence(): TruthValueAssignment =
    coherenceSolutions().random.get // Return the truth-value assignment that maximizes coherence value

  /** Calculate the optimal truth-value assignment of this BeliefNetwork
    *
    * Based on Blokpoel, M. & van Rooij, I. (2021). Theoretical modeling for cognitive science and
    * psychology Chapter 5
    *
    * @return
    *   A truth-value assignment over vertices that results in maximum coherence If multiple maximal
    *   truth-value assignments exists, get a random maximal one.
    */
  def coherenceSolutions(): Set[TruthValueAssignment] = {
    // Get the truth-assignment that maximizes coherence
    val allAssignments =
      (graph.vertices allMappings Set(true, false)) // Generate all possible truth-value assignments
        .map(tva =>
          TruthValueAssignment(tva.keySet, tva.toSet)
        ) // Convert Map to TruthValueAssignment
    allAssignments.argMax(coh)
  }


}
