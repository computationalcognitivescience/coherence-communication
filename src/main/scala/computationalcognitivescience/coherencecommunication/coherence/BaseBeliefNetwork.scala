package computationalcognitivescience.coherencecommunication.coherence

import Belief.Belief
import mathlib.graph._
import mathlib.set.SetTheory._

import scala.annotation.tailrec

/** Trait containing the specification of a belief network, represented as a weighted undirected
  * graph with negative constraints.
  *
  * ==Bibliography==
  *
  * Blokpoel, M. & van Rooij, I. (2021-2025). Theoretical modeling for cognitive science and
  * psychology. Retrieved 2025-08-15 from
  * [[https://computationalcognitivescience.github.io/lovelace/]].
  *
  * van Rooij, I. (2008). The Tractable Cognition Thesis. _Cognitive Science: A Multidisciplinary
  * Journal, 32_(6), 939–984. [[https://doi.org/10.1080/03640210801897856]]
  *
  * Thagard, P., & Verbeurgt, K. (1998). Coherence as Constraint Satisfaction. _Cognitive Science,
  * 22_(1), 1–24. [[https://doi.org/10.1207/s15516709cog2201_1]]
  */
trait BaseBeliefNetwork {

  /** The weighted undirected graph $G=(V,E)$ of the belief network. */
  val graph: WUnDiGraph[String]

  /** The negative constraints $C&#94;-$ of the belief network. */
  val negativeConstraints: Set[WUnDiEdge[Belief]]
  require(
    negativeConstraints isSubsetEqTo graph.edges,
    "[ERROR] The set of negative constraints is not a subset of or equal to the edges in the graph."
  )

  /** The positive constraints $C&#94;+=E\setminus C&#94;-$   of the belief network. */
  val positiveConstraints: Set[WUnDiEdge[Belief]] = graph.edges \ negativeConstraints

  /** The set of vertices (beliefs) in this network. */
  def vertices: Set[Belief] = graph.vertices

  /** The set of edges (constraints) in this network. */
  def edges: Set[WUnDiEdge[Belief]] = graph.edges

  /** The size of the network. */
  def size: Int = graph.size

  /** Check if in the given truth-value assignment a positive constraint is satisfied
    *
    * @param assignment
    *   The truth-value assignment over vertices
    * @param edge
    *   A positive constraint
    * @return
    *   True if the constraint is satisfied, false otherwise
    */
  protected def isSatisfiedPositiveConstraint(assignment: TruthValueAssignment)(
      edge: WUnDiEdge[Belief]
  ): Boolean = assignment(edge.left) == assignment(edge.right)

  /** Check if in the given truth-value assignment a negative constraint is satisfied
    * $\forall_{a,b\in assignment, a\neq b}T(a)\neq T(b)$.
    *
    * @param assignment
    *   The truth-value assignment over vertices
    * @param edge
    *   A negative constraint
    * @return
    *   True if the constraint is satisfied, false otherwise
    */
  protected def isSatisfiedNegativeConstraint(assignment: TruthValueAssignment)(
      edge: WUnDiEdge[Belief]
  ): Boolean = assignment(edge.left) != assignment(edge.right)

  /** Check if in the given truth-value assignment a positive constraint is determined
    * $\forall_{a,b\in assignment, a\neq b}T(a)= T(b)$.
    * @param assignment
    *   The truth-value assignment over vertices
    * @param edge
    *   A constraint
    * @return
    *   True if both endpoints of the edge have been assigned, false otherwise
    */
  protected def isDeterminedConstraint(assignment: TruthValueAssignment)(
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

  /** Calculate the coherence-value from all constraints with given truth-value assignment as
    * defined in <span style="font-variant: small-caps;">F-Coherence</span>:
    *
    * $$coh&#94;+(T)=\sum_{\substack{(a,b,w)\in C&#94;+\\T(a)=T(b)}}w$$
    *
    * $$coh&#94;-(T)=\sum_{\substack{(a,b,w)\in C&#94;-\\T(a)\neq T(b)}}w$$
    *
    * $$\arg\!\max_{T\in\mathcal{T}}\left(coh&#94;+(T)+coh&#94;-(T)\right)$$
    *
    * @param assignment
    *   A truth-value assignment over vertices
    * @return
    *   The weighted sum over all satisfied constraints
    */
  def coh(assignment: TruthValueAssignment): Double =
    cohPlus(assignment) + cohMin(assignment)

  /** Evaluates to an optimal coherence truth-value assignment. In case multiple optimal solutions
    * are possible, returns one at random.
    * @return
    *   An optimal truth-value assignment
    */
  def coherence(): TruthValueAssignment =
    coherenceSolutions().random.get // Return the truth-value assignment that maximizes coherence value

  /** Calculate the set of optimal truth-value assignment of this BeliefNetwork
    *
    * Based on Blokpoel, M. & van Rooij, I. (2021-2025). Theoretical modeling for cognitive science
    * and psychology Chapter 5.
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
