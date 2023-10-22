package com.computationalcognitivescience.coherencecommunication.coherence

import mathlib.graph._
import mathlib.set.SetTheory._

// TODO: Ask mark about weights for WUnDiEdges (They're not constrained to [0,1])
class CoherenceNetwork(
                        override val vertices: Set[Node[WeightedBelief]],
                        val positiveConstraints: Set[WUnDiEdge[Node[WeightedBelief]]],
                        val negativeConstraints: Set[WUnDiEdge[Node[WeightedBelief]]]) extends WUnDiGraph[WeightedBelief](vertices, positiveConstraints /\ negativeConstraints) {

  // Based on Thagard & Verbeurgt, 1998
  // Based on Blokpoel, M. & van Rooij, I. (2021). Theoretical modeling for cognitive science and psychology Chapter 5
  // Generalized discriminating coherence
  def dCoherence(preferredAssignment: Map[Node[WeightedBelief], Boolean]): Map[Node[WeightedBelief], Boolean]  = {

    // Output
    // The number of edges that are satisfied
    val allAssignments = vertices allMappings Set(true, false)
    allAssignments.argMax(dcoh(_, preferredAssignment)).random.get
  }

  // Generalized Foundational Coherence
  def fCoherence(requiredAssignment: Map[Node[WeightedBelief], Boolean]): Map[Node[WeightedBelief], Boolean]  = {

    // Check if truth-value assignment is valid (i.e. all foundational vertices have their required truth-value)
    def isValidAssignment(assignment: Map[Node[WeightedBelief], Boolean]): Boolean = {

      // Check if foundational vertex has its required truth-value
      def isSatisfied(vertex: Node[WeightedBelief]): Boolean = {
        requiredAssignment(vertex) == assignment(vertex)
      }

      requiredAssignment.keySet.forall(isSatisfied)
    }

    // Output
    val allAssignments = vertices allMappings Set(true, false) // Generate all possible truth-value assignments
    val validAssignments = allAssignments.filter(isValidAssignment)
    validAssignments.argMax(coh).random.get // Return the truth-value assignment that maximizes coherence value
  }

  // Based on Blokpoel, M. & van Rooij, I. (2021). Theoretical modeling for cognitive science and psychology Chapter 5
  // Standard coherence
  def coherence(): Map[Node[WeightedBelief], Boolean]  = {

    // Output
    // Get the truth-assignment that maximizes coherence
    val allAssignments = vertices allMappings Set(true, false) // Generate all possible truth-value assignments
    allAssignments.argMax(coh).random.get // Return the truth-value assignment that maximizes coherence value
  }

  // Calculate the (generalized) discriminating coherence value of a truth-value assignment
  def dcoh(assignment: Map[Node[WeightedBelief], Boolean], preferredAssignment: Map[Node[WeightedBelief], Boolean]): Double = {

    // Check for (u,v) if T(u) == T(v)
    def satisfiedPositiveConstraint(edge: WUnDiEdge[Node[WeightedBelief]]): Boolean = {
      assignment(edge.left) == assignment(edge.right)
    }

    // Check for (u,v) if T(u) != T(v)
    def satisfiedNegativeConstraint(edge: WUnDiEdge[Node[WeightedBelief]]): Boolean = {
      assignment(edge.left) != assignment(edge.right)
    }

    // Check for v if T(v) == T_pref(v)
    def satisfiedVertex(vertex: Node[WeightedBelief]): Boolean = {
      assignment(vertex) == preferredAssignment(vertex)
    }

    // Sum weights of satisfied positive constraints
    def coh_plus(): Double = {
      {positiveConstraints | satisfiedPositiveConstraint}
        .map((edge: WUnDiEdge[Node[WeightedBelief]]) => edge.weight).sum
    }

    // Sum weights of satisfied negative constraints
    def coh_min(): Double = {
      {negativeConstraints | satisfiedNegativeConstraint}
        .map((edge: WUnDiEdge[Node[WeightedBelief]]) => edge.weight).sum
    }

    // Sum weights of satisfied vertices in the preferred set
    val preferredSet: Set[Node[WeightedBelief]] = preferredAssignment.keySet
    def coh_p(): Double = {
      {preferredSet | satisfiedVertex}
        .map((vertex: Node[WeightedBelief]) => vertex.label.weight).sum
    }

    coh_plus + coh_min + coh_p
  }

  // Calculate the coherence value as the weighted sum of satisfied edges
  def coh(assignment: Map[Node[WeightedBelief], Boolean]): Double = {

    def satisfiedPositiveConstraint(edge: WUnDiEdge[Node[WeightedBelief]]): Boolean = {
      assignment(edge.left) == assignment(edge.right)
    }

    def satisfiedNegativeConstraint(edge: WUnDiEdge[Node[WeightedBelief]]): Boolean = {
      assignment(edge.left) != assignment(edge.right)
    }

    def coh_plus(): Double = {
      {positiveConstraints | satisfiedPositiveConstraint}
        .map((edge: WUnDiEdge[Node[WeightedBelief]]) => edge.weight).sum
    }

    def coh_min(): Double = {
      {negativeConstraints | satisfiedNegativeConstraint}
        .map((edge: WUnDiEdge[Node[WeightedBelief]]) => edge.weight).sum
    }

    coh_plus + coh_min
  }
}




