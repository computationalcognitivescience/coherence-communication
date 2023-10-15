package com.computationalcognitivescience.coherencecommunication.coherence

import mathlib.graph._
import mathlib.set.SetTheory._

class CoherenceNetwork(
                        override val vertices: Set[Node[WeightedBelief]],
                        val positiveConstraints: Set[WUnDiEdge[Node[WeightedBelief]]],
                        val negativeConstraints: Set[WUnDiEdge[Node[WeightedBelief]]]) extends WUnDiGraph[WeightedBelief](vertices, positiveConstraints /\ negativeConstraints) {

  // Based on Thagard & Verbeurgt, 1998
  // Based on Blokpoel, M. & van Rooij, I. (2021). Theoretical modeling for cognitive science and psychology Chapter 5
  // Calculate the D-Coherence value of th given CoherenceNetwork
  def dCoherence(): Map[Node[WeightedBelief], Boolean]  = {

    // Output
    // The number of edges that are satisfied
    def coh(assignment: Map[Node[WeightedBelief], Boolean]): Double = {
      // Check the discriminating vertices d in D, add w(d) if d is accepted
      val satisfiedVertices: Double = vertices.map((v: Node[WeightedBelief]) => if (assignment(v)) v.label.weight else 0).sum
      //TODO: FOREACH(d in vertices where assignment(d) is TRUE): SUM(d)

      // Map all satisfied edges to their weights in w
      val satisfiedEdges: Double = {edges | ((edge: WUnDiEdge[Node[WeightedBelief]]) =>
        (
          positiveConstraints.contains(edge) // This edge is a positive constraint
            && // and
            assignment(edge.left) == assignment(edge.right) // Both vertices are in the same set (A or R)
            || // or
            negativeConstraints.contains(edge) // This edge is a negative constraint
              && // and
              assignment(edge.left) != assignment(edge.right) // Vertices are in different sets
          ))}.map((edge: WUnDiEdge[Node[WeightedBelief]]) => edge.weight).sum

      satisfiedVertices + satisfiedEdges
    }

    val allAssignments = vertices allMappings Set(true, false)
    allAssignments.argMax(coh).random.get
  }

  // Based on Blokpoel, M. & van Rooij, I. (2021). Theoretical modeling for cognitive science and psychology Chapter 5
  // Calculate the "standard" coherence value of the given network
  def coherence(): Map[Node[WeightedBelief], Boolean]  = {

    // Output
    // Calculate the coherence value as the number of edges that are satisfied
    def coh(assignment: Map[Node[WeightedBelief], Boolean]): Double = {
      {
        edges | ((edge: WUnDiEdge[Node[WeightedBelief]]) =>
          (
            positiveConstraints.contains(edge) // This edge is a positive constraint
              && // and
              assignment(edge.left) == assignment(edge.right) // Both vertices are in the same set (A or R)
              || // or
              negativeConstraints.contains(edge) // This edge is a negative constraint
                && // and
                assignment(edge.left) != assignment(edge.right) // Vertices are in different sets
            ))
      }.map((edge: WUnDiEdge[Node[WeightedBelief]]) => edge.weight).sum
    }

    val allAssignments = vertices allMappings Set(true, false) // Generate all possible truth-value assignments
    allAssignments.argMax(coh).random.get // Return the truth-value assignment that maximizes coherence value
  }
}




