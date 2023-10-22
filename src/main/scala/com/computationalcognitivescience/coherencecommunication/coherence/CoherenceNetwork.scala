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
  def dCoherence(preferredAssignment: Map[Node[WeightedBelief], Boolean]): Map[Node[WeightedBelief], Boolean]  = {


    // Output
    // The number of edges that are satisfied
    val allAssignments = vertices allMappings Set(true, false)
    allAssignments.argMax(dcoh(_, preferredAssignment)).random.get
  }

  def dcoh(assignment: Map[Node[WeightedBelief], Boolean], preferredAssignment: Map[Node[WeightedBelief], Boolean]): Double = {
    // Check if a vertex' truth-value assignment equals its preferred assignment
    def isVertexSatisfied(vertex: Node[WeightedBelief]): Boolean ={
      assignment(vertex) == preferredAssignment(vertex)
    }

    // Map all satisfied preferred vertices to their weights w_p
    val preferredSet: Set[Node[WeightedBelief]] = preferredAssignment.keySet

    val satisfiedVertices: Double = {preferredSet | isVertexSatisfied }
      .map((vertex: Node[WeightedBelief]) => vertex.label.weight).sum

    def isEdgeSatisfied(edge: WUnDiEdge[Node[WeightedBelief]]): Boolean = {
      // This edge is a positive constraint and satisfied
      val posC = (positiveConstraints.contains(edge) && assignment(edge.left) == assignment(edge.right))
      // This edge is a negative constraint and satisfied
      val negC = (negativeConstraints.contains(edge) && assignment(edge.left) != assignment(edge.right))
      posC || negC
    }

    // Map all satisfied edges to their weights in w
    val satisfiedEdges: Double = { edges | isEdgeSatisfied _ }
      .map((edge: WUnDiEdge[Node[WeightedBelief]]) => edge.weight).sum

    satisfiedVertices + satisfiedEdges
  }

  // Based on Blokpoel, M. & van Rooij, I. (2021). Theoretical modeling for cognitive science and psychology Chapter 5
  // Calculate the "standard" coherence value of the given network
  def coherence(): Map[Node[WeightedBelief], Boolean]  = {

    // Output
    // Calculate the coherence value as the number of edges that are satisfied
    def coh(assignment: Map[Node[WeightedBelief], Boolean]): Double = {

      def isEdgeSatisfied(edge: WUnDiEdge[Node[WeightedBelief]]): Boolean = {
        // This edge is a positive constraint and satisfied
        val posC = (positiveConstraints.contains(edge) && assignment(edge.left) == assignment(edge.right))
        // This edge is a negative constraint and satisfied
        val negC = (negativeConstraints.contains(edge) && assignment(edge.left) != assignment(edge.right))
        posC || negC
      }

      // Map all satisfied edges to their weights in w
      val satisfiedEdges: Double = {edges | isEdgeSatisfied _ }
        .map((edge: WUnDiEdge[Node[WeightedBelief]]) => edge.weight).sum

      satisfiedEdges
    }

    val allAssignments = vertices allMappings Set(true, false) // Generate all possible truth-value assignments
    allAssignments.argMax(coh).random.get // Return the truth-value assignment that maximizes coherence value
  }
}




