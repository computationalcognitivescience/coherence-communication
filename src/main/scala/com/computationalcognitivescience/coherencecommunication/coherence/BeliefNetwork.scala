package com.computationalcognitivescience.coherencecommunication.coherence

import mathlib.graph._
import mathlib.set.SetTheory._

import scala.util.Random
class BeliefNetwork(
    val network: WUnDiGraph[String],
    val negativeConstraints: Set[WUnDiEdge[Node[String]]]
) extends WUnDiGraph[String](network.vertices, network.edges) {

  val positiveConstraints: Set[WUnDiEdge[Node[String]]] = network.edges \ negativeConstraints
  protected def isSatisfiedPositiveConstraint(assignment: Map[Node[String], Boolean])(
      edge: WUnDiEdge[Node[String]]
  ): Boolean = assignment(edge.left) == assignment(edge.right)

  protected def isSatisfiedNegativeConstraint(assignment: Map[Node[String], Boolean])(
      edge: WUnDiEdge[Node[String]]
  ): Boolean = assignment(edge.left) != assignment(edge.right)

  // Based on Blokpoel, M. & van Rooij, I. (2021). Theoretical modeling for cognitive science and psychology Chapter 5
  // Standard coherence
  def coherence(): Map[Node[String], Boolean] = {

    // Output
    // Get the truth-assignment that maximizes coherence
    val allAssignments =
      vertices allMappings Set(true, false) // Generate all possible truth-value assignments
    allAssignments
      .argMax(coh)
      .random
      .get // Return the truth-value assignment that maximizes coherence value
  }

  protected def cohPlus(assignment: Map[Node[String], Boolean]): Double = {
    val satisfiedPositiveConstraints: Set[WUnDiEdge[Node[String]]] =
      positiveConstraints.filter(isSatisfiedPositiveConstraint(assignment))

    satisfiedPositiveConstraints
      .map((edge: WUnDiEdge[Node[String]]) => edge.weight) // Get weights
      .sum                                                 // Sum weights
  }

  protected def cohMin(assignment: Map[Node[String], Boolean]): Double = {
    val satisfiedNegativeConstraints: Set[WUnDiEdge[Node[String]]] =
      negativeConstraints.filter(isSatisfiedNegativeConstraint(assignment))

    satisfiedNegativeConstraints
      .map((edge: WUnDiEdge[Node[String]]) => edge.weight) // Get weights
      .sum                                                 // Sum weights
  }

  // Calculate the coherence value as the weighted sum of satisfied edges
  def coh(assignment: Map[Node[String], Boolean]): Double =
    cohPlus + cohMin
}

object BeliefNetwork {
  def random(
              size: Int,
              density: Double,
              ratioPosNeg: Double,
              we: Option[Double] = None,
            ): BeliefNetwork = {
    val network = WUnDiGraph.random(size, density)
    val nrNegativeConstraints = network.edges.size - (network.edges.size * ratioPosNeg).intValue
    val negativeConstraints = Random.shuffle(network.edges).take(nrNegativeConstraints)
    if(we.isDefined) ???
    else ???
    ???
  })
}
