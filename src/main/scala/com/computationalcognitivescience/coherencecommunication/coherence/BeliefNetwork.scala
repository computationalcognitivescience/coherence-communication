package com.computationalcognitivescience.coherencecommunication.coherence

import mathlib.graph._
import mathlib.set.SetTheory._

import scala.util.Random
class BeliefNetwork(
    val network: WUnDiGraph[String],
    val negativeConstraints: Set[WUnDiEdge[Node[String]]]
) extends WUnDiGraph[String](network.vertices, network.edges) {

  val positiveConstraints: Set[WUnDiEdge[Node[String]]] = network.edges \ negativeConstraints

  /** Check if in the given truth-value assignment a positive constraint is satisfied
   *
   * @param assignment
   *  The truth-value assignment over vertices
   * @param edge
   *  A positive constraint
   * @return
   *  True if the constraint is satisfied, false otherwise
   */
  protected def isSatisfiedPositiveConstraint(assignment: Map[Node[String], Boolean])(
      edge: WUnDiEdge[Node[String]]
  ): Boolean = assignment(edge.left) == assignment(edge.right)

  /** Check if in the given truth-value assignment a negative constraint is satisfied
   *
   * @param assignment
   *  The truth-value assignment over vertices
   * @param edge
   *  A negative constraint
   * @return
   *  True if the constraint is satisfied, false otherwise
   */
  protected def isSatisfiedNegativeConstraint(assignment: Map[Node[String], Boolean])(
      edge: WUnDiEdge[Node[String]]
  ): Boolean = assignment(edge.left) != assignment(edge.right)

  // Based on Blokpoel, M. & van Rooij, I. (2021). Theoretical modeling for cognitive science and psychology Chapter 5
  // Standard coherence

  /** Calculate the optimal truth-value assignment of this BeliefNetwork
   *
   * @return
   *  A truth-value assignment over vertices that results in maximum coherence
   *  If multiple maximal truth-value assignments exists, get a random maximal one.
   */
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

  /** Calculate the coherence-value from positive constraints with a given truth-value assignment
   *
   * @param assignment
   *  A truth-value assignment over vertices
   * @return
   *  The weighted sum over satisfied positive constraints
   */
  protected def cohPlus(assignment: Map[Node[String], Boolean]): Double = {
    val satisfiedPositiveConstraints: Set[WUnDiEdge[Node[String]]] =
      positiveConstraints.filter(isSatisfiedPositiveConstraint(assignment))

    satisfiedPositiveConstraints
      .map((edge: WUnDiEdge[Node[String]]) => edge.weight) // Get weights
      .sum                                                 // Sum weights
  }

  /** Calculate the coherence-value from negative constraints with a given truth-value assignment
   *
   * @param assignment
   *  A truth-value assignment over vertices
   * @return
   *  The weighted sum over satisfied negative constraints
   */
  protected def cohMin(assignment: Map[Node[String], Boolean]): Double = {
    val satisfiedNegativeConstraints: Set[WUnDiEdge[Node[String]]] =
      negativeConstraints.filter(isSatisfiedNegativeConstraint(assignment))

    satisfiedNegativeConstraints
      .map((edge: WUnDiEdge[Node[String]]) => edge.weight) // Get weights
      .sum                                                 // Sum weights
  }

  /** Calculate the coherence-value from all constraints with given truth-value assignment
   *
   * @param assignment
   *  A truth-value assignment over vertices
   * @return
   *  The weighted sum over all satisfied constraints
   */
  def coh(assignment: Map[Node[String], Boolean]): Double =
    cohPlus(assignment) + cohMin(assignment)
}

// NOTES:
object RandomBeliefNetwork {
  /** Generate a belief network from a fixed number of vertices, but a (semi) random number of edges based
   * on an edge density and a (semi) random number of negative edges. Edges are generated with probability
   * equal to density and the positive-negative ratio determines how many of these will be considered
   * negative constraints.
   *
   * [KNOWN BUG]: Generated networks allow for duplicate edges (A, B) (B,A) and self edges (A, A)
   *
   * @param size
   *  The number of edges in the network
   * @param density
   *  The (expected) density of edges in the network
   * @param ratioPosNeg
   *  The target ratio of positive edges to negative edges
   * @return
   *  A randomly generated BeliefNetwork
   */
  def random(
              size: Int,
              density: Double,
              ratioPosNeg: Double,
//              we: Option[Double] = None,
            ): BeliefNetwork = {
    assert(0 <= density && density <= 1)
    assert(0 <= ratioPosNeg && ratioPosNeg <= 1)
    val network: WUnDiGraph[String] = WUnDiGraph.random(size-1, density)
    val nrNegativeConstraints: Int = (network.edges.size * (1 - ratioPosNeg)).intValue
    val negativeConstraints: Set[WUnDiEdge[Node[String]]] = Random.shuffle(network.edges).take(nrNegativeConstraints)

    new BeliefNetwork(network, negativeConstraints)
  }

  /** Generate a belief network from a fixed number of vertices, edges and negative constraints
   * with edges having random (uniformly drawn) weights between 0 and 1.
   *
   * @param size
   *  The number of vertices in the network
   * @param nrEdges
   *  The number of edges in the network
   * @param nrNegativeEdges
   *  The number of negative constraints in the belief network where
   *  The number of negative constraints must be smaller than or equal to the total number of edges
   * @return
   *  A randomly generated BeliefNetwork
   */
  def random(
              size: Int,
              nrEdges: Int,
              nrNegativeEdges: Int,
              //              we: Option[Double] = None,
            ): BeliefNetwork = {
    assert(nrEdges <= size*size)
    assert(nrNegativeEdges <= nrEdges)

    // Remove edge candidates that result int self-edges or are reversed duplicates of other edge candidates
    def noDuplicateEdges(endpoints: (Int, Int)): Boolean = {
      endpoints._1 < endpoints._2
    }

    // Generate vertex and edge candidates (candidates stay as Int for easier processing)
    val vertexCandidates: Set[Int] = (0 until size).toSet
    val edgeCandidates: Set[(Int, Int)] = (vertexCandidates x vertexCandidates).filter(noDuplicateEdges)

    // Generate vertices and edges
    val vertices: Set[Node[String]] = vertexCandidates.map("N" + _).map(Node(_))
    val edges: Set[WUnDiEdge[Node[String]]] = edgeCandidates.take(nrEdges)
      .map((e: (Int, Int)) => ("N" + e._1, "N" + e._2))
      .map((e: (String, String)) => (Node(e._1), Node(e._2)))
      .map((e: (Node[String], Node[String])) => WUnDiEdge(e._1, e._2, Random.nextDouble()))

    // Determine which edges are negative
    val negativeConstraints: Set[WUnDiEdge[Node[String]]] = edges.take(nrNegativeEdges)

    // Generate graph and belief network
    val network: WUnDiGraph[String] = WUnDiGraph(vertices, edges)
    new BeliefNetwork(network, negativeConstraints)
  }
}

