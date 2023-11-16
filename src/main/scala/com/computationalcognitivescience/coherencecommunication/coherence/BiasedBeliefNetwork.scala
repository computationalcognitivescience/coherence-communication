package com.computationalcognitivescience.coherencecommunication.coherence

import mathlib.graph.{Node, WUnDiEdge, WUnDiGraph}
import mathlib.set.SetTheory._

import scala.util.Random

class BiasedBeliefNetwork(
    network: WUnDiGraph[String],
    negativeConstraints: Set[WUnDiEdge[Node[String]]],
    val biasBeliefs: Set[Node[String]],
    val biasAssignment: Map[Node[String], Boolean],
    val biasWeights: Map[Node[String], Double]
) extends BeliefNetwork(network, negativeConstraints) {

  /** Calculate the coherence-value from biased beliefs with a given truth-value assignment
    *
    * @param assignment
    *   A truth-value assignment over vertices
    * @return
    *   The weighted sum over satisfied biased beliefs
    */
  protected def cohBias(assignment: Map[Node[String], Boolean]): Double = {

    /** Return the biased belief's weight if the belief is satisfied
      *
      * @param belief
      *   A vertex in the network, must be a biased belief
      * @return
      *   The weight of the bias if the belief's bias is satisfied, 0.0 otherwise
      */
    def biasWeight(belief: Node[String]): Double =
      if (assignment(belief) == biasAssignment(belief)) biasWeights(belief)
      else 0.0

    sum(biasBeliefs, biasWeight _)
  }

  /** Calculate the coherence-value from communicated beliefs with a given truth-value assignment
    * @param assignment
    *   A truth-value assignment over vertices
    * @return
    *   The weighted sum over satisfied communicated beliefs
    */
  def cohComm(
      assignment: Map[Node[String], Boolean],
      communicatedBeliefs: Set[Node[String]],
      communicatedAssignment: Map[Node[String], Boolean],
      communicatedWeights: Map[Node[String], Double]
  ): Double = {

    /** Return the communicated belief's weight if the belief is satisfied
      *
      * @param belief
      *   A vertex in the network, must be a communicated belief
      * @return
      *   The weight of the bias if the communicated belief is satisfied, 0.0 otherwise
      */
    def commWeight(belief: Node[String]): Double =
      if (assignment(belief) == communicatedAssignment(belief)) communicatedWeights(belief)
      else 0.0

    sum(communicatedBeliefs, commWeight _)
  }

  /** Calculate the coherence-value from all constraints and biased beliefs with the given
    * truth-value assignment.
    *
    * @param assignment
    *   A truth-value assignment over vertices
    * @return
    *   The weighted sum over all satisfied constraints
    */
  override def coh(
      assignment: Map[Node[String], Boolean]
  ): Double =
    cohPlus(assignment) + cohMin(assignment) + cohBias(assignment)

  /** Calculate the coherence-value from all constraints and biased beliefs with the given
    * truth-value assignment, taking into account the truth value assignment about the communicated
    * beliefs.
    *
    * @param assignment
    *   A truth-value assignment over vertices
    * @return
    *   The weighted sum over all satisfied constraints including weights on vertices
    */
  def coh_communicated(
      assignment: Map[Node[String], Boolean],
      communicatedBeliefs: Set[Node[String]],
      communicatedAssignment: Map[Node[String], Boolean],
      communicatedWeights: Map[Node[String], Double]
  ): Double =
    cohPlus(assignment) + cohMin(assignment) + cohBias(assignment) + cohComm(
      assignment,
      communicatedBeliefs,
      communicatedAssignment,
      communicatedWeights
    )

}

object RandomBiasedBeliefNetwork {

  /** Generate a belief network from a fixed number of vertices, a (semi) random number of edges
    * based on an edge density, and a (semi) random number of negative edges. Additionally randomly
    * chooses biased nodes and (semi-)randomly assigns truth values and weights to biases
    *
    * [KNOWN BUG]: Generated networks allow for duplicate edges (A, B) (B,A) and self edges (A, A)
    *
    * @param size
    *   The number of edges in the network
    * @param density
    *   The (expected) density of edges in the network
    * @param ratioPosNeg
    *   The target ratio of positive edges to negative edges
    * @param ratioBiased
    *   The target ratio of biased beliefs in the network
    * @param ratioBiasTrueFalse
    *   The target ratio of biased beliefs that are true as opposed to false
    * @return
    *   A randomly generated BiasedBeliefNetwork
    */
  def random(
      size: Int,
      density: Double,
      ratioPosNeg: Double,
      ratioBiased: Double,
      ratioBiasTrueFalse: Double
      //              we: Option[Double] = None,
  ): BiasedBeliefNetwork = {
    assert(0 <= density && density <= 1)
    assert(0 <= ratioPosNeg && ratioPosNeg <= 1)
    assert(0 <= ratioBiased && ratioBiased <= 1)
    assert(0 <= ratioBiasTrueFalse && ratioBiasTrueFalse <= 1)

    // Generate the vertices and edges of the network
    val network: WUnDiGraph[String] =
      WUnDiGraph.random(size - 1, density) // Call built-in graph generator
    val nrNegativeConstraints: Int =
      network.edges.size * (1 - ratioPosNeg).intValue // Calculate number of negative constraints
    val negativeConstraints: Set[WUnDiEdge[Node[String]]] = Random
      .shuffle(network.edges.toList)
      .take(nrNegativeConstraints)
      .toSet // Determine which edges become negative constraints

    // Generate biased beliefs
    val nrBiasBeliefs: Int = (ratioBiased * size).intValue // Calculate number of biased beliefs
    val biasBeliefs: List[Node[String]] =
      network.vertices.take(nrBiasBeliefs).toList // Determine which vertices become biased beliefs
    val nrTrueBias: Int =
      (ratioBiasTrueFalse * nrBiasBeliefs).intValue // Determine number of '''true''' biases
    val biasTruthValues: List[Boolean] = Random.shuffle(
      List.fill(nrTrueBias)(true) ++ List.fill(nrBiasBeliefs - nrTrueBias)(false)
    ) // Generate truth-values
    val biasAssignment: Map[Node[String], Boolean] =
      (biasBeliefs zip biasTruthValues).toMap // Assign truth values to biased beliefs
    val biasWeights: Map[Node[String], Double] =
      biasBeliefs.map((v: Node[String]) => v -> Random.nextDouble()).toMap // Generate bias weights

    new BiasedBeliefNetwork(
      network,
      negativeConstraints,
      biasBeliefs.toSet,
      biasAssignment,
      biasWeights
    )
  }

  /** Generate a belief network from a fixed number of vertices, edges and negative constraints with
    * edges having random (uniformly drawn) weights between 0 and 1. Additionally chooses a fixed
    * number of biased nodes and (semi-)randomly assigns a fixed number of false truth-values, the
    * rest will be true truth-biased, and randomly assigns weights between 0 and 1.
    *
    * @param size
    *   The number of vertices in the network
    * @param nrEdges
    *   The number of edges in the network
    * @param nrNegativeEdges
    *   The number of negative constraints in the belief network where The number of negative
    *   constraints must be smaller than or equal to the total number of edges
    * @param nrBiasBeliefs
    *   The number of biased beliefs in the network
    * @param nrFalseBiases
    *   The number of biased beliefs that are biased to be false
    * @return
    *   A randomly generated BiasedBeliefNetwork
    */
  def random(
      size: Int,
      nrEdges: Int,
      nrNegativeEdges: Int,
      nrBiasBeliefs: Int,
      nrFalseBiases: Int
      //              we: Option[Double] = None,
  ): BiasedBeliefNetwork = {
    assert(nrEdges <= size * size)
    assert(nrNegativeEdges <= nrEdges)
    assert(nrBiasBeliefs <= size)
    assert(nrFalseBiases <= nrBiasBeliefs)

    // Remove edge candidates that result int self-edges or are reversed duplicates of other edge candidates
    def noDuplicateEdges(endpoints: (Int, Int)): Boolean = {
      endpoints._1 < endpoints._2
    }

    // Generate vertex and edge candidates (candidates stay as Int for easier processing)
    val vertexCandidates: Set[Int] = (0 until size).toSet
    val edgeCandidates: Set[(Int, Int)] =
      (vertexCandidates x vertexCandidates).filter(noDuplicateEdges)

    // Generate vertices and edges
    val vertices: Set[Node[String]] = vertexCandidates.map("N" + _).map(Node(_))
    val edges: Set[WUnDiEdge[Node[String]]] = edgeCandidates
      .take(nrEdges)
      .map((e: (Int, Int)) => ("N" + e._1, "N" + e._2))
      .map((e: (String, String)) => (Node(e._1), Node(e._2)))
      .map((e: (Node[String], Node[String])) => WUnDiEdge(e._1, e._2, Random.nextDouble()))

    // Determine which edges are negative
    val negativeConstraints: Set[WUnDiEdge[Node[String]]] = edges.take(nrNegativeEdges)

    // TODO: determine whether this is stupid
    val biasBeliefs: List[Node[String]] = Random
      .shuffle(vertices.toList)
      .take(nrBiasBeliefs) // Determine which vertices are biased beliefs

    val biasTruthValues: List[Boolean] = Random.shuffle(
      List.fill(nrBiasBeliefs - nrFalseBiases)(true) ++ List.fill(nrFalseBiases)(false)
    ) // Generate truth-values
    val biasAssignment: Map[Node[String], Boolean] =
      (biasBeliefs zip biasTruthValues).toMap // Assign truth values to biased beliefs
    val biasWeights: Map[Node[String], Double] =
      biasBeliefs.map((v: Node[String]) => v -> Random.nextDouble()).toMap // Generate bias weights

    // Generate graph and belief network
    val network: WUnDiGraph[String] = WUnDiGraph(vertices, edges)
    new BiasedBeliefNetwork(
      network,
      negativeConstraints,
      biasBeliefs.toSet,
      biasAssignment,
      biasWeights
    )
  }
}
