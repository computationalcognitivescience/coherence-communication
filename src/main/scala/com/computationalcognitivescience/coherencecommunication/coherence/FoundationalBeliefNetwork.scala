package com.computationalcognitivescience.coherencecommunication.coherence

import mathlib.graph._
import mathlib.set.SetTheory._

import scala.util.Random
class FoundationalBeliefNetwork(
                     network: WUnDiGraph[String],
                     negativeConstraints: Set[WUnDiEdge[Node[String]]],
                     val foundationalBeliefs: Set[Node[String]],
                     val foundationalAssignment: Map[Node[String], Boolean]
                   ) extends BeliefNetwork(network, negativeConstraints) {

  override   def coherence(): Map[Node[String], Boolean] = {
    // Output
    // Get the truth-assignment that maximizes coherence
    val allAssignments =
    vertices allMappings Set(true, false) // Generate all possible truth-value assignments
    allAssignments
      .filter(isValidAssignment)
      .argMax(coh)
      .random
      .get // Return the truth-value assignment that maximizes coherence value
  }

  /** Check if truth-value assignment is valid (i.e. all foundational vertices have their required truth-value)
   *
   * @param assignment
   *  A truth-value assignment over vertices
   * @return
   *  True if all foundational nodes have their required truth-value assignment as given in
   *  foundationalAssignment, False otherwise
   */
  def isValidAssignment(assignment: Map[Node[String], Boolean]): Boolean = {

    // Check if foundational vertex has its required truth-value
    def isSatisfied(vertex: Node[String]): Boolean = {
      foundationalAssignment(vertex) == assignment(vertex)
    }

    foundationalBeliefs.forall(isSatisfied)
  }
}

object RandomFoundationalBeliefNetwork {
  /** Generate a belief network from a fixed number of vertices, but a (semi) random number of edges based
   * on an edge density and a (semi) random number of negative edges. Additionally, (semi-) randomly chooses
   * foundational beliefs and assigns truth-values
   *
   * [KNOWN BUG]: Generated networks allow for duplicate edges (A, B) (B,A) and self edges (A, A)
   *
   * @param size
   *  The number of edges in the network
   * @param density
   *  The (expected) density of edges in the network
   * @param ratioPosNeg
   *  The target ratio of positive edges to negative edges
   * @param ratioFoundational
   *  The target ratio of foundational beliefs as opposed to non-foundational beliefs
   * @param ratioFoundationalTrueFalse
   *  The target ratio of True foundational beliefs as opposed to False foundational beliefs
   * @return
   */
  def random(
              size: Int,
              density: Double,
              ratioPosNeg: Double,
              ratioFoundational: Double,
              ratioFoundationalTrueFalse: Double
              //              we: Option[Double] = None,
            ): FoundationalBeliefNetwork= {
    assert(0 <= density && density <= 1)
    assert(0 <= ratioPosNeg && ratioPosNeg <= 1)
    assert(0 <= ratioFoundational && ratioFoundational <= 1)
    assert(0 <= ratioFoundationalTrueFalse && ratioFoundationalTrueFalse <= 1)

    // Generate the vertices and edges of the network
    val network: WUnDiGraph[String] = WUnDiGraph.random(size-1, density)
    val nrNegativeConstraints: Int = (network.edges.size * (1 - ratioPosNeg)).intValue
    val negativeConstraints: Set[WUnDiEdge[Node[String]]] = Random.shuffle(network.edges.toList).take(nrNegativeConstraints).toSet

    // Generate foundational beliefs
    val nrFoundationalBeliefs: Int = (ratioFoundational*size).intValue // Calculate number of foundational beliefs
    val foundationalBeliefs: List[Node[String]] = network.vertices.take(nrFoundationalBeliefs).toList // Determine which vertices become foundational beliefs
    val nrTrueFoundational: Int = (ratioFoundationalTrueFalse*nrFoundationalBeliefs).intValue // Determine number of '''true''' foundational beliefs
    val foundationalTruthValues: List[Boolean] = Random.shuffle(List.fill(nrTrueFoundational)(true) ++ List.fill(nrFoundationalBeliefs - nrTrueFoundational)(false)) // Generate truth-values
    val foundationalAssignment: Map[Node[String], Boolean] = (foundationalBeliefs zip foundationalTruthValues).toMap // Assign truth values to foundational beliefs

    new FoundationalBeliefNetwork(network, negativeConstraints, foundationalBeliefs.toSet, foundationalAssignment)
  }

  /** Generate a belief network from a fixed number of vertices, edges and negative constraints
   * with edges having random (uniformly drawn) weights between 0 and 1. Additionally chooses a fixed number of biased nodes and
   * (semi-)randomly assigns a fixed number of false truth-values, the rest will be true truth-biased, and randomly assigns
   * weights between 0 and 1.
   *
   * @param size
   *  The number of vertices in the network
   * @param nrEdges
   *  The number of edges in the network
   * @param nrNegativeEdges
   *  The number of negative constraints in the belief network where
   *  The number of negative constraints must be smaller than or equal to the total number of edges
   * @param nrFoundationalBeliefs
   *  The number of foundational beliefs in the network
   * @param nrFalseFoundationalBeliefs
   *  The number of foundational beliefs that must be false
   * @return
   *  A randomly generated BiasedBeliefNetwork
   */
  def random(
              size: Int,
              nrEdges: Int,
              nrNegativeEdges: Int,
              nrFoundationalBeliefs: Int,
              nrFalseFoundationalBeliefs: Int
              //              we: Option[Double] = None,
            ): FoundationalBeliefNetwork = {
    assert(nrEdges <= size*size)
    assert(nrNegativeEdges <= nrEdges)
    assert(nrFoundationalBeliefs <= size)
    assert(nrFalseFoundationalBeliefs <= nrFoundationalBeliefs)

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
      .map((e: (Int, Int))                    => ("N" + e._1, "N" + e._2))
      .map((e: (String, String))              => (Node(e._1), Node(e._2)))
      .map((e: (Node[String], Node[String]))  => WUnDiEdge(e._1, e._2, Random.nextDouble()))

    // Determine which edges are negative
    val negativeConstraints: Set[WUnDiEdge[Node[String]]] = edges.take(nrNegativeEdges)

    // TODO: determine whether this is stupid
    val foundationalBeliefs: List[Node[String]] = Random.shuffle(vertices.toList).take(nrFoundationalBeliefs)// Determine which vertices are biased beliefs

    val foundationalTruthValues: List[Boolean] = Random.shuffle(List.fill(nrFoundationalBeliefs - nrFalseFoundationalBeliefs)(true) ++ List.fill(nrFalseFoundationalBeliefs)(false)) // Generate truth-values
    val foundationalAssignment: Map[Node[String], Boolean] = (foundationalBeliefs zip foundationalTruthValues).toMap // Assign truth values to biased beliefs

    // Generate graph and belief network
    val network: WUnDiGraph[String] = WUnDiGraph(vertices, edges)
    new FoundationalBeliefNetwork(network, negativeConstraints, foundationalBeliefs.toSet, foundationalAssignment)
  }
}
