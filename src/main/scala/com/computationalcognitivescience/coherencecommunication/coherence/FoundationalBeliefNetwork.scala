package com.computationalcognitivescience.coherencecommunication.coherence

import mathlib.graph._
import mathlib.set.SetTheory._

import scala.collection.immutable
import scala.util.Random

class FoundationalBeliefNetwork(
    graph: WUnDiGraph[String],
    negativeConstraints: Set[WUnDiEdge[Node[String]]],
    val foundationalBeliefs: Set[Node[String]],
    val foundationalAssignment: Map[Node[String], Boolean]
) extends BeliefNetwork(graph, negativeConstraints) {

  override def coherence(): Map[Node[String], Boolean] =
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
  override def coherenceSolutions(): Set[Map[Node[String], Boolean]] = {
    // Get truth-value assignment over non-foundational nodes
    val notFoundationalBeliefs: Set[Node[String]] = graph.vertices -- foundationalBeliefs
    val otherAssignments: Set[Map[Node[String], Boolean]] =
      notFoundationalBeliefs allMappings Set(true, false)

    // Add foundational truth-value assignments
    val allAssignments: Set[Map[Node[String], Boolean]] =
      otherAssignments.map(_ ++ foundationalAssignment)

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
  private def isValidAssignment(assignment: Map[Node[String], Boolean]): Boolean = {

    // Check if foundational vertex has its required truth-value
    def isSatisfied(vertex: Node[String]): Boolean = {
      foundationalAssignment(vertex) == assignment(vertex)
    }

    foundationalBeliefs.forall(isSatisfied)
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
      unassignedMinus: Set[Node[String]] // All nodes incident to a negative constraint
  ): Set[Map[Node[String], Boolean]] =
    unassignedMinus.allMappings(Set(true, false)).map(_ ++ foundationalAssignment)
}

object RandomFoundationalBeliefNetwork {

  /** Generate a belief network from a fixed number of vertices, but a (semi) random number of edges
    * based on an edge density and a (semi) random number of negative edges. Additionally, (semi-)
    * randomly chooses foundational beliefs and assigns truth-values
    *
    * [KNOWN BUG]: Generated networks allow for duplicate edges (A, B) (B,A) and self edges (A, A)
    *
    * @param size
    *   The number of edges in the network
    * @param density
    *   The (expected) density of edges in the network
    * @param ratioPosNeg
    *   The target ratio of positive edges to negative edges
    * @param ratioFoundational
    *   The target ratio of foundational beliefs as opposed to non-foundational beliefs
    * @param ratioFoundationalTrueFalse
    *   The target ratio of True foundational beliefs as opposed to False foundational beliefs
    * @return
    */
  def random(
      size: Int,
      density: Double,
      ratioPosNeg: Double,
      ratioFoundational: Double,
      ratioFoundationalTrueFalse: Double
      //              we: Option[Double] = None,
  ): FoundationalBeliefNetwork = {
    assert(0 <= density && density <= 1)
    assert(0 <= ratioPosNeg && ratioPosNeg <= 1)
    assert(0 <= ratioFoundational && ratioFoundational <= 1)
    assert(0 <= ratioFoundationalTrueFalse && ratioFoundationalTrueFalse <= 1)

    // Generate the vertices and edges of the network
    val graph: WUnDiGraph[String]  = WUnDiGraph.random(size - 1, density)
    val nrNegativeConstraints: Int = (graph.edges.size * (1 - ratioPosNeg)).intValue
    val negativeConstraints: Set[WUnDiEdge[Node[String]]] =
      Random.shuffle(graph.edges.toList).take(nrNegativeConstraints).toSet

    // Generate foundational beliefs
    val nrFoundationalBeliefs: Int =
      (ratioFoundational * size).intValue // Calculate number of foundational beliefs
    val foundationalBeliefs: List[Node[String]] = graph.vertices
      .take(nrFoundationalBeliefs)
      .toList // Determine which vertices become foundational beliefs
    val nrTrueFoundational: Int =
      (ratioFoundationalTrueFalse * nrFoundationalBeliefs).intValue // Determine number of '''true''' foundational beliefs
    val foundationalTruthValues: List[Boolean] = Random.shuffle(
      List.fill(nrTrueFoundational)(true) ++ List.fill(nrFoundationalBeliefs - nrTrueFoundational)(
        false
      )
    ) // Generate truth-values
    val foundationalAssignment: Map[Node[String], Boolean] =
      (foundationalBeliefs zip foundationalTruthValues).toMap // Assign truth values to foundational beliefs

    new FoundationalBeliefNetwork(
      graph,
      negativeConstraints,
      foundationalBeliefs.toSet,
      foundationalAssignment
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
    * @param nrFoundationalBeliefs
    *   The number of foundational beliefs in the network
    * @param nrFalseFoundationalBeliefs
    *   The number of foundational beliefs that must be false
    * @return
    *   A randomly generated BiasedBeliefNetwork
    */
  def random(
      size: Int,
      nrEdges: Int,
      nrNegativeEdges: Int,
      nrFoundationalBeliefs: Int,
      nrFalseFoundationalBeliefs: Int
      //              we: Option[Double] = None,
  ): FoundationalBeliefNetwork = {
    assert(nrEdges <= size * size)
    assert(nrNegativeEdges <= nrEdges)
    assert(nrFoundationalBeliefs <= size)
    assert(nrFalseFoundationalBeliefs <= nrFoundationalBeliefs)

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
    val foundationalBeliefs: List[Node[String]] = Random
      .shuffle(vertices.toList)
      .take(nrFoundationalBeliefs) // Determine which vertices are biased beliefs

    val foundationalTruthValues: List[Boolean] = Random.shuffle(
      List.fill(nrFoundationalBeliefs - nrFalseFoundationalBeliefs)(true) ++ List.fill(
        nrFalseFoundationalBeliefs
      )(false)
    ) // Generate truth-values
    val foundationalAssignment: Map[Node[String], Boolean] =
      (foundationalBeliefs zip foundationalTruthValues).toMap // Assign truth values to biased beliefs

    // Generate graph and belief network
    val graph: WUnDiGraph[String] = WUnDiGraph(vertices, edges)
    new FoundationalBeliefNetwork(
      graph,
      negativeConstraints,
      foundationalBeliefs.toSet,
      foundationalAssignment
    )
  }
}
