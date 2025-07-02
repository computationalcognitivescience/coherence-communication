package com.computationalcognitivescience.coherencecommunication.coherence

import mathlib.graph._
import mathlib.set.SetTheory._

case class FoundationalBeliefNetwork(
    override val graph: WUnDiGraph[String],
    override val negativeConstraints: Set[WUnDiEdge[Node[String]]],
    foundationalBeliefs: Set[Node[String]],
    foundationalAssignment: Map[Node[String], Boolean]
) extends BaseBeliefNetwork {

  def addFoundationalAssignment(assignment: Map[Node[String], Boolean]): FoundationalBeliefNetwork =
    new FoundationalBeliefNetwork(
      graph,
      negativeConstraints,
      foundationalBeliefs ++ assignment.keySet,
      foundationalAssignment ++ assignment
    )

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

case object FoundationalBeliefNetwork {
  def random(
      size: Int,
      density: Double,
      ratioNegativeEdges: Double,
      ratioFoundationalBeliefs: Double,
      ratioFoundationalBeliefsAssignment: Double,
      weightUpperbound: Double = 1.0
  ): FoundationalBeliefNetwork = {
    require(
      0.0 <= density && density <= 1.0,
      s"Density $density is not between 0.0 and 1.0 inclusive."
    )
    require(
      0.0 <= ratioNegativeEdges && ratioNegativeEdges <= 1.0,
      s"Ratio negative edges $ratioNegativeEdges is not between 0.0 and 1.0 inclusive."
    )
    require(
      0.0 <= ratioFoundationalBeliefs && ratioFoundationalBeliefs <= 1.0,
      s"Ratio foundational beliefs $ratioFoundationalBeliefs is not between 0.0 and 1.0 inclusive."
    )

    val graph = WUnDiGraph.preferentialAttachment(
      size,
      scala.math.round(density * size).intValue,
      weightUpperbound
    )

    val foundationalBeliefs = scala.util.Random
      .shuffle(graph.vertices)
      .take(scala.math.round(graph.size * ratioFoundationalBeliefs).intValue)
    val (trueFoundation, falseFoundation) = scala.util.Random
      .shuffle(foundationalBeliefs)
      .splitAt(
        scala.math.round(foundationalBeliefs.size * ratioFoundationalBeliefsAssignment).intValue
      )

    FoundationalBeliefNetwork(
      graph,
      negativeConstraints = scala.util.Random
        .shuffle(graph.edges)
        .take(scala.math.round(graph.edges.size * ratioNegativeEdges).intValue),
      foundationalBeliefs,
      foundationalAssignment = trueFoundation.map(_ -> true).toMap ++ falseFoundation.map(_ -> false).toMap
    )
  }
}
