package com.computationalcognitivescience.coherencecommunication.coherence

import mathlib.graph.{Node, WUnDiEdge, WUnDiGraph}
import mathlib.set.SetTheory._

class MultiBiasedBeliefNetwork(
    network: WUnDiGraph[String],
    negativeConstraints: Set[WUnDiEdge[Node[String]]],
    val multiBeliefBiases: Seq[BeliefBias]
) extends BeliefNetwork(network, negativeConstraints) {

  override def coh(
      assignment: Map[Node[String], Boolean]
  ): Double =
    cohPlus(assignment) + cohMin(assignment) +
      multiBeliefBiases
        .map(beliefBias => cohBias(assignment, beliefBias))
        .sum

  def allOptimalTruthValueAssignments: Set[Map[Node[String], Boolean]] =
    (vertices allMappings Set(true, false)) // Generate all possible truth-value assignments
      .argMax(coh)                          // Take all that maximize coh()

  private def cohBias(
      assignment: Map[Node[String], Boolean],
      beliefBias: BeliefBias
  ): Double = {
    def consistentWithBias(belief: Node[String]): Double = {
      if (
        assignment.isDefinedAt(belief) &&
        beliefBias.beliefs.contains(belief) &&
        assignment(belief) == beliefBias.beliefs(belief)
      )
        beliefBias.weights.getOrElse(belief, 0.0)
      else 0.0
    }

    sum(beliefBias.beliefs, consistentWithBias _)
  }



}
