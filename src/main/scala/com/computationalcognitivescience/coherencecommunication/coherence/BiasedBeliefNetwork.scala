package com.computationalcognitivescience.coherencecommunication.coherence

import com.computationalcognitivescience.coherencecommunication.coherence.Belief.Belief
import mathlib.graph.{Node, WUnDiEdge, WUnDiGraph}
import mathlib.set.SetTheory._

import scala.util.Random

case class BiasedBeliefNetwork(
    override val graph: WUnDiGraph[String],
    override val negativeConstraints: Set[WUnDiEdge[Belief]],
    biasBeliefs: Set[Belief],
    biasAssignment: TruthValueAssignment,
    biasWeights: Map[Belief, Double]
) extends BaseBeliefNetwork {

  /** Calculate the coherence-value from biased beliefs with a given truth-value assignment
    *
    * @param assignment
    *   A truth-value assignment over vertices
    * @return
    *   The weighted sum over satisfied biased beliefs
    */
  protected def cohBias(assignment: TruthValueAssignment): Double = {

    /** Return the biased belief's weight if the belief is satisfied
      *
      * @param belief
      *   A vertex in the network, must be a biased belief
      * @return
      *   The weight of the bias if the belief's bias is satisfied, 0.0 otherwise
      */
    def biasWeight(belief: Belief): Double =
      if (assignment(belief) == biasAssignment(belief)) biasWeights(belief)
      else 0.0

    sum(biasBeliefs, biasWeight _)
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
      assignment: TruthValueAssignment
  ): Double =
    cohPlus(assignment) + cohMin(assignment) + cohBias(assignment)
}

case object BiasedBeliefNetwork {
  def random(
      size: Int,
      density: Double,
      ratioNegativeEdges: Double,
      ratioBiasBeliefs: Double,
      ratioBiasBeliefsAssignment: Double,
      weightUpperbound: Double = 1.0,
      biasBeliefsWeightUpperbound: Double = 1.0
  ): BiasedBeliefNetwork = {
    require(
      0.0 <= density && density <= 1.0,
      s"Density $density is not between 0.0 and 1.0 inclusive."
    )
    require(
      0.0 <= ratioNegativeEdges && ratioNegativeEdges <= 1.0,
      s"Ratio negative edges $ratioNegativeEdges is not between 0.0 and 1.0 inclusive."
    )
    require(
      0.0 <= ratioBiasBeliefs && ratioBiasBeliefs <= 1.0,
      s"Ratio bias beliefs $ratioBiasBeliefs is not between 0.0 and 1.0 inclusive."
    )
    require(
      0.0 <= ratioBiasBeliefsAssignment && ratioBiasBeliefsAssignment <= 1.0,
      s"Ratio bias beliefs assignment $ratioBiasBeliefsAssignment is not between 0.0 and 1.0 inclusive."
    )

    val graph = WUnDiGraph.preferentialAttachment(
      size,
      scala.math.round(density * size).intValue,
      weightUpperbound
    )

    val biasBeliefs = scala.util.Random
      .shuffle(graph.vertices)
      .take(scala.math.round(graph.size * ratioBiasBeliefs).intValue)
    val (trueBias, falseBias) = scala.util.Random.shuffle(biasBeliefs).splitAt(scala.math.round(biasBeliefs.size * ratioBiasBeliefsAssignment).intValue)

    val biasAssignment =  trueBias.map(_ -> true).toMap ++ falseBias.map(_ -> false).toMap

    BiasedBeliefNetwork(
      graph,
      negativeConstraints = scala.util.Random
        .shuffle(graph.edges)
        .take(scala.math.round(graph.edges.size * ratioNegativeEdges).intValue),
      biasBeliefs,
      biasAssignment = TruthValueAssignment(biasAssignment.keySet, biasAssignment.toSet),
      biasWeights = biasBeliefs.map(_ -> scala.util.Random.nextDouble()*biasBeliefsWeightUpperbound).toMap
    )
  }
}
