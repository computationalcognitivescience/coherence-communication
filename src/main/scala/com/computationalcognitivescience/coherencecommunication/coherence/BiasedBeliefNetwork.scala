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

}
