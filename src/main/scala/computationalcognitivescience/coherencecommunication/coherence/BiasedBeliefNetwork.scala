package computationalcognitivescience.coherencecommunication.coherence

import Belief.Belief
import mathlib.graph.{Node, WUnDiEdge, WUnDiGraph}
import mathlib.set.SetTheory._

import scala.util.Random

/** A biased belief network consisting of a regular belief network with additionally a set of biased
  * beliefs.
  * @param graph
  *   The weighted undirected graph representing the belief network.
  * @param negativeConstraints
  *   The negative constraints in the belief network.
  * @param biasBeliefs
  *   The set of biased beliefs.
  * @param biasAssignment
  *   The truth-value assignment for the biased beliefs.
  * @param biasWeights
  *   The weights for each biased belief.
  */
case class BiasedBeliefNetwork(
    override val graph: WUnDiGraph[String],
    override val negativeConstraints: Set[WUnDiEdge[Belief]],
    biasBeliefs: Set[Belief],
    biasAssignment: TruthValueAssignment,
    biasWeights: Map[Belief, Double]
) extends BaseBeliefNetwork with CMinusAlgorithm {

  /** Calculate the coherence-value from biased beliefs with a given truth-value assignment
    *
    * @param assignment
    *   A truth-value assignment over vertices
    * @return
    *   The weighted sum over satisfied biased beliefs
    */
  protected def cohBias(assignment: TruthValueAssignment): Double = {

    /* Return the biased belief's weight if the belief is satisfied */
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