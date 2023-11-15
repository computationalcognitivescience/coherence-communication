package com.computationalcognitivescience.coherencecommunication.coherence

import mathlib.graph.{Node, WUnDiEdge, WUnDiGraph}
import mathlib.set.SetTheory._

class BiasedBeliefNetwork(
    network: WUnDiGraph[String],
    negativeConstraints: Set[WUnDiEdge[Node[String]]],
    biasBeliefs: Set[Node[String]],
    biasAssignment: Map[Node[String], Boolean],
    biasWeights: Map[Node[String], Double]
) extends BeliefNetwork(network, negativeConstraints) {

  protected def cohBias(assignment: Map[Node[String], Boolean]): Double = {
    def biasWeight(belief: Node[String]): Double =
      if (assignment(belief) == biasAssignment(belief)) biasWeights(belief)
      else 0.0

    sum(biasBeliefs, biasWeight _)
  }

  override def coh(
      assignment: Map[Node[String], Boolean]
  ): Double =
    cohPlus(assignment) + cohMin(assignment) + cohBias(assignment)
}
