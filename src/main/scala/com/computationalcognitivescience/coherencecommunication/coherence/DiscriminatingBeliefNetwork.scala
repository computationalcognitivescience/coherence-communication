package com.computationalcognitivescience.coherencecommunication.coherence

import mathlib.graph.{Node, WUnDiEdge}
import mathlib.set.SetTheory._

class BiasedBeliefNetwork(
    vertices: Set[Node[String]],
    positiveConstraints: Set[WUnDiEdge[Node[String]]],
    negativeConstraints: Set[WUnDiEdge[Node[String]]],
    biasBeliefs: Set[Node[String]],
    biasAssignment: Map[Node[String], Boolean],
    biasWeights: Map[Node[String], Double]
) extends BeliefNetwork(vertices, positiveConstraints, negativeConstraints) {
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
