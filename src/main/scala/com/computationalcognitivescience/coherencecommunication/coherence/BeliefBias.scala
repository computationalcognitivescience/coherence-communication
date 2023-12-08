package com.computationalcognitivescience.coherencecommunication.coherence

import mathlib.graph.Node

/** Simple representation for a belief bias for a (sub)set of beliefs.
  *
  * @param valueAssignment
  *   Represents the truth-value assignment for each belief.
  * @param weights
  *   Represents the weight bias for each belief.
  */
case class BeliefBias(
    valueAssignment: Map[Node[String], Boolean],
    weights: Map[Node[String], Double]
) {
  val beliefs: Set[Node[String]] = valueAssignment.keys.toSet
}

case object BeliefBias {

  /** Constructor that takes a single weight bias and applies it to all biased beliefs.
    * @param valueAssignment
    *   Represents the truth-value assignment for each belief.
    *
    * @param weight
    *   Represents the weight bias for each belief.
    *
    * @return
    *   A BeliefBias where all biased beliefs have the same weight.
    */
  def apply(valueAssignment: Map[Node[String], Boolean], weight: Double): BeliefBias = BeliefBias(
    valueAssignment,
    valueAssignment.keys.map(_ -> weight).toMap
  )
}
