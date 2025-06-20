package com.computationalcognitivescience.coherencecommunication.coherence

import mathlib.graph.Node

/**
 * Helper object to relabel Node[String] to Belief for easier interpretation.
 */
object Belief {
  type Belief = Node[String]

  /**
   * Factory method to construct a Belief.
   * @param label The label of the belief.
   * @return The belief.
   */
  def apply(label: String): Belief = Node(label)
}