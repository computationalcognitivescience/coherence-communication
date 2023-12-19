package com.computationalcognitivescience.coherencecommunication

import mathlib.graph._
import coherence.{BeliefNetwork, FoundationalBeliefNetwork}

class Interlocutor(
                    beliefNet: BeliefNetwork,
                    priorBeliefs: Map[Node[String], Boolean]
                  ) {

  /** Based on (van Arkel, 2021, p. 20)
   *
   * @param beliefNet
   * A network of beliefs
   * @param priorBeliefs
   * A truth-value assignment over prior beliefs
   * @param communicatedBeliefs
   * A truth-value assignment over communicated beliefs
   * @return
   * A truth value assignment over all nodes (inferred + prior + communicated) such that the prior and communicated beliefs are satisfied
   * (Communicated beliefs take precedence over prior beliefs) and coherence is maximized
   */
  def beliefRevision(
                      beliefNet: BeliefNetwork = this.beliefNet,
                      priorBeliefs: Map[Node[String], Boolean] = this.priorBeliefs,
                      communicatedBeliefs: Map[Node[String], Boolean], // utterance + previously communicated beliefs
                    ): Map[Node[String], Boolean] = {
    val foundationalAssignment = priorBeliefs ++ communicatedBeliefs // Nodes in priorBeliefs are overwritten by those in utterance
    val foundationalNet = new FoundationalBeliefNetwork( // Creating a foundational coherence instance
      beliefNet.graph, // Graph
      beliefNet.negativeConstraints, // negativeConstraints
      foundationalAssignment.keySet, // foundationalBeliefs
      foundationalAssignment) // foundationalAssignment
    foundationalNet.coherence() // Inferred beliefs
  }
}
