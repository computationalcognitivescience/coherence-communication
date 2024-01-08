package com.computationalcognitivescience.coherencecommunication

import coherence._
import mathlib.graph._
import mathlib.set.SetTheory._

class Interpreter(
    val beliefNet: BeliefNetwork,
    val priorBeliefs: Map[Node[String], Boolean],
    private val utteranceLength: Option[Int] = None
) extends Interlocutor(beliefNet, priorBeliefs) {


  val maxUtteranceLength: Int =
    if (utteranceLength.isDefined) utteranceLength.get else beliefNet.vertices.size

  /** Based on (van Arkel, 2021, p. 22)
    *
    * @param beliefNetwork
    *   A network of beliefs
    * @param inferredBeliefs
    *   A truth-value assignment over all beliefs (inferred + prior + communicated)
    * @param communicationHistory
    *   A truth-value assignment over communicated beliefs
    * @param previousCoherence
    *   The coherence value of the previous truth-value assignment
    * @return
    *   A repair request (truth-value assignment over nodes) if the new coherence is lower than the
    *   previous one.
    */
  def troubleIdentification(
      inferredBeliefs: Map[Node[String], Boolean],
      communicationHistory: Map[Node[String], Boolean],
      previousCoherence: Double,
      beliefNetwork: BeliefNetwork = this.beliefNet
  ): Map[Node[String], Boolean] = {
    val currentCoherence = beliefNetwork.coh(inferredBeliefs) // Calculate current coherence
    // If current coherence is lower than previous coherence, formulate a repair request
    if (currentCoherence < previousCoherence)
      repairFormulation(beliefNetwork, inferredBeliefs, communicationHistory)
    // If current coherence is equal to or higher than previous coherence, all is well :) (do nothing)
    else
      Map.empty
  }

  /** Based on (van Arkel, 2021, p. 24) Generate a repair request (truth-value assignment)
    *
    * @param beliefNetwork
    *   A network of beliefs
    * @param inferredBeliefs
    *   A truth-value assignment over all beliefs (inferred + prior + communicated)
    * @param communicationHistory
    *   A truth-value assignment over communicated beliefs
    * @return
    *   The the most efficient set of truth values for which a flip maximizes coherence
    */
  private def repairFormulation(
      beliefNetwork: BeliefNetwork = this.beliefNet,
      inferredBeliefs: Map[Node[String], Boolean],
      communicationHistory: Map[Node[String], Boolean]
  ): Map[Node[String], Boolean] = {
    assert(beliefNetwork.vertices == inferredBeliefs.keySet)

    // calculate V_request = V \ V_communicated
    val vRequest: Set[Node[String]] =
      inferredBeliefs.keySet -- communicationHistory.keySet

    // Generate all possible repair requests
    val allPossibleRequests: Set[Map[Node[String], Boolean]] = {
      powersetUp(
        vRequest,
        maxUtteranceLength
      ) // Take the upperbounded powerset over beliefs that may be requested
        // Map each set of beliefs to the truth-value mapping opposite of its current truth-value mapping
        .map((vRequest: Set[Node[String]]) =>
          vRequest // Take the set of requested beliefs
            .map((node: Node[String]) =>
              (node, !inferredBeliefs(node))
            ) // Flip the truth-value assignment
            .toMap
        )
    }

    // Calculate the normalized coherence scores
    val repairRequestValues: Set[(Map[Node[String], Boolean], Double)] = {
      allPossibleRequests
        .map((request: Map[Node[String], Boolean]) =>
          // Overwrite those mappings in inferredBeliefs with the request
          // and calculate the coherence
          (request, beliefNetwork.coh(inferredBeliefs ++ request) / request.size)
        )
    }
    // Get the best repair request
    repairRequestValues
      .argMax(_._2)
      .random
      .get
      ._1
  }

}
