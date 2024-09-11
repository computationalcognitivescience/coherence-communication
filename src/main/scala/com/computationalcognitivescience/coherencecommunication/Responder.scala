package com.computationalcognitivescience.coherencecommunication

import coherence._
import mathlib.graph._
import mathlib.set.SetTheory._

class Responder(
    beliefNetwork: FoundationalBeliefNetwork,
    priorBeliefs: Map[Node[String], Boolean],
    previousState: Option[Interlocutor] = None,
    communicatedBeliefs: Map[Node[String], Boolean] = Map.empty,
    maxUtteranceLength: Option[Int] = None
) extends Interlocutor(
      beliefNetwork,
      priorBeliefs,
  previousState,
      communicatedBeliefs,
      maxUtteranceLength
    ) {


  /** Based on (van Arkel, 2021, p. 22)
    * @return
    *   A repair request (truth-value assignment over nodes) if the new coherence is lower than the
    *   previous one.
    */
  def troubleIdentification(
      previousState: Interlocutor
  ): Map[Node[String], Boolean] = {
    val previousCoherence = previousState.beliefNetwork.coh(previousState.allBeliefTruthValueAssignments)
    val currentCoherence = beliefNetwork.coh(allBeliefTruthValueAssignments) // Calculate current coherence
    // If current coherence is lower than previous coherence, formulate a repair request
    if (currentCoherence < previousCoherence)
      repairFormulation()
    // If current coherence is equal to or higher than previous coherence, all is well :) (do nothing)
    else
      Map.empty
  }

  /** Based on (van Arkel, 2021, p. 24) Generate a repair request (truth-value assignment)
    * @return
    *   The the most efficient set of truth values for which a flip maximizes coherence
    */
  private def repairFormulation(): Map[Node[String], Boolean] = {
    // calculate V_request = V \ V_communicated
    val vRequest: Set[Node[String]] =
      allBeliefTruthValueAssignments.keySet -- communicatedBeliefs.keySet

    // Generate all possible repair requests
    val allPossibleRequests: Set[Map[Node[String], Boolean]] = {
      powersetUp(
        vRequest,
        utteranceLengthLimit
      ) // Take the upperbounded powerset over beliefs that may be requested
        // Map each set of beliefs to the truth-value mapping opposite of its current truth-value mapping
        .filterNot(_.isEmpty) // Remove empty
        .map((vRequest: Set[Node[String]]) =>
          vRequest // Take the set of requested beliefs
            .map((node: Node[String]) =>
              (node, !allBeliefTruthValueAssignments(node))
            ) // Flip the truth-value assignment
            .toMap
        )
    }

    // Get the best repair request
    allPossibleRequests
      .argMax(repairRequest =>
        beliefNetwork.coh(repairRequest) / (repairRequest.size + 1.0)
      )
      .random
      .get
  }

  override def addCommunicatedBeliefs(utterance: Map[Node[String], Boolean]): Responder =
    new Responder(
      beliefNetwork,
      priorBeliefs,
      Some(this),
      communicatedBeliefs ++ utterance,
      maxUtteranceLength
    )
}
