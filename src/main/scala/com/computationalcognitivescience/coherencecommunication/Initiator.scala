package com.computationalcognitivescience.coherencecommunication

import com.computationalcognitivescience.coherencecommunication.coherence.{
  BeliefNetwork,
  FoundationalBeliefNetwork
}
import mathlib.set.SetTheory._
import mathlib.graph._

//NOTES:
// PRIOR BELIEFS AND COMMUNICATIVE INTENT CAN OVERLAP
// WE ALLOW FOR EMPTY UTTERANCES BY DIVIDING BY UTTERANCE SIZE + 1
class Initiator(
    beliefNetwork: FoundationalBeliefNetwork,
    priorBeliefs: Map[Node[String], Boolean],
    val communicativeIntent: Map[Node[String], Boolean],
    previousState: Option[Interlocutor] = None,
    communicatedBeliefs: Map[Node[String], Boolean] = Map.empty,
    maxUtteranceLength: Option[Int] = None
) extends Interlocutor(
      beliefNetwork,
      priorBeliefs ++ communicativeIntent,
      previousState,
      communicatedBeliefs,
      maxUtteranceLength
    ) {

  assert(
    communicativeIntent.keySet.forall(beliefNetwork.vertices.contains),
    "Communicative intent contains beliefs not present in the belief network."
  )

//  private val communicativeIntent: Map[Node[String], Boolean] =
//    communicativeIntent
//      .map((v: Node[String]) => v -> inferredBeliefs(v))
//      .toMap

  private def simulateBelieveInferences(
      utterance: Map[Node[String], Boolean]
  ): Initiator =
    addCommunicatedBeliefs(utterance)

  /** Based on (van Arkel, 2021, p. 28)
    *
    * @return
    *   A truth-value assignment over any set of non-prior, non-already communicated beliefs that
    *   will most efficiently communicate the intent
    */
  def produceUtterance(): Map[Node[String], Boolean] = {
//    assert(priorBeliefs.keySet /\ communicativeIntent.keySet == Set.empty)

    val allPossibleUtterances: Set[Map[Node[String], Boolean]] =
      powersetUp(
        beliefNetwork.vertices \ communicatedBeliefs.keySet,
        utteranceLengthLimit
      )
        // Map each set of beliefs to its current truth-value mapping
        .filterNot(_.isEmpty) // Disallow empty utterance?
        .map((utterance: Set[Node[String]]) =>
          utterance // Take the set of beliefs
            .map((node: Node[String]) => (node, inferredBeliefs(node)))
            .toMap
        )

    // Get utterance which will make the interpreter's
    // beliefs most similar to our communicative intent
    allPossibleUtterances
      .argMax(utterance =>
        structuralSimilarity(
          simulateBelieveInferences(utterance),
          communicativeIntent.keySet
        )
          / (utterance.size.doubleValue + 1)
      )
      .random
      .get
  }

  /** Based on (van Arkel, 2021, p.31)
    *
    * @param repairRequest
    *   Incoming truth-value assignment from the interpreter
    * @return
    *   A correction, in the form of a truth-value assignment, if the repairRequest is faulty
    *   (beliefs have been assigned wrong) or else the unadjusted repairRequest
    */
  def repairSolution(
      repairRequest: Map[Node[String], Boolean]
  ): Map[Node[String], Boolean] = {
    assert(
      repairRequest.keySet /\ communicatedBeliefs.keySet == Set.empty,
      "Repair request contains previously communicated beliefs, something went wrong."
    )

    val isCorrectRequest: Boolean =
      repairRequest.keySet
        .forall((belief: Node[String]) => repairRequest(belief) == inferredBeliefs(belief))

    if (isCorrectRequest) repairRequest
    else {
      repairRequest.keySet
        .map((belief: Node[String]) => (belief, inferredBeliefs(belief)))
        .toMap
    }
  }

  /** Based on (van Arkel, 2021, p.33)
    * @param repairRequest
    *   Incoming truth-value assignment from the interpreter
    * @return
    *   'true' if the producer beliefs the interpreter has taken over the communicativeIntent, and
    *   there is no repairRequest 'false' otherwise
    */
  def endConversation(
      repairRequest: Map[Node[String], Boolean]
  ): Boolean = {
    val allBeliefsCommunicated: Boolean = {
      val simulatedInterlocutor = simulateBelieveInferences(repairRequest)

      // Infer if all nodes to be communicated have the correct truth-value assignment in the interpreter's network

      communicativeIntent.keySet
        .forall(belief =>
          simulatedInterlocutor.allBeliefTruthValueAssignments(belief) == communicativeIntent(
            belief
          )
        )
    }

    allBeliefsCommunicated && repairRequest.isEmpty
  }

  override def addCommunicatedBeliefs(utterance: Map[Node[String], Boolean]): Initiator =
    new Initiator(
      beliefNetwork,
      priorBeliefs,
      communicativeIntent,
      Some(this),
      communicatedBeliefs ++ utterance,
      maxUtteranceLength
    )
}