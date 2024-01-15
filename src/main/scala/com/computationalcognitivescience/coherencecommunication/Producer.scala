package com.computationalcognitivescience.coherencecommunication

import com.computationalcognitivescience.coherencecommunication.coherence.BeliefNetwork
import mathlib.set.SetTheory._
import mathlib.graph._

//NOTES:
// PRIOR BELIEFS AND COMMUNICATIVE INTENT CAN OVERLAP
// WE ALLOW FOR EMPTY UTTERANCES BY DIVIDING BY UTTERANCE SIZE + 1
class Producer(
    val beliefNet: BeliefNetwork,
    val priorBeliefs: Map[Node[String], Boolean],
    val communicativeNodes: Set[Node[String]],
    private val utteranceLength: Option[Int] = None
) extends Interlocutor(beliefNet, priorBeliefs) {

  assert(communicativeNodes.forall(beliefNet.vertices.contains))

  val inferredBeliefs: Map[Node[String], Boolean] =
    beliefRevision(priorBeliefs, beliefNet, Map.empty)
  private val communicativeIntent: Map[Node[String], Boolean] =
    communicativeNodes
      .map((v: Node[String]) => v -> inferredBeliefs(v))
      .toMap
  val maxUtteranceLength: Int =
    if (utteranceLength.isDefined) utteranceLength.get else beliefNet.vertices.size

  /** Based on (van Arkel, 2021, p. 28)
    *
    * @param beliefNet
    *   A network of beliefs
    * @param priorBeliefs
    *   A truth-value assignment over prior beliefs
    * @param communicativeIntent
    *   A truth-value assignment over beliefs to be communicated
    * @param communicationHistory
    *   A truth-value assignment over all already communicated beliefs
    * @return
    *   A truth-value assignment over any set of non-prior, non-already communicated beliefs that
    *   will most efficiently communicate the intent
    */
  def communicateBeliefs(
      communicationHistory: Map[Node[String], Boolean],
      beliefNet: BeliefNetwork = this.beliefNet,
      priorBeliefs: Map[Node[String], Boolean] = this.priorBeliefs,
      communicativeIntent: Map[Node[String], Boolean] = this.communicativeIntent
  ): Map[Node[String], Boolean] = {
//    assert(priorBeliefs.keySet /\ communicativeIntent.keySet == Set.empty)

    val allPossibleUtterances: Set[Map[Node[String], Boolean]] =
      powersetUp(beliefNet.vertices -- communicationHistory.keySet, maxUtteranceLength)
        // Map each set of beliefs to its current truth-value mapping
        .filterNot(_.isEmpty) // Disallow empty utterance?
        .map((utterance: Set[Node[String]]) =>
          utterance // Take the set of beliefs
            .map((node: Node[String]) => (node, inferredBeliefs(node)))
            .toMap
        )

    // Simulate the inferred beliefs of the interpreter
    val simulatedAssignments: Set[(Map[Node[String], Boolean], Double)] = allPossibleUtterances
      .map((utterance: Map[Node[String], Boolean]) =>
        // Collect tuples of [Utterance, Inferred Beliefs]
        (
          utterance, {
            val simulatedBeliefs =
              beliefRevision(priorBeliefs, beliefNet, communicationHistory ++ utterance)
            structuralSim(inferredBeliefs, simulatedBeliefs, communicativeIntent.keySet) / (utterance.size.doubleValue + 1)
          }
        )
      )

    // Get utterance which will make the interpreter's
    // beliefs most similar to our communicative intent
    simulatedAssignments
      .argMax(_._2)
      .random
      .get
      ._1
  }

  /** Based on (van Arkel, 2021, p.31)
    *
    * @param beliefNet
    *   A network of beliefs
    * @param priorBeliefs
    *   A truth-value assignment over prior beliefs
    * @param communicativeIntent
    *   A truth-value assignment over beliefs to be communicated
    * @param communicationHistory
    *   A truth-value assignment over all already communicated beliefs
    * @param repairRequest
    *   Incoming truth-value assignment from the interpreter
    * @return
    *   A correction, in the form of a truth-value assignment, if the repairRequest is faulty
    *   (beliefs have been assigned wrong) or else the unadjusted repairRequest
    */
  def repairSolution(
      repairRequest: Map[Node[String], Boolean],
      communicationHistory: Map[Node[String], Boolean],
      beliefNet: BeliefNetwork = this.beliefNet,
      priorBeliefs: Map[Node[String], Boolean] = this.priorBeliefs,
      communicativeIntent: Map[Node[String], Boolean] = this.communicativeIntent
  ): Map[Node[String], Boolean] = {
    assert(repairRequest.keySet /\ communicationHistory.keySet == Set.empty)

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
    *
    * @param communicativeIntent
    *   A truth-value assignment over beliefs to be communicated
    * @param communicationHistory
    *   A truth-value assignment over all already communicated beliefs
    * @param repairRequest
    *   Incoming truth-value assignment from the interpreter
    * @return
    *   'true' if the producer beliefs the interpreter has taken over the communicativeIntent, and
    *   there is no repairRequest 'false' otherwise
    */
  def endConversation(
      communicationHistory: Map[Node[String], Boolean],
      repairRequest: Map[Node[String], Boolean],
      communicativeIntent: Map[Node[String], Boolean] = this.communicativeIntent
  ): Boolean = {
    val interpreterBeliefs = beliefRevision(priorBeliefs, beliefNet, communicationHistory)

    val allBeliefsCommunicated: Boolean = {
      // Infer if all nodes to be communicated have the correct truth-value assignment in the interpreter's network
      communicativeIntent.keySet
        .forall((belief: Node[String]) => interpreterBeliefs(belief) == communicativeIntent(belief))
    }

    allBeliefsCommunicated && repairRequest.isEmpty
  }

}
