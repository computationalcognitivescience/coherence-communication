package com.computationalcognitivescience.coherencecommunication

import com.computationalcognitivescience.coherencecommunication.coherence.BeliefNetwork
import mathlib.set.SetTheory._
import mathlib.graph._

class Producer(
                beliefNet: BeliefNetwork,
                priorBeliefs: Map[Node[String], Boolean],
                communicativeIntent: Map[Node[String], Boolean]
              ) extends Interlocutor(beliefNet, priorBeliefs) {

  val inferredBeliefs: Map[Node[String], Boolean] = beliefRevision(beliefNet, priorBeliefs, communicativeIntent)

  def communicateBeliefs(
                          beliefNet: BeliefNetwork = this.beliefNet,
                          priorBeliefs: Map[Node[String], Boolean] = this.priorBeliefs,
                          communicativeIntent: Map[Node[String], Boolean] = this.communicativeIntent,
                          communicationHistory: Map[Node[String], Boolean]
                        ): Map[Node[String], Boolean] = {
    assert(priorBeliefs.keySet /\ communicativeIntent.keySet == Set.empty)

    // TODO: Ask mark about including the already communicated history
    val allPossibleUtterances: Set[Map[Node[String], Boolean]] =
      powerset(beliefNet.vertices -- communicationHistory.keySet)
        // Map each set of beliefs to its current truth-value mapping
        .map((utterance: Set[Node[String]]) =>
          utterance // Take the set of beliefs
            .map((node: Node[String]) => (node, inferredBeliefs(node)))
            .toMap
        )

    // Simulate the inferred beliefs of the interpreter
    val simulatedAssignments: Set[(Map[Node[String], Boolean], Int)] = allPossibleUtterances
      .map((utterance: Map[Node[String], Boolean]) =>
        // Collect tuples of [Utterance, Inferred Beliefs]
        (utterance, {
          val simulatedBeliefs = beliefRevision(beliefNet, priorBeliefs, communicationHistory ++ utterance)
          sim(inferredBeliefs, simulatedBeliefs, communicativeIntent.keySet)
        })
      )

    def sim(assignment: Map[Node[String], Boolean], otherAssignment: Map[Node[String], Boolean], V: Set[Node[String]]): Int = {
      V.toList.map((v: Node[String]) =>
        if (assignment(v) == otherAssignment(v)) 1
        else 0).sum
    }

    simulatedAssignments
      .argMax(_._2)
      .random
      .get
      ._1
  }

  def repairSolution(
                      beliefNet: BeliefNetwork = this.beliefNet,
                      priorBeliefs: Map[Node[String], Boolean] = this.priorBeliefs,
                      communicativeIntent: Map[Node[String], Boolean] = this.communicativeIntent,
                      communicationHistory: Map[Node[String], Boolean],
                      repairRequest: Map[Node[String], Boolean]
                    ): Map[Node[String], Boolean] = {
    assert(repairRequest.keySet /\ communicationHistory.keySet == Set.empty)

    val correctRequest: Boolean =
      repairRequest.keySet
        .forall((belief: Node[String]) =>
          repairRequest(belief) == inferredBeliefs(belief))

    if (correctRequest) Map.empty
    else {
      repairRequest.keySet
        .map((belief: Node[String]) =>
          (belief, inferredBeliefs(belief))
        ).toMap
    }
  }

  def endConversation(
                       communicativeIntent: Map[Node[String], Boolean] = this.communicativeIntent,
                       communicationHistory: Map[Node[String], Boolean],
                      repairRequest: Map[Node[String], Boolean]
                     ): Boolean = {
    val interpreterBeliefs = beliefRevision(beliefNet, priorBeliefs, communicationHistory)

    val allBeliefsCommunicated: Boolean = {
      communicativeIntent.keySet
        .forall((belief: Node[String]) =>
          interpreterBeliefs(belief) == communicativeIntent(belief)
        )
    }

    allBeliefsCommunicated && repairRequest.isEmpty
  }
}
