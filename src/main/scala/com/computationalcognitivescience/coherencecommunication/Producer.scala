package com.computationalcognitivescience.coherencecommunication

import mathlib.graph._

class Producer(
                communicativeIntent: Map[Node[String], Boolean],
                priorBeliefs: Map[Node[String], Boolean]
              ) extends Interlocutor(priorBeliefs) {

  def communicateBeliefs(
                          communicationHistory: Map[Node[String], Boolean]
                        ): Map[Node[String], Boolean] = {
    ???
  }

  def repairSolution(
                      communicationHistory: Map[Node[String], Boolean],
                      repairRequest: Map[Node[String], Boolean]
                    ): Map[Node[String], Boolean] = {
    ???
  }

  def endConversation(): Boolean = {
    ???
  }
}
