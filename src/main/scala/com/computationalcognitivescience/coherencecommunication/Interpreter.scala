package com.computationalcognitivescience.coherencecommunication

import mathlib.graph._
class Interpreter(
                 priorBeliefs: Map[Node[String], Boolean]
                 ) extends Interlocutor(priorBeliefs){

  def beliefRevision(
                      utterance: Map[Node[String], Boolean]
                    ): Map[Node[String], Boolean] = {
    ???
  }

  def troubleIdentification(): Map[Node[String], Boolean] = {
    ???
  }

  def repairFormulation(): Map[Node[String], Boolean] = {
    ???
  }

}
