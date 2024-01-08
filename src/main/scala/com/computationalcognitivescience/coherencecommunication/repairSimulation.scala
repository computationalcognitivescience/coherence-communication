package com.computationalcognitivescience.coherencecommunication

import mathlib.graph._
import coherence._
class repairSimulation (
                       producer: Producer,
                       interpreter: Interpreter
                       ){
  /**
   *
   * @return
   */
  def run(): Any = {
    var nrRounds = 0

    var intentCommunicated: Boolean = false
    var communicationHistory: Map[Node[String], Boolean] = Map.empty
    var interpreterCoherence = interpreter.beliefNet.coh(
      interpreter.beliefNet.fCoherence(interpreter.priorBeliefs))
    var utterance: Map[Node[String], Boolean] = producer.communicateBeliefs(communicationHistory) // The first utterance

    // Go until the producer ends the conversation
    while(!intentCommunicated){
      // Update the communication history
      communicationHistory = communicationHistory ++ utterance

      // Interpreter revise beliefs based on updated communication history
      var inferredBeliefs: Map[Node[String], Boolean] = interpreter.beliefRevision(communicationHistory)

      // Interpreter identify trouble and create repair request
      var repairRequest: Map[Node[String], Boolean] = interpreter.troubleIdentification(inferredBeliefs, communicationHistory, interpreterCoherence)

      // If there is a repair request (Trouble)
      if (repairRequest.nonEmpty){
        // Get repair solution
        // If the repair request is faulty, it will be corrected here, otherwise the same repair request is kept
        var repairFormulation: Map[Node[String], Boolean] = producer.repairSolution(repairRequest, communicationHistory)

        // Update the interpreter's coherence
        interpreterCoherence = interpreter.beliefNet.coh(
          interpreter.beliefNet.fCoherence(interpreter.priorBeliefs ++ communicationHistory ++ repairFormulation))

        // New utterance is equal to the repair solution
        utterance = repairFormulation
      }
      // If there is no repair request (No trouble)
      else{
        // Update the interpreter's coherence
        interpreterCoherence = interpreter.beliefNet.coh(
          interpreter.beliefNet.fCoherence(interpreter.priorBeliefs ++ communicationHistory))

        // Check if all nodes have been communicated
        intentCommunicated = producer.endConversation(communicationHistory, repairRequest)

        //  Generate new utterance
        utterance = producer.communicateBeliefs(communicationHistory)
      }

      nrRounds += 1
      // TODO: Check Van Arkel's thesis for what simulation results should be kept track of
    }

    (producer.inferredBeliefs, interpreter.beliefRevision(communicationHistory), nrRounds)
    // Return relevant simulation data
  }

}
