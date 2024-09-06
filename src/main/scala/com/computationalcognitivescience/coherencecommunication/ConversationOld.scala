package com.computationalcognitivescience.coherencecommunication

import mathlib.graph._

class ConversationOld(
                       producer: Initiator,
                       interpreter: Responder
                       ){
  /** Runs a communication repair simulation loop as described in Van Arkel (2021)
   *
   * @return
   *  A list of simulation data:
   *  1. The producer's final beliefs
   *  2. The Interpreter's final predicted beliefs
   *  3. The interpreter's final true beliefs
   *  4. The predicted structural similarity between beliefs
   *  5. The true structural similarity between beliefs
   *  6. The predicted structural similarity over the communicative intent
   *  7. The true structural similarity over the communicative intent
   *  8. The number of completed rounds
   */
  def run(): List[Any] = {
//    var nrRounds = 0
//
//    var intentCommunicated: Boolean = false
//
//    // Initialize relevant simulation variables
//    var communicationHistory: Map[Node[String], Boolean] = Map.empty // History of utterances
//    var interpreterCoherence = interpreter.beliefNet.coh(
//      interpreter.beliefNet.fCoherence(interpreter.priorBeliefs)) // Coherence of the interpreter's belief network
//    var interpreterBeliefs: Map[Node[String], Boolean] = Map.empty // The interpreter's (revised) beliefs
//    var repairRequest: Map[Node[String], Boolean] = Map.empty // The interpreter's repair request
//    var repairSolution: Map[Node[String], Boolean] = Map.empty // The producer's response to the repair request
//
//    // The first utterance
//    var utterance: Map[Node[String], Boolean] = producer.communicateBeliefs(communicationHistory)
//
//    // Go until the producer ends the conversation
//    while(!intentCommunicated || nrRounds == 6){
//      // Add the latest utterance to the communication history
//      communicationHistory = communicationHistory ++ utterance
//
//      // Interpreter revise beliefs based on updated communication history
//      interpreterBeliefs = interpreter.beliefRevision(communicationHistory)
//
//      // Interpreter identify trouble and create repair request
//      repairRequest = interpreter.troubleIdentification(interpreterBeliefs, communicationHistory, interpreterCoherence)
//
//      // If there is a repair request (Trouble)
//      if (repairRequest.nonEmpty){
//        // Get repair solution
//        // If the repair request is faulty, it will be corrected here, otherwise the same repair request is kept
//        repairSolution = producer.repairSolution(repairRequest, communicationHistory)
//
//        // Update the interpreter's coherence
//        interpreterCoherence = interpreter.beliefNet.coh(
//          interpreter.beliefRevision(communicationHistory ++ repairSolution))
//
//        // New utterance is equal to the repair solution
//        utterance = repairSolution
//      }
//      // If there is no repair request (No trouble)
//      else{
//        // Update the interpreter's coherence
//        interpreterCoherence = interpreter.beliefNet.coh(interpreterBeliefs)
//
//        // Check if all nodes have been communicated
//        intentCommunicated = producer.endConversation(communicationHistory, repairRequest)
//
//        //  Generate new utterance
//        utterance = producer.communicateBeliefs(communicationHistory)
//      }
//
//      nrRounds += 1
//      // TODO: Check Van Arkel's thesis for what simulation results should be kept track of
//    }
//
//    // Not captured: utterance length, coherence (over time), similarity over time
//
//    // Return relevant simulation data
//    val predictedInterpreterBeliefs = producer.beliefRevision(communicationHistory, producer.beliefNet, producer.priorBeliefs)
//    val finalPredictedSim = producer.structuralSim(producer.inferredBeliefs, predictedInterpreterBeliefs, producer.beliefNet.vertices)
//    val finalTrueSim = producer.structuralSim(producer.inferredBeliefs, interpreterBeliefs, producer.beliefNet.vertices)
//    val intentPredictedSim = producer.structuralSim(producer.inferredBeliefs, predictedInterpreterBeliefs, producer.communicativeNodes)
//    val intentTrueSim = producer.structuralSim(producer.inferredBeliefs, interpreterBeliefs, producer.communicativeNodes)
//
//    List(producer.inferredBeliefs, predictedInterpreterBeliefs, interpreterBeliefs, finalPredictedSim, finalTrueSim, intentPredictedSim, intentTrueSim, nrRounds)
    List()
  }
}
