package com.computationalcognitivescience.coherencecommunication

class repairSimulation (
                       producer: Producer,
                       interpreter: Interpreter
                       ){
  def run(): Any = {
    var intentCommunicated: Boolean = false

    // producer DO
      // utterance = CommunicateBeliefs

    // While(!intentCommunicated) DO
      // interpreter DO
        // BeliefRevision

        // Trouble Identification

        // If(Trouble) DO
          // Repair Formulation
        // Else
          // Nothing

      // producer DO
        // if (Trouble)
          // Repair Solution
        // Else
          // End Conversation (check)

        // if (conversation ended)
          // intentCommunicated = true

        // Else
          // utterance = CommunicateBeliefs

    // TODO: Check Van Arkel's thesis for what simulation results should be kept track of
  }

}
