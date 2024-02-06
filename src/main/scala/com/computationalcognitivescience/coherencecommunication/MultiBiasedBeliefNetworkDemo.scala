package com.computationalcognitivescience.coherencecommunication

import com.computationalcognitivescience.coherencecommunication.coherence.Simulation
import mathlib.set.SetTheory._

object MultiBiasedBeliefNetworkDemo {
  def main(args: Array[String]): Unit = {

    // Run the conversations for all agent pairs with different parameter conditions
    Simulation.run()

  }
}
