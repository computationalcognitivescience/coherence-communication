package com.computationalcognitivescience.coherencecommunication

import com.computationalcognitivescience.coherencecommunication.coherence.{BeliefNetwork, FoundationalBeliefNetwork}
import mathlib.graph.WUnDiGraph
import mathlib.set.SetTheory._

import scala.util._

case class Simulation (
    beliefNetworkSize: Int,
    beliefNetworkPCRatio: Double,
    intentionSize: Int,
    priorSize: Int,
    maxUtteranceLength: Int,
    numberOfSimulations: Int
) {
  def run(): Seq[ConversationData] = {
    for(n <- 0 until numberOfSimulations) yield {
      val randomGraph =
        WUnDiGraph.preferentialAttachment(beliefNetworkSize, 2, 1.0)

      val initiatorPrior = Random.shuffle(randomGraph.vertices.toSeq)
        .take(intentionSize)
        .map(belief => (belief, Random.nextBoolean()))
        .toMap
      val initiatorCommunicativeIntent =  (randomGraph.vertices \ initiatorPrior.keySet)
        .map(belief => (belief, Random.nextBoolean()))
        .toMap

      val initiatorBeliefNetwork = new FoundationalBeliefNetwork(
        randomGraph,
        scala.util.Random.shuffle(randomGraph.edges.toSeq)
          .take((randomGraph.size * beliefNetworkPCRatio).intValue).toSet,
        initiatorPrior.keySet \/ initiatorCommunicativeIntent.keySet,
        initiatorPrior ++ initiatorCommunicativeIntent
      )

      val initiator = new Initiator(
        initiatorBeliefNetwork,
        initiatorPrior,
        initiatorCommunicativeIntent
      )


    }
  }


  def main(args: Array[String]): Unit = {
    Simulation(8, 0.5, 3, 2, 10).run()
  }
}
