package com.computationalcognitivescience.coherencecommunication

import com.computationalcognitivescience.coherencecommunication.coherence.{
  BeliefNetwork,
  FoundationalBeliefNetwork
}
import mathlib.graph.WUnDiGraph
import mathlib.set.SetTheory._

import scala.util._

case class Simulation(
    beliefNetworkSizes: List[Int],
    beliefNetworkPCRatios: List[Double],
    intentionRatios: List[Double],
    initiatorPriorRatios: List[Double],
    responderPriorRatios: List[Double],
    maxUtteranceLengths: List[Int],
    maxRoundLengths: List[Int],
    numberOfSimulations: Int
) {
  def run(): Map[Parameters, Seq[ConversationData]] = {
    (for (
      beliefNetworkSize <- beliefNetworkSizes;
      beliefNetworkPCRatio <- beliefNetworkPCRatios;
      intentionRatio <- intentionRatios;
      initiatorPriorRatio <- initiatorPriorRatios;
      responderPriorRatio <- responderPriorRatios;
      maxUtteranceLength <- maxUtteranceLengths;
      maxRoundLength <- maxRoundLengths;
      n <- 0 until numberOfSimulations
    ) yield {
      val randomGraph =
        WUnDiGraph.preferentialAttachment(beliefNetworkSize, 2, 1.0)
      val negativeConstraints = scala.util.Random
        .shuffle(randomGraph.edges.toSeq)
        .take((randomGraph.size * beliefNetworkPCRatio).intValue)
        .toSet

      val initiatorPrior = Random
        .shuffle(randomGraph.vertices.toSeq)
        .take((randomGraph.vertices.size * initiatorPriorRatio).intValue)
        .map(belief => (belief, Random.nextBoolean()))
        .toMap
      val initiatorCommunicativeIntent = Random
        .shuffle((randomGraph.vertices \ initiatorPrior.keySet).toSeq)
        .take((randomGraph.vertices.size * intentionRatio).intValue)
        .map(belief => (belief, Random.nextBoolean()))
        .toMap

      val initiatorBeliefNetwork = new FoundationalBeliefNetwork(
        randomGraph,
        negativeConstraints,
        initiatorPrior.keySet \/ initiatorCommunicativeIntent.keySet,
        initiatorPrior ++ initiatorCommunicativeIntent
      )

      val initiator = new Initiator(
        initiatorBeliefNetwork,
        initiatorPrior,
        initiatorCommunicativeIntent
      )

      val responderPrior = Random
        .shuffle(randomGraph.vertices.toSeq)
        .take((randomGraph.vertices.size * responderPriorRatio).intValue)
        .map(belief =>
          (belief, Random.nextBoolean())
        ) // TODO Make structural similarity a generative parameter property.
        .toMap

      val responderBeliefNetwork = new FoundationalBeliefNetwork(
        randomGraph,
        negativeConstraints,
        responderPrior.keySet,
        responderPrior
      )

      val responder = new Responder(
        responderBeliefNetwork,
        responderPrior
      )

      val conversation = Conversation(
        initiator,
        responder,
        maxRoundLength
      )
      val conversationData = conversation.simulate()
      Parameters(
        n,
        beliefNetworkSize,
        beliefNetworkPCRatio,
        intentionRatio,
        initiatorPriorRatio,
        responderPriorRatio,
        maxUtteranceLength,
        maxRoundLength
      ) -> conversationData
    }).toMap
  }
}

object Simulation {
  def main(args: Array[String]): Unit = {
    val data = Simulation(
      beliefNetworkSizes = List(10),
      beliefNetworkPCRatios = List(.5),
      intentionRatios = List(0.2),
      initiatorPriorRatios = List(0.2),
      responderPriorRatios = List(0.2),
      maxUtteranceLengths = List(3),
      maxRoundLengths = List(5),
      numberOfSimulations = 1
    ).run()

    println(data.last._2.head.initiatorState.beliefNetwork.vertices)
    data.last._2.head.initiatorState.allBeliefTruthValueAssignments.keySet.toList.sortBy(_.label)
      .foreach(node => {
        println(node + " i(" + data.last._2.last.initiatorState.allBeliefTruthValueAssignments(node) + ") r(" + data.last._2.last.responderState.allBeliefTruthValueAssignments(node) + ")")
      })
    data.last._2.reverse.foreach(turn => println(turn.round + "i: " + turn.utterance.getOrElse(Map.empty)+ "\n" + turn.round + "r: " + turn.repair.getOrElse(Map.empty)))
    data.last._2.last.initiatorState.allBeliefTruthValueAssignments.keySet.toList.sortBy(_.label)
      .foreach(node => {
        println(node + " i("+data.last._2.last.initiatorState.allBeliefTruthValueAssignments(node)+") r("+ data.last._2.last.responderState.allBeliefTruthValueAssignments(node)+")")
      })

  }
}
