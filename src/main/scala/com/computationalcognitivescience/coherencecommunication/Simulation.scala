package com.computationalcognitivescience.coherencecommunication

import com.computationalcognitivescience.coherencecommunication.coherence.{BeliefNetwork, FoundationalBeliefNetwork}
import mathlib.graph.WUnDiGraph
import mathlib.set.SetTheory._

import java.time.{LocalDateTime, ZoneOffset}
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

    println("\n===")
    println(data.last._2.head.initiatorState.beliefNetwork.vertices)
    println("Intent is: " + data.head._2.head.initiatorState.communicativeIntent)
    val orderedData: Seq[ConversationData] = data.last._2.reverse

    orderedData.head.initiatorState.allBeliefTruthValueAssignments.keySet.toList.sortBy(_.label)
      .foreach(node => {
        val i = orderedData.head.initiatorState.allBeliefTruthValueAssignments(node)
        val r = orderedData.head.responderState.allBeliefTruthValueAssignments(node)
        val mark = if(i==r) "*" else ""
        println(node + " i(" + i + ") r(" + r + ") " + mark)
      })
    orderedData.foreach(turn => println(turn.round + "i: " + turn.utterance.getOrElse(Map.empty)+ "\n" + turn.round + "r: " + turn.repair.getOrElse(Map.empty)))
    orderedData.last.initiatorState.allBeliefTruthValueAssignments.keySet.toList.sortBy(_.label)
      .foreach(node => {
        val i = orderedData.last.initiatorState.allBeliefTruthValueAssignments(node)
        val r = orderedData.last.responderState.allBeliefTruthValueAssignments(node)
        val mark = if (i == r) "*" else ""
        println(node + " i(" + i + ") r(" + r +") " + mark)
      })


    val dataDir = os.pwd/"output"
    val filename = "out"+LocalDateTime.now().toEpochSecond(ZoneOffset.UTC)+".html"
    os.write(dataDir/filename,
      """
        |<!DOCTYPE html>
        |<meta charset="utf-8">
        |<body>
        |<script src="//d3js.org/d3.v7.min.js"></script>
        |<script src="https://unpkg.com/@hpcc-js/wasm@2.20.0/dist/graphviz.umd.js"></script>
        |<script src="https://unpkg.com/d3-graphviz@5.6.0/build/d3-graphviz.js"></script>
        |<div id="graph" style="text-align: center;"></div>
        |<script>
        |
        |var dotIndex = 0;
        |var graphviz = d3.select("#graph").graphviz()
        |    .transition(function () {
        |        return d3.transition("main")
        |            .ease(d3.easeLinear)
        |            .delay(500)
        |            .duration(1500);
        |    })
        |    .logEvents(true)
        |    .on("initEnd", render);
        |
        |function render() {
        |    var dotLines = dots[dotIndex];
        |    var dot = dotLines.join('');
        |    graphviz
        |        .renderDot(dot)
        |        .on("end", function () {
        |            dotIndex = (dotIndex + 1) % dots.length;
        |            render();
        |        });
        |}
        |
        |var dots = [
        |""".stripMargin
    )
    orderedData.foreach(round => {
      os.write.append(dataDir / filename, "[\n'graph G {',\n'label="+round.round+"',\n")
      os.write.append(dataDir / filename, ("sub" + round.initiatorState.toDOTString).split("\n").mkString("'","',\n'","',\n"))
      os.write.append(dataDir / filename, ("sub" + round.responderState.toDOTString).split("\n").mkString("'","',\n'","',\n"))
      os.write.append(dataDir / filename, "'}',\n],\n")
    })
    os.write.append(dataDir / filename, "];\n</script>")

  }
}
