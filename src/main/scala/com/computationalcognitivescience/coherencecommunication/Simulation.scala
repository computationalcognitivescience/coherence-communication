package com.computationalcognitivescience.coherencecommunication

import coherence.TruthValueAssignment
import coherence.TruthValueAssignment._
import com.computationalcognitivescience.coherencecommunication.ConversationData.ConversationData
import com.computationalcognitivescience.coherencecommunication.util.CSV
import mathlib.graph.{WUnDiEdge, WUnDiGraph}
import mathlib.set.SetTheory._

import java.time.{LocalDateTime, ZoneOffset}
import scala.collection.parallel.CollectionConverters._
import scala.util._
import io.circe.generic.auto._
import io.circe.parser.decode
import io.circe.syntax._
import os.Path

case class Simulation(
    beliefNetworkSizes: List[Int],
    beliefNetworkConstraintsRatios: List[Double],
    beliefNetworkPCRatios: List[Double],
    intentionRatios: List[Double],
    initiatorOwnBeliefsRatios: List[Double],
    responderOwnBeliefsRatios: List[Double],
    ownBeliefsOverlapRatios: List[Double],
    ownBeliefsAsymmetryRatios: List[Double],
    maxUtteranceLengths: List[Int],
    maxRoundLengths: List[Int],
    numberOfSimulations: Int
) {

  def run(dataFolderPath: Path): Unit = {

    val allPar =
      for (
        beliefNetworkSize             <- beliefNetworkSizes;
        beliefNetworkConstraintsRatio <- beliefNetworkConstraintsRatios;
        beliefNetworkPCRatio          <- beliefNetworkPCRatios;
        intentionRatio                <- intentionRatios;
        initiatorPriorRatio           <- initiatorOwnBeliefsRatios;
        responderPriorRatio           <- responderOwnBeliefsRatios;
        priorOverlap                  <- ownBeliefsOverlapRatios;
        priorAsymmetry                <- ownBeliefsAsymmetryRatios;
        maxUtteranceLength            <- maxUtteranceLengths;
        maxRoundLength                <- maxRoundLengths
      ) yield {
        (
          beliefNetworkSize,
          beliefNetworkConstraintsRatio,
          beliefNetworkPCRatio,
          intentionRatio,
          initiatorPriorRatio,
          responderPriorRatio,
          priorOverlap,
          priorAsymmetry,
          maxUtteranceLength,
          maxRoundLength
        )
      }

    // Hack to set id without global var
    val allParameters = (allPar.indices zip allPar)
      .map(tmp =>
        Parameters(
          tmp._1.intValue,
          tmp._2._1,
          tmp._2._2,
          tmp._2._3,
          tmp._2._4,
          tmp._2._5,
          tmp._2._6,
          tmp._2._7,
          tmp._2._8,
          tmp._2._9,
          tmp._2._10
        )
      )
      .sortBy(_.id)

    // Create batches

    // Parallelize computations

    println(s"Created ${allParameters.size} batches of $numberOfSimulations agent pairs.")
    println("Starting simulation.")
    for (parameters <- allParameters) {
      println(s"Batch ${parameters.id} / ${allParameters.size}")
      val agentPairId = (1 to numberOfSimulations).toList
      val batchConversationData: Seq[ConversationData] = agentPairId//.par
        .map(id => {
          val conversation: Conversation = Simulation.randomConversation(
            parameters.beliefNetworkSize,
            preferentialAttachementM = 2,
            parameters.beliefNetworkPCRatio,
            parameters.initiatorPriorRatio,
            parameters.initiatorCommunicativeIntentRatio,
            parameters.maxUtteranceLength,
            parameters.priorsOverlapRatio,
            parameters.priorsAsymmetryRatio,
            parameters.responderPriorRatio,
            parameters.maxRoundLength
          )
          conversation.simulate()
        })
        .toList

      val batchData = SimulationData(
        parameters = parameters,
        conversations = batchConversationData
      )
      /*
      Write full simulation to JSON.
       */
//      os.write(
//        dataFolderPath / s"part-${parameters.id}.json",
//        batchData.asJson.toString(),
//        createFolders = true
//      )
      /*
      Write summary to CSV.
       */
      CSV.simulationDataToCVSV(batchData, dataFolderPath, dataFilename = "summary.csv")
    }
    println("Done simulating.")
  }
}

object Simulation {

  def main(args: Array[String]): Unit = {
    val dataDir        = os.pwd / "output"
    val dataFolderPath = dataDir / LocalDateTime.now().toEpochSecond(ZoneOffset.UTC).toString

    /*
    Large Simulation settings.
     */
//    Simulation(
//      beliefNetworkSizes = List(10),
//      beliefNetworkConstraintsRatios = List(1.0 / 3.0, 2.0 / 3.0, 1.0),
//      beliefNetworkPCRatios = List(.25, .5, .75, 1.0),
//      intentionRatios = List(0.2, 0.4),
//      initiatorPriorRatios = List(0, .2, .4),
//      responderPriorRatios = List(0, .2, .4),
//      priorsOverlapRatios = List(0, .5, 1),
//      priorsAsymmetryRatios = List(0, .5, 1),
//      maxUtteranceLengths = List(3, 5),
//      maxRoundLengths = List(5),
//      numberOfSimulations = 10
//    ).run(dataDir)
    /*
   Medium simulation settings.
     */
//    Simulation(
//      beliefNetworkSizes = List(10),
//      beliefNetworkConstraintsRatios = List(1.0 / 3.0, 2.0 / 3.0, 1.0),
//      beliefNetworkPCRatios = List(.25, .5, .75),
//      intentionRatios = List(.2, .4),
//      initiatorOwnBeliefsRatios = List(0, .2, .4),
//      responderOwnBeliefsRatios = List(0, .2, .4),
//      ownBeliefsOverlapRatios = List(0, .5, 1),
//      ownBeliefsAsymmetryRatios = List(0, .5, 1),
//      maxUtteranceLengths = List(5),
//      maxRoundLengths = List(5),
//      numberOfSimulations = 10
//    ).run(dataFolderPath)
//    /*
//    Small simulation settings.
//     */
    Simulation(
      beliefNetworkSizes = List(8),
      beliefNetworkConstraintsRatios = List(1.0), // not used in preferential attachement
      beliefNetworkPCRatios = List(.25, .5, .75),
      intentionRatios = List(3.0/8, 4.0/8),
      initiatorOwnBeliefsRatios = List(0, 1.0/8, 2.0/8, 3.0/8),
      responderOwnBeliefsRatios = List(0, 1.0/8, 2.0/8, 3.0/8),
      ownBeliefsOverlapRatios = List(0, .5, 1),
      ownBeliefsAsymmetryRatios = List(0, .5, 1),
      maxUtteranceLengths = List(5),
      maxRoundLengths = List(5),
      numberOfSimulations = 5
    ).run(dataFolderPath)

//    Uncomment line below to merge all json files into a single json file. Memory intensive.
//    JSON.mergeDatafileParts(dataFolderPath)

    println("Finished, exiting.")

//    os.write(dataDir/filename,
//      """
//        |<!DOCTYPE html>
//        |<meta charset="utf-8">
//        |<body>
//        |<script src="//d3js.org/d3.v7.min.js"></script>
//        |<script src="https://unpkg.com/@hpcc-js/wasm@2.20.0/dist/graphviz.umd.js"></script>
//        |<script src="https://unpkg.com/d3-graphviz@5.6.0/build/d3-graphviz.js"></script>
//        |<div id="graph" style="text-align: center;"></div>
//        |<div>
//        |    <button onclick="prev()">Previous</button>
//        |    <button onclick="next()">Next</button>
//        |</div>
//        |<script>
//        |
//        |var dotIndex = 0;
//        |var graphviz = d3.select("#graph").graphviz()
//        |    .engine("fdp")
//        |    .transition(function () {
//        |        return d3.transition("main")
//        |            //.ease(d3.easeLinear)
//        |            //.duration(1500)
//        |            .delay(0);
//        |    })
//        |    .logEvents(true)
//        |    .on("initEnd", render);
//        |
//        |function render() {
//        |    var dotLines = dots[dotIndex];
//        |    var dot = dotLines.join('');
//        |    graphviz
//        |        .renderDot(dot);
//        |}
//        |
//        |function next() {
//        |    dotIndex = (dotIndex + 1) % dots.length;
//        |    render();
//        |}
//        |
//        |function prev() {
//        |    dotIndex = (dotIndex - 1) % dots.length;
//        |    render();
//        |}
//        |
//        |var dots = [
//        |""".stripMargin
//    )
//    orderedData.foreach(round => {
//      val r = round.round
//      val initiator = round.initiatorState
//      val responder = round.responderState
//      val utterance = "Initiator said: " + round.utterance.getOrElse("")
//      val request = "Responder asks: " + round.repair.getOrElse("")
//      os.write.append(dataDir / filename, "[\n'graph G {',\n'label="+r+"',\n")
//      os.write.append(dataDir / filename, ("sub" + initiator.toDOTString(utterance)).split("\n").mkString("'","',\n'","',\n"))
//      os.write.append(dataDir / filename, ("sub" + responder.toDOTString(request)).split("\n").mkString("'","',\n'","',\n"))
//      os.write.append(dataDir / filename, "'}',\n],\n")
//    })
//    os.write.append(dataDir / filename, "];\n</script>")

  }

  def randomConversation(
      beliefNetworkSize: Int,
      preferentialAttachementM: Int,
      beliefNetworkPCRatio: Double,
      initiatorPriorRatio: Double,
      initiatorCommunicativeIntentRatio: Double,
      maxUtteranceLength: Int,
      priorsOverlapRatio: Double,
      priorsAsymmetryRatio: Double,
      responderPriorRatio: Double,
      maxRoundLength: Int
  ): Conversation = {
    val randomGraph1 =
      WUnDiGraph.preferentialAttachment(beliefNetworkSize + 2, preferentialAttachementM, 1.0)
    val randomGraph = WUnDiGraph(
      randomGraph1.vertices,
      randomGraph1.edges.map(edge => WUnDiEdge(edge.left, edge.right, 1.0))
    )
    //          WUnDiGraph.uniform(
    //            n = parameters.beliefNetworkSize,
    //            numberEdges =
    //              (parameters.beliefNetworkSize * parameters.beliefNetworkConstraintsRatio).intValue
    //          )
    val negativeConstraints = scala.util.Random
      .shuffle(randomGraph.edges.toSeq)
      .take((randomGraph.edges.size * beliefNetworkPCRatio).intValue)
      .toSet

    val initiatorOwnBeliefs = Random
      .shuffle(randomGraph.vertices.toSeq)
      .take((randomGraph.vertices.size * initiatorPriorRatio).intValue)
      .map(belief => (belief, Random.nextBoolean()))
      .toMap
      .toTruthValueAssignment
    val initiatorCommunicativeIntent = Random
      .shuffle((randomGraph.vertices \ initiatorOwnBeliefs.beliefs).toSeq)
      .take(
        (randomGraph.vertices.size * initiatorCommunicativeIntentRatio).intValue
      )
      .map(belief => (belief, Random.nextBoolean()))
      .toMap
      .toTruthValueAssignment

    val initiator = Initiator(
      randomGraph,
      negativeConstraints,
      initiatorOwnBeliefs,
      sharedBeliefs = TruthValueAssignment.empty,
      initiatorCommunicativeIntent,
      maxUtteranceLength = Some(maxUtteranceLength)
    )

    val initiatorPriorVertices = initiatorOwnBeliefs.beliefs.toSeq
    val responderOverlappingPriorVertices = Random
      .shuffle(initiatorPriorVertices)
      .take((initiatorPriorVertices.size * priorsOverlapRatio).intValue)

    val responderOverlappingSymmetricOwnBeliefs = Random
      .shuffle(responderOverlappingPriorVertices)
      .take(
        (responderOverlappingPriorVertices.size * priorsAsymmetryRatio).intValue
      )
      .map(belief => (belief, initiatorOwnBeliefs(belief).get))
      .toMap
      .toTruthValueAssignment
    val responderOverlappingAsymmetricOwnBeliefs =
      (initiatorPriorVertices.toSet \ responderOverlappingSymmetricOwnBeliefs.beliefs)
        .map(belief => (belief, !initiatorOwnBeliefs(belief).get))
        .toMap
        .toTruthValueAssignment
    val responderNonOverlappingOwnBeliefs = Random
      .shuffle((randomGraph.vertices \ responderOverlappingPriorVertices.toSet).toSeq)
      .take((randomGraph.vertices.size * responderPriorRatio).intValue - responderOverlappingPriorVertices.size)
      .map(belief => (belief, Random.nextBoolean()))
      .toMap
      .toTruthValueAssignment
    val responderOwnBeliefs = responderOverlappingSymmetricOwnBeliefs ++
      responderOverlappingAsymmetricOwnBeliefs ++
      responderNonOverlappingOwnBeliefs

    val responder = Responder(
      randomGraph,
      negativeConstraints,
      responderOwnBeliefs,
      sharedBeliefs = TruthValueAssignment.empty,
      maxUtteranceLength = Some(maxUtteranceLength)
    )

    Conversation(
      initiator,
      responder,
      maxRoundLength
    )
  }
}
