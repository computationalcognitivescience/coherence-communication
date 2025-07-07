package com.computationalcognitivescience.coherencecommunication

import coherence.TruthValueAssignment
import coherence.TruthValueAssignment._
import com.computationalcognitivescience.coherencecommunication.ConversationData.ConversationData
import com.computationalcognitivescience.coherencecommunication.util.CSV
import mathlib.graph.WUnDiGraph
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
    initiatorPriorRatios: List[Double],
    responderPriorRatios: List[Double],
    priorsOverlapRatios: List[Double],
    priorsAsymmetryRatios: List[Double],
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
        initiatorPriorRatio           <- initiatorPriorRatios;
        responderPriorRatio           <- responderPriorRatios;
        priorOverlap                  <- priorsOverlapRatios;
        priorAsymmetry                <- priorsAsymmetryRatios;
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
      val agentPairId = (0 until numberOfSimulations).toList
      val batchConversationData: Seq[ConversationData] = agentPairId.par
        .map(id => {
          val randomGraph =
            WUnDiGraph.preferentialAttachment(parameters.beliefNetworkSize + 2, 2, 1.0)
          //          WUnDiGraph.uniform(
          //            n = parameters.beliefNetworkSize,
          //            numberEdges =
          //              (parameters.beliefNetworkSize * parameters.beliefNetworkConstraintsRatio).intValue
          //          )
          val negativeConstraints = scala.util.Random
            .shuffle(randomGraph.edges.toSeq)
            .take((randomGraph.size * parameters.beliefNetworkPCRatio).intValue)
            .toSet

          val initiatorOwnBeliefs = Random
            .shuffle(randomGraph.vertices.toSeq)
            .take((randomGraph.vertices.size * parameters.initiatorPriorRatio).intValue)
            .map(belief => (belief, Random.nextBoolean()))
            .toMap
            .toTruthValueAssignment
          val initiatorCommunicativeIntent = Random
            .shuffle((randomGraph.vertices \ initiatorOwnBeliefs.beliefs).toSeq)
            .take(
              (randomGraph.vertices.size * parameters.initiatorCommunicativeIntentRatio).intValue
            )
            .map(belief => (belief, Random.nextBoolean()))
            .toMap
            .toTruthValueAssignment

          val initiator = Initiator(
            randomGraph,
            negativeConstraints,
            initiatorOwnBeliefs,
            sharedBeliefs = TruthValueAssignment.emtpy,
            initiatorCommunicativeIntent,
            maxUtteranceLength = Some(parameters.maxUtteranceLength)
          )

          val initiatorPriorVertices = initiatorOwnBeliefs.beliefs.toSeq
          val responderOverlappingPriorVertices = Random
            .shuffle(initiatorPriorVertices)
            .take((initiatorPriorVertices.size * parameters.priorsOverlapRatio).intValue)

          val responderOverlappingSymmetricOwnBeliefs = Random
            .shuffle(responderOverlappingPriorVertices)
            .take(
              (responderOverlappingPriorVertices.size * parameters.priorsAsymmetryRatio).intValue
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
            .shuffle(randomGraph.vertices.toSeq)
            .take((randomGraph.vertices.size * parameters.responderPriorRatio).intValue)
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
            sharedBeliefs = TruthValueAssignment.emtpy,
            maxUtteranceLength = Some(parameters.maxUtteranceLength)
          )

          val conversation = Conversation(
            initiator,
            responder,
            parameters.maxRoundLength
          )
          conversation.simulate()
        })
        .toList

      val batchData = SimulationData(
        parameters = parameters,
        conversations = batchConversationData
      )

      os.write(
        dataFolderPath / s"part-${parameters.id}.json",
        batchData.asJson.toString(),
        createFolders = true
      )
    }
    println("Done simulating.")
  }
}

object Simulation {

  def mergeDatafileParts(dataFolderPath: Path): Unit = {
    val fileList = os.list(dataFolderPath).filter(_.toString().contains("part"))
    val outputFile = dataFolderPath / s"complete.json"
    println(s"Merging datafile parts into ${outputFile.toString()}...")
    val data: List[SimulationData] = fileList
      .map(dataFilePath => {
          decode[SimulationData](os.read.lines(dataFilePath).mkString).toOption
      })
      .filter(_.isDefined)
      .map(_.get)
      .toList
    os.write(outputFile, data.asJson.toString(), createFolders = true)
    println("Done.")
  }

  def main(args: Array[String]): Unit = {
    val dataDir        = os.pwd / "output"
    val dataFolderPath = dataDir / LocalDateTime.now().toEpochSecond(ZoneOffset.UTC).toString

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
    Simulation(
      beliefNetworkSizes = List(8),
      beliefNetworkConstraintsRatios = List(1.0 / 3.0, 2.0 / 3.0, 1.0),
      beliefNetworkPCRatios = List(.25, .5, .75, 1.0),
      intentionRatios = List(0.2),
      initiatorPriorRatios = List(0, .2, .4),
      responderPriorRatios = List(0, .2, .4),
      priorsOverlapRatios = List(.5),
      priorsAsymmetryRatios = List(.5),
      maxUtteranceLengths = List(5),
      maxRoundLengths = List(5),
      numberOfSimulations = 5
    ).run(dataFolderPath)

    mergeDatafileParts(dataFolderPath)

    CSV.jsonToCSV(dataFolderPath, "complete.json")

    println("Finished, exiting.")

////    println("\n===")
//    println(data.last._2.head.initiatorState.beliefNetwork.vertices)
////    println("Intent is: " + data.head._2.head.initiatorState.communicativeIntent)
//    val orderedData: Seq[ConversationData] = data.last._2.reverse
//
//    orderedData.head.initiatorState.allBeliefTruthValueAssignments.keySet.toList
//      .sortBy(_.label)
//      .foreach(node => {
//        val i    = orderedData.head.initiatorState.allBeliefTruthValueAssignments(node)
//        val r    = orderedData.head.responderState.allBeliefTruthValueAssignments(node)
//        val mark = if (i == r) "*" else ""
////        println(node + " i(" + i + ") r(" + r + ") " + mark)
//      })
//    orderedData.foreach(turn =>
//      println(
//        turn.round + "i: " + turn.utterance.getOrElse(
//          Map.empty
//        ) + "\n" + turn.round + "r: " + turn.restrictedOffer.getOrElse(Map.empty)
//      )
//    )
//    orderedData.last.initiatorState.allBeliefTruthValueAssignments.keySet.toList
//      .sortBy(_.label)
//      .foreach(node => {
//        val i    = orderedData.last.initiatorState.allBeliefTruthValueAssignments(node)
//        val r    = orderedData.last.responderState.allBeliefTruthValueAssignments(node)
//        val mark = if (i == r) "*" else ""
////        println(node + " i(" + i + ") r(" + r +") " + mark)
//      })
//
//    val dataDir  = os.pwd / "output"
//    val filename = "out" + LocalDateTime.now().toEpochSecond(ZoneOffset.UTC) + ".json"
//    os.write(
//      dataDir / filename,
//      upickle.default.write(data.map(bla => (bla._1, bla._2.map(_.toPicklableConversationData))))
//    )

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
}
