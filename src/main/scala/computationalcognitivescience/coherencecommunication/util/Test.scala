package computationalcognitivescience.coherencecommunication.util

import computationalcognitivescience.coherencecommunication.{
  Conversation,
  Initiator,
  Responder,
  Simulation
}
import computationalcognitivescience.coherencecommunication.coherence.{
  BeliefNetwork,
  BiasedBeliefNetwork,
  FoundationalBeliefNetwork,
  MaxFlow,
  MinCut,
  TruthValueAssignment
}
import computationalcognitivescience.coherencecommunication.coherence.TruthValueAssignment.ImplMap
import mathlib.graph.GraphImplicits.{EdgeImpl2, N, WUnDiEdgeImpl}
import mathlib.graph.{Node, WDiEdge, WDiGraph, WUnDiEdge, WUnDiGraph}
import mathlib.set.SetTheory._

import java.time.{LocalDateTime, ZoneOffset}
import scala.util.Random

object Test {
  def main(args: Array[String]): Unit = {

    /** FPT stuff */
    //    val _g = WDiGraph.preferentialAttachment(size = 10, m = 2)
    //    val g = WDiGraph(_g.vertices, _g.edges.map(e => WDiEdge(e.left, e.right, math.round(e.weight*10.0)/10.0)))
    //    val s = g.vertices.random.get
    //    val t = (g.vertices - s).random.get
    //    println(g.toDOTString)
    //    println(s"s:\t\t $s")
    //    println(s"t:\t\t $t")
    //    MaxFlow.shortestPaths(g, s, t)

    val _randomGraph =
      WUnDiGraph.preferentialAttachment(12, 3, 1.0)
    val randomGraph = WUnDiGraph(
      _randomGraph.vertices,
      _randomGraph.edges.map(edge => WUnDiEdge(edge.left, edge.right, 1.0))
    )

    val negativeConstraints = scala.util.Random
      .shuffle(randomGraph.edges.toSeq)
      .take((randomGraph.edges.size * 0.2).intValue)
      .toSet

    val biasedBeliefAssignment = Random
      .shuffle(randomGraph.vertices.toSeq)
      .take((randomGraph.vertices.size * 0.2).intValue)
      .map(belief => (belief, Random.nextBoolean()))
      .toMap
      .toTruthValueAssignment

    val bn = BeliefNetwork(
      randomGraph,
      negativeConstraints,
      biasedBeliefAssignment.beliefs,
      biasedBeliefAssignment
    )

    val data = bn.cMin()
//    println(MinCut.minCut(data.head.graph))

    val testGraph = WUnDiGraph(
      Set(
        "a" ~ "b" % 2,
        "a" ~ "e" % 3,
        "b" ~ "c" % 3,
        "b" ~ "e" % 2,
        "b" ~ "f" % 2,
        "c" ~ "d" % 4,
        "c" ~ "g" % 2,
        "d" ~ "g" % 2,
        "d" ~ "h" % 2,
        "e" ~ "f" % 3,
        "f" ~ "g" % 1,
        "g" ~ "h" % 3
      )
    )

    MinCut.minCut(
      testGraph
    ).foreach(cut => {
      println(cut)
      println(" => " + MinCut.minCutValue(testGraph, cut._1, cut._2))
    })


    val dataDir  = os.pwd / "output"
    val filename = LocalDateTime.now().toEpochSecond(ZoneOffset.UTC).toString + ".html"
//TODO Visualize ac1, ac2 and ac3 steps.
//    os.write(
//      dataDir / filename,
//      s"""
//            |<!DOCTYPE html>
//            |<meta charset="utf-8">
//            |<body>
//            |<script src="https://unpkg.com/d3@7.9.0/dist/d3.min.js"></script>
//            |<script src="https://unpkg.com/@hpcc-js/wasm@2.20.0/dist/graphviz.umd.js"></script>
//            |<script src="https://unpkg.com/d3-graphviz@5.6.0/build/d3-graphviz.js"></script>
//            |<div id="leftgraph" style="text-align: center;">
//            |</div>
//            |<div id="rightgraph" style="text-align: center;"></div>
//            |<div id="index"></div>
//            |<div>
//            |    <button onclick="prev()">Previous</button>
//            |    <button onclick="next()">Next</button>
//            |</div>
//            |<script>
//            |
//            |var dotIndex = 0;
//            |var rightgraphviz = d3.select("#rightgraph").graphviz()
//            |    .engine("fdp")
//            |    .transition(function () {
//            |        return d3.transition("main")
//            |            //.ease(d3.easeLinear)
//            |            //.duration(1500)
//            |            .delay(0);
//            |    })
//            |    .logEvents(true)
//            |    .on("initEnd", render);
//            |var leftgraphviz = d3.select("#leftgraph").graphviz()
//            |    .engine("fdp")
//            |    .transition(function () {
//            |        return d3.transition("main")
//            |            .delay(0);
//            |    })
//            |    .logEvents(true)
//            |    .on("initEnd", render);
//            |
//            |function render() {
//            |    var dotLines = dots[dotIndex];
//            |    var dot = dotLines.join('');
//            |    var ldot = leftDot.join('');
//            |    rightgraphviz
//            |        .renderDot(dot);
//            |    leftgraphviz
//            |        .renderDot(ldot);
//            |    d3.select("#index").text((dotIndex+1) + "/" + (dots.length));
//            |}
//            |
//            |function next() {
//            |    dotIndex = (dotIndex + 1) % dots.length;
//            |    render();
//            |}
//            |
//            |function prev() {
//            |    if(dotIndex - 1 < 0) {
//            |      dotIndex = dots.length - 1;
//            |    } else {
//            |      dotIndex = (dotIndex - 1) % dots.length;
//            |    }
//            |    render();
//            |}
//            |
//            |var leftDot = ${bn.toDOTString.split("\n").mkString("['", "',\n'", "']\n")}
//            |
//            |var dots = [
//            |""".stripMargin,
//      createFolders = true
//    )
//    data.foreach(network => {
//      os.write.append(
//        dataDir / filename,
//        network.toDOTString.split("\n").mkString("['", "',\n'", "'],\n")
//      )
//    })
//    os.write.append(dataDir / filename, "];\n</script>")
//    os.write.append(dataDir / filename, "<body></html>")

    /** Simulation stuff */
//    val t1 = TruthValueAssignment(Set(N("a"), N("b")), Set(N("a") -> true, N("b") -> false))
//    val t2 = TruthValueAssignment(
//      Set(N("a"), N("b"), N("c")),
//      Set(N("a") -> true, N("b") -> true, N("c") -> true)
//    )
//    println(t2.subAssignment(t1.beliefs))

//    println(t1 ~ t2)
//    println(
//      1.0 - (t1 ~ t2 /
//        t1.beliefs.size.doubleValue)
//    )
//
//    println("t1 ++ t2:\t" + (t1 ++ t2))
//    println("t2 ++ t1:\t" + (t2 ++ t1))
//    println("t2 -- Set(N(\"a\")):\t" + (t2 -- Set(N("a"))))
//    println("t2 \\ t1:\t" + t2 \ t1)

//    val c = Conversation.random(
//      beliefNetworkSize = 8,
//      preferentialAttachementM = 2,
//      beliefNetworkPCRatio = .5,
//      initiatorPriorRatio = 3.0/8,
//      initiatorCommunicativeIntentRatio = 3.0/8,
//      maxUtteranceLength = 5,
//      priorsOverlapRatio = 2 / 3.0,
//      priorsAsymmetryRatio = .5,
//      responderPriorRatio = 3.0/8,
//      maxRoundLength = 6
//    )
//    c.simulate().reverse.foreach(println)

//    val graph = WUnDiGraph.preferentialAttachment(10, 2, 1.0)
//    val negativeConstraints = scala.util.Random
//      .shuffle(graph.edges.toSeq)
//      .take((graph.size * .5).intValue)
//      .toSet
//
//    val initiatorOwnBeliefs = Random
//      .shuffle(graph.vertices.toSeq)
//      .take((graph.vertices.size * .2).intValue)
//      .map(belief => (belief, Random.nextBoolean()))
//      .toMap
//      .toTruthValueAssignment
////    val initiatorCommunicativeIntent = Random
////      .shuffle((graph.vertices \ initiatorOwnBeliefs.beliefs).toSeq)
////      .take(
////        (graph.vertices.size * .3).intValue
////      )
////      .map(belief => (belief, Random.nextBoolean()))
////      .toMap
////      .toTruthValueAssignment
//
//    val responder = Responder(
//      graph,
//      negativeConstraints,
//      initiatorOwnBeliefs,
//      sharedBeliefs = TruthValueAssignment.empty,
//      maxUtteranceLength = Some(5)
//    )
//    println(responder.sharedBeliefs)
//    println(responder.addSharedBeliefs(TruthValueAssignment(Set(N("V1")), Set(N("V1") -> true))).sharedBeliefs)
  }
}
