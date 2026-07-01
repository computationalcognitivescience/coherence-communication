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
  TruthValueAssignment
}
import computationalcognitivescience.coherencecommunication.coherence.TruthValueAssignment.ImplMap
import mathlib.graph.GraphImplicits.N
import mathlib.graph.{WDiEdge, WDiGraph, WUnDiEdge, WUnDiGraph}
import mathlib.set.SetTheory._

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

    println(bn)
    bn.cMin().foreach(test => {
      println(test.biasAssignment)
    })

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
