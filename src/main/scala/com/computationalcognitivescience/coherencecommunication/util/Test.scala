package com.computationalcognitivescience.coherencecommunication.util

import com.computationalcognitivescience.coherencecommunication.{Initiator, Responder, Simulation}
import com.computationalcognitivescience.coherencecommunication.coherence.{
  FoundationalBeliefNetwork,
  TruthValueAssignment
}
import com.computationalcognitivescience.coherencecommunication.coherence.TruthValueAssignment.ImplMap
import mathlib.graph.GraphImplicits.N
import mathlib.graph.{WUnDiEdge, WUnDiGraph}
import mathlib.set.SetTheory._

import scala.util.Random

object Test {
  def main(args: Array[String]): Unit = {
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


//
//    val c = Simulation.randomConversation(
//      beliefNetworkSize = 8,
//      preferentialAttachementM = 2,
//      beliefNetworkPCRatio = .5,
//      initiatorPriorRatio = .25,
//      initiatorCommunicativeIntentRatio = 3.0/8,
//      maxUtteranceLength = 5,
//      priorsOverlapRatio = 1 / 3.0,
//      priorsAsymmetryRatio = .5,
//      responderPriorRatio = .25,
//      maxRoundLength = 6
//    )
//    c.simulate()//.reverse.foreach(println)

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
