package com.computationalcognitivescience.coherencecommunication.util

import com.computationalcognitivescience.coherencecommunication.{Initiator, Responder}
import com.computationalcognitivescience.coherencecommunication.coherence.TruthValueAssignment
import com.computationalcognitivescience.coherencecommunication.coherence.TruthValueAssignment.ImplMap
import mathlib.graph.GraphImplicits.N
import mathlib.graph.WUnDiGraph
import mathlib.set.SetTheory._

import scala.util.Random

object Test {
  def main(args: Array[String]): Unit = {
    val t1 = TruthValueAssignment(Set(N("a"), N("b")), Set(N("a") -> true, N("b") -> false))
    val t2 = TruthValueAssignment(Set(N("a"), N("b"), N("c")), Set(N("a") -> true, N("b") -> false, N("c") -> true))
    println(t1 ~ t2)
    println(1.0 - (t1 ~ t2 /
      t1.beliefs.size.doubleValue))


    val graph = WUnDiGraph.preferentialAttachment(10, 2, 1.0)
    val negativeConstraints = scala.util.Random
      .shuffle(graph.edges.toSeq)
      .take((graph.size * .5).intValue)
      .toSet

    val initiatorOwnBeliefs = Random
      .shuffle(graph.vertices.toSeq)
      .take((graph.vertices.size * .2).intValue)
      .map(belief => (belief, Random.nextBoolean()))
      .toMap
      .toTruthValueAssignment
//    val initiatorCommunicativeIntent = Random
//      .shuffle((graph.vertices \ initiatorOwnBeliefs.beliefs).toSeq)
//      .take(
//        (graph.vertices.size * .3).intValue
//      )
//      .map(belief => (belief, Random.nextBoolean()))
//      .toMap
//      .toTruthValueAssignment

    val responder = Responder(
      graph,
      negativeConstraints,
      initiatorOwnBeliefs,
      sharedBeliefs = TruthValueAssignment.emtpy,
      maxUtteranceLength = Some(5)
    )
    println(responder.sharedBeliefs)
    println(responder.addSharedBeliefs(TruthValueAssignment(Set(N("V1")), Set(N("V1") -> true))).sharedBeliefs)
  }
}
