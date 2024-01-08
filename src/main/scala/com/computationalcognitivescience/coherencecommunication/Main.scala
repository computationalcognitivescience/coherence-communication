package com.computationalcognitivescience.coherencecommunication

import mathlib.set.SetTheory._
import mathlib.graph._
import coherence._
import mathlib.graph.GraphImplicits.{EdgeImpl2, WDiEdgeImpl, WUnDiEdgeImpl}

import scala.util.Random

object Main {
  def main(args: Array[String]): Unit = {

    val vertices: Set[Node[String]] = Set(
      Node("Australia"),
      Node("winter"),
      Node("swimming"),
      Node("cold"),
      Node("outside"),
      Node("beach"))

    val edges: Set[WUnDiEdge[Node[String]]] = Set(
      "Australia" ~ "winter" % 1,
      "winter" ~ "swimming" % 1,
      "winter" ~ "cold" % 1,
      "swimming" ~ "cold" % 1,
      "swimming" ~ "outside" % 1,
      "cold" ~ "outside" % 1,
      "outside" ~ "beach" % 1,
    )

    val graph: WUnDiGraph[String] = WUnDiGraph(vertices, edges)

    val negConstraints: Set[WUnDiEdge[Node[String]]] = Set(
      "Australia" ~ "winter" % 1,
      "winter" ~ "swimming" % 1,
      "swimming" ~ "cold" % 1,
      "cold" ~ "outside" % 1,
    )

    val network = new BeliefNetwork(graph, negConstraints)

    val producer = new Producer(network,
      priorBeliefs = Map(
        Node("Australia") -> true,
        Node("winter") -> false,
        Node("swimming") -> true
      ),
      communicativeNodes = Set(
        Node("swimming"),
        Node("cold"),
        Node("beach")
      ))

    val interpreter = new Interpreter(network,
      priorBeliefs = Map(
        Node("Australia") -> false,
        Node("winter") -> true,
      ))

    val utterance: Map[Node[String], Boolean] = producer.communicateBeliefs(Map.empty)
    print("Utterance: ")
    println(utterance)

    val startingBeliefs = interpreter.beliefRevision(Map.empty)
    val revisedBeliefs = interpreter.beliefRevision(utterance)

    print("startingBeliefs: ")
    println(startingBeliefs)

    print("RevisedBeliefs: ")
    println(revisedBeliefs)

    val prevCoh = interpreter.beliefNet.coh(startingBeliefs)
    val newCoh =  interpreter.beliefNet.coh(revisedBeliefs)
    val trouble = interpreter.troubleIdentification(revisedBeliefs, utterance, prevCoh)

    print("prevCoh: ")
    print(prevCoh)
    print(", newCoh: ")
    println(newCoh)
    println(trouble)

//    val assignment = Map(
//      Node("A") -> true,
//      Node("B") -> false,
//      Node("C") -> true,
//    )
//    println(assignment)
//
//    val newAssignment = assignment ++ Map(
//      Node("B") -> true,
//      Node("C") -> false,
//    )
//    println(newAssignment)
//    val V: Set[Node[String]] = ('A' to 'C').map((x: Char) => Node(x.toString)).toSet
//    println(V.size)
//    val requests: Set[Map[Node[String], Boolean]] = powerset(V).flatMap(_ allMappings Set(true, false))
//    println(requests.size)
//    requests.foreach(println(_))
  }
}
