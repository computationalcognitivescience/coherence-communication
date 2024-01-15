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

    val network: BeliefNetwork = new BeliefNetwork(graph, negConstraints)

    val producer: Producer = new Producer(network,
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

    val interpreter: Interpreter = new Interpreter(network,
      priorBeliefs = Map(
        Node("Australia") -> false,
        Node("winter") -> true,
      ))
    
    val repairSimulation: repairSimulation = new repairSimulation(producer, interpreter)
    
    val results: Seq[Any] = repairSimulation.run()

    printResults(results)

    def printResults(results: Seq[Any]): Any = {
      print("Producer beliefs: ")
      println(results(0))
      print("Interpreter predicted beliefs: ")
      println(results(1))
      print("Interpreter true beliefs: ")
      println(results(2))
      print("Predicted overall structural similarity: ")
      println(results(3))
      print("True overall structural similarity: ")
      println(results(4))
      print("Predicted communicative intent structural similarity: ")
      println(results(5))
      print("True communicative intent structural similarity: ")
      println(results(6))
      print("Number of communication rounds: ")
      println(results(7))
    }

//    val randomBeliefNet: BeliefNetwork = RandomBeliefNetwork.random(size = 10, density = 0.4, ratioPosNeg = 0.6)
//    val nrPriorBeliefs1: Int = 3
//    val priorTruthValues: List[Boolean] = List.fill(nrPriorBeliefs1)(Random.nextDouble())
//      .map((v: Double) => if(v > 0.5) true else false)
//    val randomPriorBeliefs1: Map[Node[String], Boolean] =  Random.shuffle(randomBeliefNet.vertices.toList)
//      .take(nrPriorBeliefs1)
//      .zip(priorTruthValues)
//      .toMap



  }
}
