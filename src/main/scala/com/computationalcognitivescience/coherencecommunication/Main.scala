package com.computationalcognitivescience.coherencecommunication

import mathlib.set.SetTheory._
import mathlib.graph._
import coherence._
import mathlib.graph.GraphImplicits.{EdgeImpl2, WDiEdgeImpl, WUnDiEdgeImpl}

import scala.util.Random

object Main {
  def main(args: Array[String]): Unit = {

    // TODO: bug-fix FPT-algorithm
//    val customVertices: Set[Node[String]] = Set(Node("A"), Node("B"), Node("C"), Node("D"),
//      Node("E"), Node("F"), Node("G"), Node("H"), Node("a"), Node("r"))
//    val customEdges: Set[WUnDiEdge[Node[String]]] = Set(
//      "A" ~ "B" % 1,
//      "A" ~ "C" % 1,
//      "A" ~ "D" % 1,
//      "B" ~ "E" % 1,
//      "B" ~ "F" % 1,
//      "D" ~ "G" % 1,
//      "D" ~ "H" % 1,
//      "a" ~ "E" % 1,
//      "E" ~ "r" % 1
//    )
//    val graph: WUnDiGraph[String] = WUnDiGraph(customVertices, customEdges)
//    val network: BeliefNetwork = RandomBeliefNetwork.random(10, 0.5, 0.8)
//    val newGraph: WDiGraph[String] = network.maxFlow(graph)

//    println(newGraph.edges)
//    println(newGraph.vertices)

//    val network: BiasedBeliefNetwork = RandomBiasedBeliefNetwork.random(10, 10, 2, 4, 3)
//    val network: BeliefNetwork = RandomBeliefNetwork.random(10, 0.5, 0.8)
//    println("Vertices: ", network.vertices)
//    println("Positive Constraints: ", network.positiveConstraints)
//    println("Negative Constraints: ", network.negativeConstraints)
//    println("Biased Beliefs: ", network.biasBeliefs)
//    println("Bias Assignment: ", network.biasAssignment)
//    println("Bias Weights: ", network.biasWeights)

    val capitalCityVertices: Set[Node[String]] = Set(Node("A"), Node("B"), Node("C"), Node("D"),
      Node("E"), Node("F"), Node("G"), Node("H"), Node("I"),
      Node("a"), Node("b"), Node("c"), Node("d"),
      Node("e"), Node("f"), Node("g"), Node("h"))

    val capitalCityPosConstraints: Set[WUnDiEdge[Node[String]]] = Set(
      "A" ~ "D" % 1,
      "B" ~ "E" % 1,
      "C" ~ "F" % 1,
      "D" ~ "G" % 1,
      "E" ~ "H" % 1,
      "F" ~ "I" % 1,

      "a" ~ "b" % 1,
      "b" ~ "c" % 1,
      "d" ~ "E" % 1,
      "a" ~ "A" % 1,
      "a" ~ "D" % 1,

      "c" ~ "e" % 1,
      "e" ~ "f" % 1,
      "f" ~ "G" % 1,
      "I" ~ "e" % 1,
      "f" ~ "g" % 1
    )

    val capitalCityNegConstraints: Set[WUnDiEdge[Node[String]]] = Set(
      "A" ~ "B" % 1,
      "C" ~ "A" % 1,
      "C" ~ "D" % 1,
      "D" ~ "E" % 1,
      "C" ~ "E" % 1,
      "F" ~ "H" % 1,
      "F" ~ "G" % 1,
      "G" ~ "H" % 1,
      "I" ~ "G" % 1,
      "I" ~ "H" % 1
    )

    val t1 = System.nanoTime()

    val capitalCityEdges: Set[WUnDiEdge[Node[String]]] = capitalCityPosConstraints \/ capitalCityNegConstraints

    val capitalCityGraph: WUnDiGraph[String] = WUnDiGraph(capitalCityVertices, capitalCityEdges)

    val capitalCityBeliefNet: BeliefNetwork = new BeliefNetwork(capitalCityGraph, capitalCityNegConstraints)

    val trueCoh: Map[Node[String], Boolean] = capitalCityBeliefNet.coherence()
    println((System.nanoTime() - t1) / 1e9d)
    val t2 = System.nanoTime()
    val fptCoh: Map[Node[String], Boolean] = capitalCityBeliefNet.cMinusCoherence()
    println((System.nanoTime() - t2) / 1e9d)

//    val customAssignment = Map(
//      Node("A") -> true,
//      Node("B") -> false,
//      Node("C") -> false,
//      Node("D") -> true,
//      Node("E") -> false,
//      Node("F") -> true,
//      Node("G") -> false,
//      Node("H") -> false,
//      Node("I") -> true,
//    )
//    println(capitalCityBeliefNet.coh(customAssignment))
    println()
    println(trueCoh == fptCoh)
    println(trueCoh)
    println(capitalCityBeliefNet.coh(trueCoh))
    println(fptCoh)
    println(capitalCityBeliefNet.coh(fptCoh))
//    println(capitalCityBeliefNet.coh(trueCoh))
//    println(fptCoh._1)
//    println(fptCoh._2)
//    println(trueCoh == fptCoh._1)
//    println(capitalCityBeliefNet.coh(trueCoh) == fptCoh._2)

//    val network: FoundationalBeliefNetwork = RandomFoundationalBeliefNetwork.random(10, 10, 2, 4, 3)
////    val network: BeliefNetwork = RandomBeliefNetwork.random(10, 0.5, 0.8)
//    println("Vertices: ", network.vertices)
//    println("Positive Constraints: ", network.positiveConstraints)
//    println("Negative Constraints: ", network.negativeConstraints)
//    println("Foundational Beliefs: ", network.foundationalBeliefs)
//    println("Foundational Assignment: ", network.foundationalAssignment)

//    WUnDiEdge(Node(N6),Node(N7),0.5129638916902957), WUnDiEdge(Node(N7),Node(N6),0.1716230297797422)
//    val v1 = Node("N6")
//    val v2 = Node("N7")
//    val e1 = WUnDiEdge(v1,v2, 1)
//    val e2 = WUnDiEdge(v2,v1, 1)
//    println(e1 == e2)
//    print("A" ~ "B" % 1 == "B" ~ "A" % 5)

//    val customEdges: Set[WDiEdge[Node[String]]] = Set(
//      "A" ~> "B" % 1,
//      "A" ~> "C" % 0,
//      "B" ~> "C" % 1,
//      "C" ~> "D" % 1,
//      "D" ~> "E" % 1,
//      "a" ~> "A" % 1,
//      "E" ~> "r" % 1,
//
//      "B" ~> "A" % 1,
//      "C" ~> "A" % 1,
//      "C" ~> "B" % 1,
//      "D" ~> "C" % 1,
//      "E" ~> "D" % 1,
//      "A" ~> "a" % 1,
//      "r" ~> "E" % 1,
//      "a" ~> "r" % 1
//    )
  }
}
