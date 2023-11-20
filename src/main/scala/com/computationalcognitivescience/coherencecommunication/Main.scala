package com.computationalcognitivescience.coherencecommunication

import mathlib.set.SetTheory._
import mathlib.graph._
import coherence._
import mathlib.graph.GraphImplicits.{EdgeImpl2, WDiEdgeImpl, WUnDiEdgeImpl}

import scala.util.Random

object Main {
  def main(args: Array[String]): Unit = {

    val customVertices: Set[Node[String]] = Set(Node("A"), Node("B"), Node("C"), Node("D"), Node("E"), Node("a"), Node("r"))
    val customEdges: Set[WUnDiEdge[Node[String]]] = Set(
      "A" ~ "B" % 1,
      "A" ~ "C" % 1,
      "B" ~ "C" % 1,
      "C" ~ "D" % 1,
      "D" ~ "E" % 1,
      "a" ~ "A" % 1,
      "E" ~ "r" % 1,
    )
    val graph: WUnDiGraph[String] = WUnDiGraph(customVertices, customEdges)
    val network: BeliefNetwork = RandomBeliefNetwork.random(10, 0.5, 0.8)
    val maxFlowGraph = network.maxFlow(graph)

    println(maxFlowGraph.edges)
    println(maxFlowGraph.vertices)

//    val network: BiasedBeliefNetwork = RandomBiasedBeliefNetwork.random(10, 10, 2, 4, 3)
//    val network: BeliefNetwork = RandomBeliefNetwork.random(10, 0.5, 0.8)
//    println("Vertices: ", network.vertices)
//    println("Positive Constraints: ", network.positiveConstraints)
//    println("Negative Constraints: ", network.negativeConstraints)
//    println("Biased Beliefs: ", network.biasBeliefs)
//    println("Bias Assignment: ", network.biasAssignment)
//    println("Bias Weights: ", network.biasWeights)

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
