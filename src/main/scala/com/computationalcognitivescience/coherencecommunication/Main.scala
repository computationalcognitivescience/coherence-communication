package com.computationalcognitivescience.coherencecommunication

import mathlib.set.SetTheory._
import mathlib.graph._
import coherence._
import mathlib.graph.GraphImplicits.{EdgeImpl2, WUnDiEdgeImpl}

import scala.util.Random

object Main {
  def main(args: Array[String]): Unit = {

    // Setting up network structure
    val vertices: Set[Node[String]] = Set(
                  Node("N0"),
                  Node("N1"),
                  Node("N2"),
                  Node("N3"),
                  Node("N4"))
    val edges: Set[WUnDiEdge[Node[String]]] = Set(
                  WUnDiEdge(Node("N0"), Node("N1"), 0.82),
                  WUnDiEdge(Node("N0"), Node("N4"), 0.47),
                  WUnDiEdge(Node("N2"), Node("N3"), 0.64),
                  WUnDiEdge(Node("N1"), Node("N2"), 0.92))
    val negativeConstraints: Set[WUnDiEdge[Node[String]]] = Set(
                  WUnDiEdge(Node("N0"), Node("N4"), 0.47))

    // Setting up nodes with weights
    val biasBeliefs: Set[Node[String]] = Set(
                  Node("N1"),
                  Node("N4"))
    val biasAssignment: Map[Node[String], Boolean] = Map(
                  Node("N1") -> true,
                  Node("N4") -> false)
    val biasWeights: Map[Node[String], Double] = Map(
                  Node("N1") -> 0.5,
                  Node("N4") -> 0.2)

    // Network initialization
    val network: WUnDiGraph[String] = WUnDiGraph(vertices, edges)
    val beliefNetwork = new BiasedBeliefNetwork(network, negativeConstraints, biasBeliefs, biasAssignment, biasWeights)

    // Setting up communicated nodes and corresponding communicated beliefs
    val communicatedBeliefs : Set[Node[String]] = Set(
                  Node("N1"),
                  Node("N2"))
    val communicatedAssignment: Map[Node[String], Boolean] = Map(
                  Node("N1") -> true,
                  Node("N2") -> false)
    val communicatedWeights: Map[Node[String], Double] = Map(
                  Node("N1") -> 0.8,
                  Node("N4") -> 0.8)
    val assignment: Map[Node[String], Boolean] = Map(
                  Node("N0") -> false,
                  Node("N1") -> true,
                  Node("N2") -> true,
                  Node("N3") -> true,
                  Node("N4") -> false)

    // Coherence calculation (max coherence given network and weighted nodes.
    // and coherence given certain communicated nodes with extra weight)
    val coherence: Map[Node[String], Boolean] = beliefNetwork.coherence()
    val coherence_value = beliefNetwork.coh(coherence)
    val coherence_comm: Double = beliefNetwork.coh_communicated(assignment, communicatedBeliefs, communicatedAssignment, communicatedWeights)

    // Printing out network information
    println("Vertices: ", beliefNetwork.vertices)
    println("Positive Constraints: ", beliefNetwork.positiveConstraints)
    println("Negative Constraints: ", beliefNetwork.negativeConstraints)
    println("Biased Beliefs: ", beliefNetwork.biasBeliefs)
    println("Bias Assignment: ", beliefNetwork.biasAssignment)
    println("Bias Weights: ", beliefNetwork.biasWeights)
    println("Coherence assignment: ", coherence)
    println("Coherence value: ", coherence_value)
    println("Coherence with communicated: ", coherence_comm)






    //    val network: BiasedBeliefNetwork = RandomBiasedBeliefNetwork.random(10, 10, 2, 4, 3)
////    val network: BeliefNetwork = RandomBeliefNetwork.random(10, 0.5, 0.8)
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


  }
}
