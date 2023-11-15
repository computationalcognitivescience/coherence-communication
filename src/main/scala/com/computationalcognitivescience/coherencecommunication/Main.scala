package com.computationalcognitivescience.coherencecommunication

import mathlib.set.SetTheory._
import mathlib.graph._
import coherence._
import mathlib.graph.GraphImplicits.{EdgeImpl2, WUnDiEdgeImpl}

import scala.util.Random

object Main {
  def main(args: Array[String]): Unit = {

    val network: BiasedBeliefNetwork = RandomBiasedBeliefNetwork.random(10, 10, 2, 4, 3)
//    val network: BeliefNetwork = RandomBeliefNetwork.random(10, 0.5, 0.8)
    println("Vertices: ", network.vertices)
    println("Positive Constraints: ", network.positiveConstraints)
    println("Negative Constraints: ", network.negativeConstraints)
    println("Biased Beliefs: ", network.biasBeliefs)
    println("Bias Assignment: ", network.biasAssignment)
    println("Bias Weights: ", network.biasWeights)

//    WUnDiEdge(Node(N6),Node(N7),0.5129638916902957), WUnDiEdge(Node(N7),Node(N6),0.1716230297797422)
//    val v1 = Node("N6")
//    val v2 = Node("N7")
//    val e1 = WUnDiEdge(v1,v2, 1)
//    val e2 = WUnDiEdge(v2,v1, 1)
//    println(e1 == e2)
  }
}
