package com.computationalcognitivescience.coherencecommunication

import mathlib.set.SetTheory._
import mathlib.graph._
import coherence._

object Main {
  def main(args: Array[String]): Unit = {
    val A = Node[WeightedBelief](WeightedBelief("A", 3))
    val B = Node[WeightedBelief](WeightedBelief("A", -2))

    println(A == B)

    val r = scala.util.Random
    val G = WUnDiGraph.random(6,10)

    val verticesPrime = G.vertices.map((v: Node[String]) => Node[WeightedBelief](WeightedBelief(v.label, r.nextDouble())))

    println(verticesPrime)
  }
}
