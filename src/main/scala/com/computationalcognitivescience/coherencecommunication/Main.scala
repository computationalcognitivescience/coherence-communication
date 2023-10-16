package com.computationalcognitivescience.coherencecommunication

import mathlib.set.SetTheory._
import mathlib.graph._
import coherence._

object Main {
  def main(args: Array[String]): Unit = {
//    val A = Node[WeightedBelief](WeightedBelief("A", 3))
//    val B = Node[WeightedBelief](WeightedBelief("A", -2))
//
//    println(A == B)

    val r = scala.util.Random
    val G = WUnDiGraph.random(5,0.1)

    val verticesPrime = G.vertices.map((v: Node[String]) => Node[WeightedBelief](WeightedBelief(v.label, r.nextDouble())))
    val vertexMap = verticesPrime.map(v => v.label.label -> v.label.weight).toMap
    val edgesPrime = G.edges.map((e) => WUnDiEdge(Node(WeightedBelief(e.left.label, vertexMap(e.left.label))), Node(WeightedBelief(e.right.label, vertexMap(e.right.label))), e.weight))

//    println(verticesPrime)

    def foo(x: Set[WUnDiEdge[Node[WeightedBelief]]]): (Set[WUnDiEdge[Node[WeightedBelief]]], Set[WUnDiEdge[Node[WeightedBelief]]]) = {
      val A = x.map((e: WUnDiEdge[Node[WeightedBelief]]) => (e, r.nextDouble()))
      val B = A.filter(_._2 < 0.5).map(e => e._1)
      val C = A.filter(_._2 > 0.5).map(e => e._1)

      (B, C)
    }

    val (positiveConstraints, negativeConstraints) = foo(edgesPrime)
    val cohNet = new CoherenceNetwork(verticesPrime, positiveConstraints, negativeConstraints)

    val optAssignment = cohNet.dCoherence()

    println(optAssignment)
    println(cohNet.dcoh(optAssignment))

  }
}
