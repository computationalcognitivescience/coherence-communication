package com.computationalcognitivescience.coherencecommunication

import com.computationalcognitivescience.coherencecommunication.coherence.{BeliefBias, MultiBiasedBeliefNetwork}
import mathlib.graph.GraphImplicits.{EdgeImpl2, N, WUnDiEdgeImpl}
import mathlib.graph.WUnDiGraph
import mathlib.set.SetTheory._

object MultiBiasedBeliefNetworkDemo {
  def main(args: Array[String]): Unit = {
    val pc = Set(
      "a" ~ "b" % 1,
      "b" ~ "c" % 1,
      "a" ~ "c" % 1,
      "c" ~ "d" % 1
    )
    val nc = Set(
      "d" ~ "b" % 1
    )
    val net = WUnDiGraph(
      pc \/ nc
    )
    val biases = Seq(
      BeliefBias(
        Map(
          N("a") -> true
        ),
        Map(N("a") -> 0.4)
      ),
      BeliefBias(
        Map(
          N("a") -> false,
          N("b") -> true
        ),
        Map(N("a") -> 0.2, N("b") -> 0.2)
      )
    )

    val mbbnet = new MultiBiasedBeliefNetwork(
      network = net,
      negativeConstraints = nc,
      multiBeliefBiases = biases
    )

//    mbbnet.allOptimalTruthValueAssignments.foreach(println)



    // COHERENCE CALCULATIONS

    // TVA(s) with maximal coherence (WITH weight on communicated nodes)
    val tvas_communicated = mbbnet.allOptimalTruthValueAssignments


    // Printing out network information
    println("NETWORK INFO =============================")

    // Vertices
    print(f"Vertices: ")
    mbbnet.vertices.foreach(v => {
      print(f"${v.label} ")
    })
    println("\n----------")

    // Positive constraints
    mbbnet.positiveConstraints.foreach(pc => {
      println(f"Positive constraint: ${pc.left.label} - ${pc.right.label}, ${pc.weight}")
    })
    println("----------")

    // Negative constraints
    mbbnet.negativeConstraints.foreach(nc => {
      println(f"Negative constraint: ${nc.left.label} - ${nc.right.label}, ${nc.weight}")
    })
    println("----------")

    // Sets of biased beliefs (nodes, assigned value and weight)
    println(f"Biased Beliefs: ")
    mbbnet.multiBeliefBiases.foreach(bias => {
      println(f"\t${bias} ")
    })
    println("----------")

    // TVA's with maximal coherence
    tvas_communicated.foreach(tva => {
      println(f"Max TVA assignment: ${mbbnet.coh(tva)} \n $tva ")
    })

  }
}
