package com.computationalcognitivescience.coherencecommunication

import com.computationalcognitivescience.coherencecommunication.coherence.{
  BeliefBias,
  MultiBiasedBeliefNetwork
}
import mathlib.graph.GraphImplicits.{EdgeImpl2, N, WUnDiEdgeImpl}
import mathlib.graph.WUnDiGraph
import mathlib.set.SetTheory._

object MultiBiasedBeliefNetworkDemo {
  def main(args: Array[String]): Unit = {
    val pc = Set(
      "a" ~ "b" % 1,
      "b" ~ "c" % 1,
      "d" ~ "b" % 1
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

    mbbnet.coherence().foreach(println)
  }
}
