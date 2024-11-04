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




  }
}
