package com.computationalcognitivescience.coherencecommunication.coherence

import mathlib.graph.GraphImplicits.{EdgeImpl2, WUnDiEdgeImpl}
import mathlib.graph._
import mathlib.set.SetTheory._

import scala.annotation.tailrec
import scala.util.Random

/** A belief network representing positive and negative cohering beliefs.
  * @param graph
  *   A weighted directed graph, where the vertices with string values represent beliefs.
  * @param negativeConstraints
  *   A subset of the graph's edges that represent negative constraints.
  */
case class BeliefNetwork(
    override val graph: WUnDiGraph[String],
    override val negativeConstraints: Set[WUnDiEdge[Node[String]]]
) extends BaseBeliefNetwork

case object BeliefNetwork {
  def random(
      size: Int,
      density: Double,
      ratioNegativeEdges: Double,
      weightUpperbound: Double = 1.0
  ): BeliefNetwork = {
    require(
      0.0 <= density && density <= 1.0,
      s"Density $density is not between 0.0 and 1.0 inclusive."
    )
    require(
      0.0 <= ratioNegativeEdges && ratioNegativeEdges <= 1.0,
      s"Ratio negative edges $ratioNegativeEdges is not between 0.0 and 1.0 inclusive."
    )

    val graph = WUnDiGraph.preferentialAttachment(
      size,
      scala.math.round(density * size).intValue,
      weightUpperbound
    )
    BeliefNetwork(
      graph,
      negativeConstraints = scala.util.Random
        .shuffle(graph.edges)
        .take(scala.math.round(graph.edges.size * ratioNegativeEdges).intValue)
    )
  }
}
