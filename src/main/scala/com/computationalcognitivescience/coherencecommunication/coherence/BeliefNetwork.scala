package com.computationalcognitivescience.coherencecommunication.coherence

import com.computationalcognitivescience.coherencecommunication.coherence.Belief.Belief
import mathlib.graph._

/** A belief network representing positive and negative cohering beliefs.
  * @param graph
  *   A weighted directed graph, where the vertices with string values represent beliefs.
  * @param negativeConstraints
  *   A subset of the graph's edges that represent negative constraints.
  */
case class BeliefNetwork(
    override val graph: WUnDiGraph[String],
    override val negativeConstraints: Set[WUnDiEdge[Belief]]
) extends BaseBeliefNetwork

case object BeliefNetwork {

}
