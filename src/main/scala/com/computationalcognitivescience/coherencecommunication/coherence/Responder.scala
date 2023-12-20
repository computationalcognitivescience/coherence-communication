package com.computationalcognitivescience.coherencecommunication.coherence

import mathlib.graph.GraphImplicits.N
import mathlib.graph.{Node, WUnDiEdge}

class Responder (
                  net : MultiBiasedBeliefNetwork,
                  pc : Set[WUnDiEdge[Node[String]]],
                  nc : Set[WUnDiEdge[Node[String]]],
                  w_prior : Double,
                  w_communicated : Double
                ) {

  private val prior = BeliefBias(
    Map(
      N("a") -> true
    ),
    Map(
      N("a") -> w_prior)
  )

  private val communicated = BeliefBias(
    Map(
      N("a") -> false
    ),
    Map(
      N("a") -> w_communicated
    )
  )

  private val biases = Seq(
    prior,
    communicated
  )

  var BeliefNetwork = new MultiBiasedBeliefNetwork(
    network = net,
    negativeConstraints = nc,
    multiBeliefBiases = biases
  )

  var T_complete: Map[Node[String], Boolean] = BeliefNetwork.allOptimalTruthValueAssignments.head


}
