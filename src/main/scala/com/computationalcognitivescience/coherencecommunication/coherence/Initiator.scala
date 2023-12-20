package com.computationalcognitivescience.coherencecommunication.coherence

import mathlib.graph.GraphImplicits.N
import mathlib.graph.{Node, WUnDiEdge, WUnDiGraph}
import mathlib.set.SetTheory._

class Initiator(
                 net : WUnDiGraph[String],
                 pc : Set[WUnDiEdge[Node[String]]],
                 nc : Set[WUnDiEdge[Node[String]]],
                 w_prior : Double,
                 w_intention : Double,
                 w_communicated : Double
) {

  private val prior = BeliefBias(
    Map(
      N("a") -> true
    ),
    Map(
      N("a") -> w_prior)
  )

  private val intention = BeliefBias(
    Map(
      N("b") -> true,
      N("c") -> false
    ),
    Map(
      N("b") -> w_intention,
      N("c") -> w_intention)
  )

  private val communicated = BeliefBias(
    Map(
      N("c") -> false
    ),
    Map(
      N("c") -> w_communicated
    )
  )

  private val biases = Seq(
    prior,
    intention,
    communicated
  )

  var BeliefNetwork = new MultiBiasedBeliefNetwork(
    network = net,
    negativeConstraints = nc,
    multiBeliefBiases = biases
  )

  var T_complete: Map[Node[String], Boolean] = BeliefNetwork.allOptimalTruthValueAssignments.head

  /**
   * Creates the shortest possible utterance such that the responder taking over the beliefs in the utterance, leads to
   * a belief network where the beliefs part of the intention of the initiator are most similar
   * @return A truth-value assignment for the utterance beliefs
   */
  def utteranceProduction(): Map[Node[String], Boolean] = {

    var currentMaxRatio = BigDecimal(0)
    var maxSimulatedTVA : Map[Node[String], Boolean] = null
    var maxV_utterance : Set[Node[String]] = null

    // Go over all possible subsets of vertices (excluding the empty set)
    P(BeliefNetwork.vertices).foreach(V_utterance => {
      if (V_utterance.nonEmpty){
        // TVA for current beliefs in V_utterance
        val T_utterance = T_complete.filter(n => V_utterance.contains(n._1))

        // Combine communicated beliefs with proposed utterance (V_utterance and T_utterance)
        // Will always take the value of the second map (T_utterance) in case of a node being present in both maps
        val commAndUtteranceNodes = communicated.valueAssignment ++ T_utterance
        val commAndUtterance = BeliefBias(commAndUtteranceNodes, commAndUtteranceNodes.map { case (node, value) => node -> w_communicated })

        // Simulate the belief network given proposed utterance
        val simulatedNetwork = new MultiBiasedBeliefNetwork(
          network = net,
          negativeConstraints = nc,
          multiBeliefBiases = Seq(prior, commAndUtterance))

        // Get simulated TVA most similar to current T_complete
        val allTVAs = simulatedNetwork.allOptimalTruthValueAssignments
        val mostSimilarTVA = allTVAs.groupBy(n => n.count(m => n.get(m._1) == T_complete.get(m._1)))
        val T_simulated = mostSimilarTVA.get(mostSimilarTVA.keySet.max).head.head

        // Count how many of the intention beliefs are correct in T_simulated
        val simulatedIntention = T_simulated.filter(n => intention.valueAssignment.contains(n._1))
        val intentionCount = simulatedIntention.count(n => simulatedIntention.get(n._1) == intention.valueAssignment.get(n._1))

        // Normalise the count with the length of the utterance
        val BDintentionCount = BigDecimal(intentionCount)
        val BDutteranceSize = BigDecimal(V_utterance.size)
        val BDratio = (BDintentionCount/BDutteranceSize).setScale(3, BigDecimal.RoundingMode.HALF_UP)

        // Save TVA when T_simulated normalised with the length of V_utterance is better than current saved TVA
        if (BDratio >= currentMaxRatio){
          if (BDratio > currentMaxRatio || maxSimulatedTVA == null){
            currentMaxRatio = BDratio
            maxSimulatedTVA = T_simulated
            maxV_utterance = V_utterance
          }
          // When equal score to current TVA compare entire network on similarity to T_complete
          else{
              val T_simulatedCount = T_simulated.count(n => T_simulated.get(n._1) == T_complete.get(n._1))
              val maxSimulatedTVACount = maxSimulatedTVA.count(n => maxSimulatedTVA.get(n._1) == T_complete.get(n._1))

              if (T_simulatedCount > maxSimulatedTVACount) {
                currentMaxRatio = BDratio
                maxSimulatedTVA = T_simulated
                maxV_utterance = V_utterance
              }
          }
        }

      }
    })
    // return the TVA of the utterance nodes that provided the best TVA
    val result = maxSimulatedTVA.filter(n => maxV_utterance.contains(n._1))
    result
  }

}
