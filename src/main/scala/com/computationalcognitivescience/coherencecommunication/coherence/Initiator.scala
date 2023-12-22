package com.computationalcognitivescience.coherencecommunication.coherence

import mathlib.graph.{Node, WUnDiEdge, WUnDiGraph}
import mathlib.set.SetTheory._

class Initiator(
                 net : WUnDiGraph[String],
                 pc : Set[WUnDiEdge[Node[String]]],
                 nc : Set[WUnDiEdge[Node[String]]],
                 priorBeliefs : Map[Node[String], Boolean],
                 intentionBeliefs : Map[Node[String], Boolean],
                 w_prior : Double,
                 w_intention : Double,
                 w_communicated : Double
) {

  private val prior = BeliefBias(
    priorBeliefs,
    w_prior
  )

  private val intention = BeliefBias(
    intentionBeliefs,
    w_intention
  )

  private var communicatedByInitiator = BeliefBias(
    Map(),
    w_communicated
  )

  private var communicatedByProducer = BeliefBias(
    Map(),
    w_communicated
  )

  private val biases = Seq(
    prior,
    intention,
    communicatedByProducer
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
    println("Utterance Production")
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
        val commAndUtteranceNodes = communicatedByInitiator.valueAssignment ++ T_utterance
        val commAndUtterance = BeliefBias(commAndUtteranceNodes, w_communicated)

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

    // update own list of communcitaed nodes
    val commAndUtteranceNodes = communicatedByInitiator.valueAssignment ++ result
    val commAndUtterance = BeliefBias(commAndUtteranceNodes, w_communicated)
    communicatedByInitiator = commAndUtterance

    result
  }

  /**
   * Take the repair request, update own beliefs and then answer with own tva of these beliefs
   * @param T_repair The tva of the responder for the beliefs in the repair request
   * @return The tva for the beliefs in the repair request according to own belief network
   */
  def repairProcessing(T_repair: Map[Node[String], Boolean]): Map[Node[String], Boolean] = {
    println("Repair Processing")
    // Responder gives the flipped tva of their own truth-value for these beliefs, thus flip back to update network
    val T_repairFlipped = T_repair.map(n => n._1 -> !n._2)
    updateNetwork(T_repairFlipped)

    // Retrieve own tva about the beliefs in the repair request
    val answer = T_complete.filter(n => T_repair.keySet.contains(n._1))
    answer
  }

  def endConversation(T_repair : Map[Node[String], Boolean]) : Boolean = {
    println("End Conversation")
    val simulatedNetwork = new MultiBiasedBeliefNetwork(
      network = net,
      negativeConstraints = nc,
      multiBeliefBiases = Seq(prior, communicatedByInitiator))

    // Get simulated TVA most similar to current T_complete
    val allTVAs = simulatedNetwork.allOptimalTruthValueAssignments
    val mostSimilarTVA = allTVAs.groupBy(n => n.count(m => n.get(m._1) == T_complete.get(m._1)))
    val T_simulated = mostSimilarTVA.get(mostSimilarTVA.keySet.max).head.head

    var result : Boolean = false
    val simulatedIntention = T_simulated.filter(n => intention.valueAssignment.contains(n._1))
    val intentionCount = simulatedIntention.count(n => simulatedIntention.get(n._1) == intention.valueAssignment.get(n._1))
    val Nintention = intention.valueAssignment.size

    if (intentionCount == Nintention && T_repair == null){
        result = true
    }
    result
  }


  /**
   * Combine the communicated beliefs with the repair request (V_repair and T_repair)
   * @param T_repair The TVA of the utterance
   */
  def updateNetwork(T_repair: Map[Node[String], Boolean]): Unit = {
    // Will always take the value of the second map (T_repair) in case of a node being present in both maps
    val commAndUtteranceNodes = communicatedByProducer.valueAssignment ++ T_repair
    val commAndUtterance = BeliefBias(commAndUtteranceNodes, w_communicated)
    communicatedByProducer = commAndUtterance

    BeliefNetwork = new MultiBiasedBeliefNetwork(
      network = net,
      negativeConstraints = nc,
      multiBeliefBiases = Seq(prior, intention, commAndUtterance))

    // Get new TVA most similar to current T_complete
    val allTVAs = BeliefNetwork.allOptimalTruthValueAssignments
    val mostSimilarTVA = allTVAs.groupBy(n => n.count(m => n.get(m._1) == T_complete.get(m._1)))
    T_complete = mostSimilarTVA.get(mostSimilarTVA.keySet.max).head.head
  }

  def getTVA(): Map[Node[String], Boolean] = {
    T_complete
  }
}
