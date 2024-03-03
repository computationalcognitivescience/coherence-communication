package com.computationalcognitivescience.coherencecommunication.coherence

import mathlib.graph.{Node, WUnDiEdge, WUnDiGraph}
import mathlib.set.SetTheory.{P, _}

/**
 * The responder wants to reach mutual understanding during the conversation.
 * This class holds all functions the initiator needs to have a conversation and update their beliefs accordingly.
 * @param net The belief network.
 * @param pc A set of edges that are the positive constraints.
 * @param nc A set of edges that are the negative constraints.
 * @param priorBeliefs The beliefs the initiator has some prior belief/knowledge about.
 * @param w_prior The weight that is put on the prior beliefs.
 * @param weight_communication The weight that is put on the beliefs communicated by the responder.
 */
class Responder (
                  net : WUnDiGraph[String],
                  pc : Set[WUnDiEdge[Node[String]]],
                  nc : Set[WUnDiEdge[Node[String]]],
                  priorBeliefs : Map[Node[String], Boolean],
                  w_prior : Double,
                  weight_communication : Double
                ) {

  private val w_communicated = weight_communication

  private val prior = BeliefBias(
    priorBeliefs,
    w_prior
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
    communicatedByInitiator
  )

  var BeliefNetwork = new MultiBiasedBeliefNetwork(
    network = net,
    negativeConstraints = nc,
    multiBeliefBiases = biases
  )

  var T_complete: Map[Node[String], Boolean] = BeliefNetwork.allOptimalTruthValueAssignments.head

  /**
   * Take the utterance, update own beliefs and then evaluate whether to initiate repair.
   * @param T_utterance Utterance provided by the initiator.
   * @return Boolean indicating whether to indicate repair or not.
   */
  def utteranceProcessing(T_utterance: Map[Node[String], Boolean]): Boolean = {
    // Update the network with the new utterance and update the current TVA
    updateNetwork(T_utterance)

    // Count how many of the intention beliefs are correct in T_simulated
    val communicatedT_complete = T_complete.filter(n => communicatedByInitiator.valueAssignment.contains(n._1))
    val commCount = communicatedT_complete.count(n => communicatedT_complete.get(n._1) == communicatedByInitiator.valueAssignment.get(n._1))

    // Normalise the count with the length of the utterance
    val BDcommCount = BigDecimal(commCount)
    val BDcommunicatedSize = BigDecimal(communicatedByInitiator.valueAssignment.size)
    val BDcommRatio = (BDcommCount / BDcommunicatedSize).setScale(3, BigDecimal.RoundingMode.HALF_UP)

    var initiateRepair: Boolean = false
    if (BDcommRatio <= 0.75) {
      initiateRepair = true
    }
    initiateRepair
  }

  /**
   * Evaluate what the best beliefs are to ask in a repair request.
   * @return A TVA for the repair request beliefs.
   */
  def repairProduction(): Map[Node[String], Boolean] = {
    var maxNormalisedCoherence: Double = 0.0
    var maxSimulatedFlipped : Map[Node[String], Boolean] = null
    var maxT_repair : Map[Node[String], Boolean] = null

    // Go over all possible repair request (all subsets of vertices (excluding the empty set))
    P(BeliefNetwork.vertices).foreach(V_repair => {
      if (V_repair.nonEmpty){
        val allMappings = V_repair allMappings Set(true, false)

        // Consider all mappings of truth-values for the current repair request.
        allMappings.foreach(T_repair => {
          if(T_repair.nonEmpty){
            // Combine communicated beliefs with proposed repair (V_repair and T_repair)
            // Will always take the value of the second map (T_utterance) in case of a node being present in both maps
            val commAndRepairNodes = communicatedByInitiator.valueAssignment ++ T_repair
            val commAndRepair = BeliefBias(commAndRepairNodes, commAndRepairNodes.map { case (node, value) => node -> w_communicated })

            // Simulate network with current repair request
            val simulatedNetwork = new MultiBiasedBeliefNetwork(
              network = net,
              negativeConstraints = nc,
              multiBeliefBiases = Seq(prior, commAndRepair))

            val allTVAs = simulatedNetwork.allOptimalTruthValueAssignments
            val mostSimilarTVA = allTVAs.groupBy(n => n.count(m => n.get(m._1) == T_complete.get(m._1)))
            val T_flipped = mostSimilarTVA.get(mostSimilarTVA.keySet.max).head.head

            val countNonFlippedBeliefs = T_flipped.filter(n => V_repair.contains(n._1)).count(n => T_flipped.get(n._1) == T_complete.get(n._1))
            val countNonTakenCommunicated = T_flipped.filter(n => communicatedByInitiator.valueAssignment.contains(n._1))
              .count(n => T_flipped.get(n._1) != communicatedByInitiator.valueAssignment.get(n._1))

            // Check all beliefs in the repair request are flipped and all communicated beliefs are taken over
            if (countNonFlippedBeliefs == 0 && countNonTakenCommunicated != 0 || maxT_repair == null) {
              val normalisedCoherence = simulatedNetwork.coh(T_flipped) / V_repair.size

              if (normalisedCoherence >= maxNormalisedCoherence || maxNormalisedCoherence == 0.0) {
                if (normalisedCoherence > maxNormalisedCoherence || maxSimulatedFlipped == null) {
                  maxNormalisedCoherence = normalisedCoherence
                  maxSimulatedFlipped = T_flipped
                  maxT_repair = T_repair
                }
                else {
                  val T_simulatedCount = T_flipped.count(n => T_flipped.get(n._1) == T_complete.get(n._1))
                  val maxSimulatedFlippedCount = maxSimulatedFlipped.count(n => maxSimulatedFlipped.get(n._1) == T_complete.get(n._1))

                  if (T_simulatedCount > maxSimulatedFlippedCount) {
                    maxNormalisedCoherence = normalisedCoherence
                    maxSimulatedFlipped = T_flipped
                    maxT_repair = T_repair
                  }
                }
              }
            }
          }

        })
      }
    })
  maxT_repair
  }

  /**
   * Combine the communicated beliefs with utterance (V_utterance and T_utterance)
   * @param T_utterance The TVA of the utterance
   */
  def updateNetwork(T_utterance : Map[Node[String], Boolean]): Unit = {
    // Will always take the value of the second map (T_utterance) in case of a node being present in both maps
    val commAndUtteranceNodes = communicatedByInitiator.valueAssignment ++ T_utterance
    val commAndUtterance = BeliefBias(commAndUtteranceNodes, w_communicated)
    communicatedByInitiator = commAndUtterance

    BeliefNetwork = new MultiBiasedBeliefNetwork(
      network = net,
      negativeConstraints = nc,
      multiBeliefBiases = Seq(prior, commAndUtterance))

    // Get new TVA most similar to current T_complete
    val allTVAs = BeliefNetwork.allOptimalTruthValueAssignments
    val mostSimilarTVA = allTVAs.groupBy(n => n.count(m => n.get(m._1) == T_complete.get(m._1)))
    T_complete = mostSimilarTVA.get(mostSimilarTVA.keySet.max).head.head
  }

  /**
   * Get the current truth-value assignment of the initiator.
   * @return Current TVA.
   */
  def getTVA(): Map[Node[String], Boolean] = {
    T_complete
  }

  /**
   * Get the communicated weight of the initiator.
   * @return communicated weight.
   */
  def get_w_communicated(): Double = {
    w_communicated
  }
}
