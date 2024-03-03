package com.computationalcognitivescience.coherencecommunication.coherence

import mathlib.graph.{Node, WUnDiEdge, WUnDiGraph}
import mathlib.set.SetTheory._

/**
 * The initiator that has some beliefs they intent to tell to their interlocutor.
 * This class holds all functions the initiator needs to have a conversation and update their beliefs accordingly.
 * @param net The belief network.
 * @param pc A set of edges that are the positive constraints.
 * @param nc A set of edges that are the negative constraints.
 * @param priorBeliefs The beliefs the initiator has some prior belief/knowledge about.
 * @param intentionBeliefs The beliefs the initiator wishes to convince their interlocutor of.
 * @param weight_prior The weight that is put on the prior beliefs.
 * @param weight_intention The weight that is put on the intention beliefs.
 * @param weight_communication The weight that is put on the beliefs communicated by the responder.
 */
class Initiator(
                 net : WUnDiGraph[String],
                 pc : Set[WUnDiEdge[Node[String]]],
                 nc : Set[WUnDiEdge[Node[String]]],
                 priorBeliefs : Map[Node[String], Boolean],
                 intentionBeliefs : Map[Node[String], Boolean],
                 weight_prior : Double,
                 weight_intention : Double,
                 weight_communication : Double
) {

  private val w_prior = weight_prior
  private val w_intention = weight_intention
  private val w_communicated = weight_communication

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

  var T_complete : Map[Node[String], Boolean] = BeliefNetwork.allOptimalTruthValueAssignments.head

  /**
   * Creates the shortest possible utterance such that the responder taking over the beliefs in the utterance, leads to
   * a belief network where the beliefs part of the intention of the initiator are most similar.
   * @return A TVA for the utterance beliefs.
   */
  def utteranceProduction(): Map[Node[String], Boolean] = {
    // Initialising variables to keep track of (current) best utterance option
    var currentMaxRatio = BigDecimal(0)
    var maxSimulatedTVA : Map[Node[String], Boolean] = null
    var maxV_utterance : Set[Node[String]] = null

    // Go over all possible utterances (all subsets of vertices (excluding the empty set))
    P(BeliefNetwork.vertices)
      .filter(_.nonEmpty)
      .foreach(V_utterance => {
        // TVA for current beliefs in V_utterance
        val T_utterance = T_complete.filter(n => V_utterance.contains(n._1))

        // If we have communicated a belief in the current V_utterances, do not consider it as an option
        var communicatedPreviously = false
        V_utterance.foreach(node => {
          if (communicatedByInitiator.beliefs.contains(node)){
            if(communicatedByInitiator.valueAssignment(node) == T_utterance(node)){
              communicatedPreviously = true
            }
          }
        })

        if(!communicatedPreviously) {
          // Combine communicated beliefs with proposed utterance (V_utterance and T_utterance)
          // Will always take the value of the second map (T_utterance) in case of a node being present in both maps
          val commAndUtteranceNodes = communicatedByInitiator.valueAssignment ++ T_utterance
          val commAndUtterance = BeliefBias(commAndUtteranceNodes, w_communicated)

          // Simulate the belief network given proposed utterance
          val simulatedNetwork = new MultiBiasedBeliefNetwork(
            network = net,
            negativeConstraints = nc,
            multiBeliefBiases = Seq(prior, commAndUtterance))

          // Get simulated TVA structurally most similar to current T_complete
          val allTVAs = simulatedNetwork.allOptimalTruthValueAssignments
          val mostSimilarTVA = allTVAs.groupBy(n => n.count(m => n.get(m._1) == T_complete.get(m._1)))
          val T_simulated = mostSimilarTVA.get(mostSimilarTVA.keySet.max).head.head

          // Count how many of the intention beliefs are correct in T_simulated
          val simulatedIntention = T_simulated.filter(n => intention.valueAssignment.contains(n._1))
          val intentionCount = simulatedIntention.count(n => simulatedIntention.get(n._1) == intention.valueAssignment.get(n._1))

          // Normalise the count with the length of the utterance
          val BDintentionCount = BigDecimal(intentionCount)
          val BDutteranceSize = BigDecimal(V_utterance.size)
          val BDratio = (BDintentionCount / BDutteranceSize).setScale(3, BigDecimal.RoundingMode.HALF_UP)

          // Save TVA when T_simulated (normalised with the length of V_utterance) is better than current saved TVA
          if (BDratio >= currentMaxRatio) {
            if (BDratio > currentMaxRatio || maxSimulatedTVA == null) {
              currentMaxRatio = BDratio
              maxSimulatedTVA = T_simulated
              maxV_utterance = V_utterance
            }
            // When equal score to current TVA compare entire network on similarity to T_complete
            else {
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

    if(maxV_utterance != null) {
      // Return the TVA of the utterance nodes that provided the best TVA
      val result = T_complete.filter(n => maxV_utterance.contains(n._1))

      // Update own list of communicated nodes
      val commAndUtteranceNodes = communicatedByInitiator.valueAssignment ++ result
      val commAndUtterance = BeliefBias(commAndUtteranceNodes, w_communicated)
      communicatedByInitiator = commAndUtterance

      result
    }
    else {
      null
    }
  }

  /**
   * Take the repair request, update own beliefs and then answer with own TVA of these beliefs.
   * @param T_repair The TVA of the responder for the beliefs in the repair request.
   * @return The TVA for the beliefs in the repair request according to own belief network.
   */
  def repairProcessing(T_repair: Map[Node[String], Boolean]): Map[Node[String], Boolean] = {
    // Responder gives the flipped TVA of their own truth-value for these beliefs, thus flip back to update network
    val T_repairFlipped = T_repair.map(n => n._1 -> !n._2)
    updateNetwork(T_repairFlipped)

    // Retrieve own TVA about the beliefs in the repair request
    val answer = T_complete.filter(n => T_repair.keySet.contains(n._1))
    val commAndAnswerNodes = communicatedByInitiator.valueAssignment ++ answer
    val commAndAnswer = BeliefBias(commAndAnswerNodes, w_communicated)
    communicatedByInitiator = commAndAnswer

    answer
  }

  /**
   * Checks if both interlocutors agree to end the conversation.
   * @param T_repair The most recent repair request (null if no repair request was initiated).
   * @return Tuple of booleans to respectively indicate if both interlocutors want to end the conversation,
   *         if the initiator wants to end the conversation and if the responder wants to end the conversation.
   */
  def endConversation(T_repair : Map[Node[String], Boolean]) : (Boolean, Boolean, Boolean) = {
    // Simulate the network of the responder
    val simulatedNetwork = new MultiBiasedBeliefNetwork(
      network = net,
      negativeConstraints = nc,
      multiBeliefBiases = Seq(prior, communicatedByInitiator))

    // Get simulated TVA most similar to current T_complete
    val allTVAs = simulatedNetwork.allOptimalTruthValueAssignments
    val mostSimilarTVA = allTVAs.groupBy(n => n.count(m => n.get(m._1) == T_complete.get(m._1)))
    val T_simulated = mostSimilarTVA.get(mostSimilarTVA.keySet.max).head.head

    // Count how many intention beliefs are taken over in the simulated TVA
    val simulatedIntention = T_simulated.filter(n => intention.valueAssignment.contains(n._1))
    val intentionCount = simulatedIntention.count(n => simulatedIntention.get(n._1) == intention.valueAssignment.get(n._1))
    val Nintention = intention.valueAssignment.size

    var result : Boolean = false
    // If all intention beliefs are taken over and no repair was initiated, we want to end the conversation
    if (intentionCount == Nintention && T_repair == null){
        result = true
    }

    (result, intentionCount==Nintention, T_repair==null)
  }

  /**
   * Update the network with the information in the repair request.
   * @param T_repair The TVA of the repair request.
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

  /**
   * Get the current truth-value assignment of the initiator.
   * @return Current TVA.
   */
  def getTVA(): Map[Node[String], Boolean] = {
    T_complete
  }

  /**
   * Get the prior weight of the initiator.
   * @return Prior weight.
   */
  def get_w_prior(): Double = {
    w_prior
  }

  /**
   * Get the intention weight of the initiator.
   * @return intention weight.
   */
  def get_w_intention(): Double = {
    w_intention
  }

  /**
   * Get the communicated weight of the initiator.
   * @return communicated weight.
   */
  def get_w_communicated(): Double = {
    w_communicated
  }

}
