package com.computationalcognitivescience.coherencecommunication.coherence

import mathlib.graph.{Node, WUnDiEdge, WUnDiGraph}
import mathlib.set.SetTheory.{P, _}

class Responder (
                  net : WUnDiGraph[String],
                  pc : Set[WUnDiEdge[Node[String]]],
                  nc : Set[WUnDiEdge[Node[String]]],
                  priorBeliefs : Map[Node[String], Boolean],
                  w_prior : Double,
                  w_communicated : Double
                ) {

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

  def utteranceProcessing(T_utterance: Map[Node[String], Boolean]): Boolean = {
    println("Utterance Processing")
    // Update the network with the new utterance and update the current TVA
    updateNetwork(T_utterance)

    // Count how many of the intention beliefs are correct in T_simulated
    val communicatedT_complete = T_complete.filter(n => communicatedByInitiator.valueAssignment.contains(n._1))
    val commCount = communicatedT_complete.count(n => communicatedT_complete.get(n._1) == communicatedByInitiator.valueAssignment.get(n._1))

    // Normalise the count with the length of the utterance
    val BDcommCount = BigDecimal(commCount)
    val BDcommunicatedSize = BigDecimal(communicatedByInitiator.valueAssignment.size)
    val BDcommRatio = (BDcommCount / BDcommunicatedSize).setScale(3, BigDecimal.RoundingMode.HALF_UP)

    println(f"BDcommCount: $BDcommCount")
    println(f"BDcommunicatedSize: $BDcommunicatedSize")
    println(f"BDcommRatio: $BDcommRatio")

    var initiateRepair: Boolean = false
    if (BDcommRatio < 0.5) {
      initiateRepair = true
    }
    initiateRepair
  }

  def repairProduction(): Map[Node[String], Boolean] = {
    println("Repair Production")
    var maxNormalisedCoherence: Double = 0.0
    var maxSimulatedFlipped : Map[Node[String], Boolean] = null
    var maxT_repair : Map[Node[String], Boolean] = null

    P(BeliefNetwork.vertices).foreach(V_repair => {
      if (V_repair.nonEmpty){
        val allMappings = V_repair allMappings Set(true, false)

        allMappings.foreach(T_repair => {
          // Combine communicated beliefs with proposed repair (V_repair and T_repair)
          // Will always take the value of the second map (T_utterance) in case of a node being present in both maps
          val commAndRepairNodes = communicatedByInitiator.valueAssignment ++ T_repair
          val commAndRepair = BeliefBias(commAndRepairNodes, commAndRepairNodes.map { case (node, value) => node -> w_communicated })

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


          // TODO: if I call manually it returns null, because the clause below is never true (because countNonFlippedBeliefs is never 0 and
          // TODO: countNonTakenCommunicated is never NOT 0). check if this is still a problem when the entire function is
          // TODO: called by the utteranceProcessing function

          // TODO: Check if these cases do what is supposed to happen (with taking the most similiar tva to T_complete when multiple equal normalise coh)
          if (countNonFlippedBeliefs == 0 && countNonTakenCommunicated != 0) {
            val normalisedCoherence = simulatedNetwork.coh(T_flipped) / V_repair.size

            if (normalisedCoherence >= maxNormalisedCoherence) {
              if (normalisedCoherence > maxNormalisedCoherence || maxSimulatedFlipped == null) {
                maxNormalisedCoherence = normalisedCoherence
                maxSimulatedFlipped = T_flipped
                maxT_repair = T_repair
              }
              else{
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
        })
      }
    })
  println(f"maxT_repair: $maxT_repair")
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

  def getTVA(): Map[Node[String], Boolean] = {
    T_complete
  }
}
