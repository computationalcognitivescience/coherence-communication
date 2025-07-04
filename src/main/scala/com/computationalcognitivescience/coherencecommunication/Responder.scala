package com.computationalcognitivescience.coherencecommunication

import coherence._
import com.computationalcognitivescience.coherencecommunication.coherence.Belief.Belief
import mathlib.graph._
import mathlib.set.SetTheory._
import coherence.TruthValueAssignment._

case class Responder(
    override val graph: WUnDiGraph[String],
    override val negativeConstraints: Set[WUnDiEdge[Belief]],
    override val ownBeliefs: TruthValueAssignment,
    override val sharedBeliefs: TruthValueAssignment,
    override val previousState: Option[Responder] = None,
    override val maxUtteranceLength: Option[Int] = None
) extends Interlocutor {

  /** Evaluates to `true` iff. the coherence of the current truth-value assignment is lower than of
    * the previous truth-value assignment.
    * @return
    */
  def troubleIdentification(utterance: TruthValueAssignment): (Boolean, Responder) = {
    val nextResponder = this.addSharedBeliefs(utterance)
    (nextResponder.coherence < this.coherence, nextResponder)
  }

  /** Computes <span style="font-variant-caps: normal;">Repair Formulation</span> for this
    * [[Responder]].
    *
    * TODO Include updated LaTeX definition.
    *
    * @param utterance
    *   The utterance to integrate into beliefs and formulate a restricted offer for.
    * @return
    *   A restricted offer or None.
    */
  def repairFormulation(utterance: TruthValueAssignment): Option[TruthValueAssignment] = {
    val allPossibleOfferBeliefs: Set[Set[Belief]] =
      if (maxUtteranceLength.isDefined)
        powersetUp(graph.vertices \ sharedBeliefs.beliefs, maxUtteranceLength.get)
      else
        powerset(graph.vertices \ sharedBeliefs.beliefs)

    val allTOffers: Set[TruthValueAssignment] = allPossibleOfferBeliefs
      .flatMap((offers: Set[Belief]) => offers.allMappings(Set(true, false)))
      .map(_.toTruthValueAssignment)

    def relativeTOfferCoherence(offer: TruthValueAssignment): Double =
      addSharedBeliefs(offer).coherence / offer.size

    val allOptimalTOffers = argMax(allTOffers, relativeTOfferCoherence)

    if (previousState.isDefined) {
      val tPrev: TruthValueAssignment = previousState.get.allBeliefs
      allOptimalTOffers.argMax(tva => tva ~ tPrev).random
    } else {
      allOptimalTOffers.random
    }
  }

  override protected def addSharedBeliefs(utterance: TruthValueAssignment): Responder = Responder(
    graph = graph,
    negativeConstraints = negativeConstraints,
    ownBeliefs = ownBeliefs,
    sharedBeliefs = sharedBeliefs ++ utterance,
    previousState = Some(this),
    maxUtteranceLength = maxUtteranceLength
  )

//  /** Based on (van Arkel, 2021, p. 22)
//    * @return
//    *   A repair request (truth-value assignment over nodes) if the new coherence is lower than the
//    *   previous one.
//    */
//  def troubleIdentification(
//      previousState: Interlocutor
//  ): Option[Map[Node[String], Boolean]] = {
//    val previousCoherence =
//      previousState.beliefNetwork.coh(previousState.allBeliefTruthValueAssignments)
//    val currentCoherence =
//      beliefNetwork.coh(allBeliefTruthValueAssignments) // Calculate current coherence
//    // If current coherence is lower than previous coherence, formulate a repair request
//    if (currentCoherence < previousCoherence) {
//      val repairRequest = repairFormulation()
////      println("[Responder.troubleIdentification] " + currentCoherence + "<" + previousCoherence)
////      println("[Responder.troubleIdentification] r says: " + repairRequest)
//      Some(repairRequest)
//    } // If current coherence is equal to or higher than previous coherence, all is well :) (do nothing)
//    else {
////      println("[Responder.troubleIdentification] " + currentCoherence + ">=" + previousCoherence)
////      println("[Responder.troubleIdentification] r says: Nothing")
//      None
//    }
//  }
//
//  /** Based on (van Arkel, 2021, p. 24) Generate a repair request (truth-value assignment)
//    * @return
//    *   The the most efficient set of truth values for which a flip maximizes coherence
//    */
//  private def repairFormulation(): Map[Node[String], Boolean] = {
////    println("[Responder.repairFormulation]")
//    // calculate V_request = V \ V_communicated
//    val vRequest: Set[Node[String]] =
//      allBeliefTruthValueAssignments.keySet -- sharedBeliefs.keySet
//
//    // Generate all possible repair requests
//    val allPossibleRequests: Set[Map[Node[String], Boolean]] = {
//      powersetUp(
//        vRequest,
//        utteranceLengthLimit
//      ) // Take the upperbounded powerset over beliefs that may be requested
//        // Map each set of beliefs to the truth-value mapping opposite of its current truth-value mapping
//        .filterNot(_.isEmpty) // Remove empty
//        .map((vRequest: Set[Node[String]]) =>
//          vRequest // Take the set of requested beliefs
//            .map((node: Node[String]) =>
//              (node, !allBeliefTruthValueAssignments(node))
//            ) // Flip the truth-value assignment
//            .toMap
//        )
//    }
//    // Get the best repair request
//    // TODO Investigate why allPossibleRequests sometimes is emtpy. See also Christians report.
//    allPossibleRequests
//      .argMax(repairRequest => {
//        val updatedNetwork = beliefNetwork.addFoundationalAssignment(repairRequest)
//        updatedNetwork.coh(updatedNetwork.coherence()) / (repairRequest.size + 1.0)
//      })
//      .random
//      .get
//  }
//
//  override def addCommunicatedBeliefs(utterance: Map[Node[String], Boolean]): Responder =
//    Responder(
//      beliefNetwork = beliefNetwork.addFoundationalAssignment(utterance),
//      priorBeliefs = priorBeliefs,
//      previousState = Some(this),
//      sharedBeliefs = sharedBeliefs ++ utterance,
//      presetInferredBeliefs = None,
//      maxUtteranceLength
//    )

//  def toDOTString(msg: String): String = {
//    val colorMaps = graph.vertices
//      .map(v =>
//        v ->
//          (List.empty :::
//            (if (ownBeliefs.contains(v)) List("deeppink") else List()) :::
//            (if (sharedBeliefs.contains(v)) List("aquamarine") else List()))
//      )
//      .toMap
//    super.toDOTString("Responder", Some(colorMaps), Some(3), msg = msg, xOffset = 10)
//  }
}
