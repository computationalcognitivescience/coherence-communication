package com.computationalcognitivescience.coherencecommunication

import com.computationalcognitivescience.coherencecommunication.Understandings.Understanding
import com.computationalcognitivescience.coherencecommunication.coherence.Belief.Belief
import com.computationalcognitivescience.coherencecommunication.util.SetTheoryDev._
import com.computationalcognitivescience.coherencecommunication.coherence.{
  BeliefNetwork,
  FoundationalBeliefNetwork,
  TruthValueAssignment
}
import mathlib.set.SetTheory._
import mathlib.graph._

case class UtteranceInitiatorPair(
    utteranceOption: Option[TruthValueAssignment],
    nextInitiator: Initiator
)

//NOTES:
// PRIOR BELIEFS AND COMMUNICATIVE INTENT CAN OVERLAP
// WE ALLOW FOR EMPTY UTTERANCES BY DIVIDING BY UTTERANCE SIZE + 1
case class Initiator(
    override val graph: WUnDiGraph[String],
    override val negativeConstraints: Set[WUnDiEdge[Belief]],
    override val ownBeliefs: TruthValueAssignment,
    override val sharedBeliefs: TruthValueAssignment,
    communicativeIntent: TruthValueAssignment,
    override val previousState: Option[Initiator] = None,
    override val maxUtteranceLength: Option[Int] = None
) extends Interlocutor {

  assert(
    communicativeIntent.beliefs.forall(graph.vertices.contains),
    "Communicative intent contains beliefs not present in the belief network."
  )

  /** Computes a belief inference for an utterance from the other agents perspective by ignoring the
    * [[Initiator]]s own beliefs.
    * @param beliefs
    *   The utterance to compute the belief inference for.
    * @return
    */
  private def perspectiveTaking(beliefs: TruthValueAssignment): TruthValueAssignment = {
    val perspective = Initiator(
      graph = graph,
      negativeConstraints = negativeConstraints,
      ownBeliefs = TruthValueAssignment.emtpy,
      sharedBeliefs = sharedBeliefs ++ beliefs,
      communicativeIntent = TruthValueAssignment.emtpy,
      previousState = Some(this),
      maxUtteranceLength = maxUtteranceLength
    )
    perspective.allBeliefs
  }

  /** Computes <span style="font-variant-caps: normal;">Produce Utterance</span> for this
    * [[Initiator]].
    *
    * TODO Include updated LaTeX definition.
    * @return
    */
  def produceUtterance(): UtteranceInitiatorPair = {
    val allPossibleUtteranceBeliefs: Set[TruthValueAssignment] =
      if (maxUtteranceLength.isDefined)
        (powersetUp(graph.vertices \ sharedBeliefs.beliefs, maxUtteranceLength.get) \ Set.empty)
          .map(beliefSet => allBeliefs.subAssignment(beliefSet)) // Map the belief set to a tva
      else
        (powerset(graph.vertices \ sharedBeliefs.beliefs) \ Set.empty)
          .map(beliefSet => allBeliefs.subAssignment(beliefSet)) // Map the belief set to a tva

    def relativeStructuralSimilarity(utterance: TruthValueAssignment): Double = {
      1.0 / (utterance.size) * (communicativeIntent ~ perspectiveTaking(utterance))
    }

    val allPossibleOptimalUtterances =
      argMax(allPossibleUtteranceBeliefs, relativeStructuralSimilarity)
    val utteranceOption = allPossibleOptimalUtterances.random
    UtteranceInitiatorPair(
      utteranceOption,
      nextInitiator = this.addSharedBeliefs(utteranceOption.getOrElse(TruthValueAssignment.emtpy))
    )
  }

  /** Computes <span style="font-variant-caps: normal;">Perceived Mutual Understanding</span> for
    * this [[Initiator]], relative to an optional offer from the [[Responder]].
    *
    * TODO Include updated LaTeX definition.
    *
    * @param offer
    *   An optional restricted offer from a [[Responder]].
    * @return
    *   Yes, NotYet, or No understanding.
    */
  def perceivedMutualUnderstanding(offer: Option[TruthValueAssignment]): Understanding = {
    if (offer.isEmpty) Understandings.NotYet
    else {
      val perspective                      = perspectiveTaking(offer.get)
      def compare(belief: Belief): Boolean = allBeliefs(belief) == perspective(belief)
      if (forall(communicativeIntent.beliefs, compare)) Understandings.Yes
      else Understandings.No
    }
  }

  def repairSolution(offer: TruthValueAssignment): TruthValueAssignment = {
    assert(
      offer.beliefs /\ sharedBeliefs.beliefs == Set.empty,
      "Restricted offer contains previously communicated beliefs, something went wrong."
    )
    allBeliefs.subAssignment(offer.beliefs)
  }

  override protected def addSharedBeliefs(utterance: TruthValueAssignment): Initiator = Initiator(
    graph = graph,
    negativeConstraints = negativeConstraints,
    ownBeliefs = ownBeliefs,
    sharedBeliefs = sharedBeliefs ++ utterance,
    communicativeIntent = communicativeIntent,
    previousState = Some(this),
    maxUtteranceLength = maxUtteranceLength
  )

  //
////  override val inferredBeliefs: Map[Node[String], Boolean] =
////    if (previousState.isDefined)
////      previousState.get.inferredBeliefs // If not first time initiator, keep old beliefs.
////    else super.inferBeliefs()           // If first time initiator, infer beliefs from scratch.
//
//  /** Creates a copy of this initiator with only the initiator's prior beliefs and the communicated
//    * beliefs (including the utterance).
//    * @param utterance
//    *   A truth-value assignment for the uttered beliefs.
//    * @return
//    *   A simulated responder from the initiator's perspective.
//    */
//  private def simulateBelieveInferences(
//      utterance: Map[Node[String], Boolean]
//  ): Initiator =
//    Initiator(
//      beliefNetwork.addFoundationalAssignment(utterance),
//      priorBeliefs,
//      Map.empty,
//      None,
//      sharedBeliefs ++ utterance,
//      if (previousState.isDefined) Some(previousState.get.inferredBeliefs) else None,
//      maxUtteranceLength
//    )
//
//  override lazy val allBeliefTruthValueAssignments: Map[Node[String], Boolean] =
//    priorBeliefs ++ sharedBeliefs ++ inferredBeliefs ++ communicativeIntent
//
//  /** Based on (van Arkel, 2021, p. 28)
//    *
//    * @return
//    *   A truth-value assignment over any set of non-already communicated beliefs that
//    *   will most efficiently communicate the intent
//    */
//  def produceUtterance(): Map[Node[String], Boolean] = {
////    println("[Initiator.produceUtterance]")
////    assert(priorBeliefs.keySet /\ communicativeIntent.keySet == Set.empty)
//
//    val allPossibleUtterances: Set[Map[Node[String], Boolean]] =
//      powersetUp(
//        beliefNetwork.vertices \ sharedBeliefs.keySet,
//        utteranceLengthLimit
//      )
//        // Map each set of beliefs to its current truth-value mapping
//        .filterNot(
//          _.isEmpty
//        ) // Disallow empty utterance? TODO Check with computational-level theory.
//        .map((utterance: Set[Node[String]]) =>
//          utterance // Take the set of beliefs
//            .map((node: Node[String]) => (node, inferredBeliefs(node)))
//            .toMap
//        )
//
//    // Get utterance which will make the interpreter's
//    // beliefs most similar to our communicative intent
//    allPossibleUtterances
//      .argMax(utterance =>
//        structuralSimilarity(
//          simulateBelieveInferences(utterance),
//          communicativeIntent.keySet
//        )
//          / (utterance.size.doubleValue + 1)
//      )
//      .random
//      .get
//  }
//
//  /** Based on (van Arkel, 2021, p.31)
//    *
//    * @param repairRequest
//    *   Incoming truth-value assignment from the interpreter
//    * @return
//    *   A correction, in the form of a truth-value assignment, if the repairRequest is faulty
//    *   (beliefs have been assigned wrong) or else the unadjusted repairRequest
//    */
//  def repairSolution(
//      repairRequest: Map[Node[String], Boolean]
//  ): Map[Node[String], Boolean] = {
//    assert(
//      repairRequest.keySet /\ sharedBeliefs.keySet == Set.empty,
//      "Repair request contains previously communicated beliefs, something went wrong."
//    )
////    println("[Initiator.repairSolution]")
//
//    val isCorrectRequest: Boolean =
//      repairRequest.keySet
//        .forall((belief: Node[String]) => repairRequest(belief) == inferredBeliefs(belief))
//
//    if (isCorrectRequest) repairRequest
//    else {
//      repairRequest.keySet
//        .map((belief: Node[String]) => (belief, inferredBeliefs(belief)))
//        .toMap
//    }
//  }
//
//  /** Based on (van Arkel, 2021, p.33)
//    * @param repairRequest
//    *   Incoming truth-value assignment from the interpreter
//    * @return
//    *   'true' if the producer beliefs the interpreter has taken over the communicativeIntent, and
//    *   there is no repairRequest 'false' otherwise
//    */
//  def endConversation(
//      repairRequest: Option[Map[Node[String], Boolean]]
//  ): Boolean = {
////    println("[Initiator.endConversation]")
//    if (repairRequest.isEmpty) true
//    else {
//      val simulatedInterlocutor = simulateBelieveInferences(repairRequest.get)
//      // Infer if all nodes to be communicated have the correct truth-value assignment in the interpreter's network
//      communicativeIntent.keySet
//        .forall(belief =>
//          simulatedInterlocutor.allBeliefTruthValueAssignments(belief) == communicativeIntent(
//            belief
//          )
//        )
//    }
//  }
//
//  override def addCommunicatedBeliefs(utterance: Map[Node[String], Boolean]): Initiator =
//    Initiator(
//      beliefNetwork.addFoundationalAssignment(utterance),
//      priorBeliefs,
//      communicativeIntent,
//      previousState = Some(this),
//      sharedBeliefs = sharedBeliefs ++ utterance,
//      presetInferredBeliefs =
//        if (previousState.isDefined) Some(previousState.get.inferredBeliefs) else None,
//      maxUtteranceLength
//    )
//
//  def toDOTString(msg: String): String = {
//    val colorMap = beliefNetwork.vertices
//      .map(v =>
//        v ->
//          (List.empty :::
//            (if (priorBeliefs.contains(v)) List("deeppink") else List()) :::
//            (if (communicativeIntent.contains(v)) List("darkorange") else List()) :::
//            (if (sharedBeliefs.contains(v)) List("aquamarine") else List()))
//      )
//      .toMap
//    super.toDOTString("Initiator", Some(colorMap), Some(3), msg = msg)
//  }

}
