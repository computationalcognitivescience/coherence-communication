package com.computationalcognitivescience.coherencecommunication

import com.computationalcognitivescience.coherencecommunication.Understandings._
import com.computationalcognitivescience.coherencecommunication.coherence.Belief.Belief
import com.computationalcognitivescience.coherencecommunication.util.SetTheoryDev._
import com.computationalcognitivescience.coherencecommunication.coherence.{
  BeliefNetwork,
  FoundationalBeliefNetwork,
  TruthValueAssignment
}
import mathlib.set.SetTheory._
import mathlib.graph._

case class Initiator(
    override val graph: WUnDiGraph[String],
    override val negativeConstraints: Set[WUnDiEdge[Belief]],
    override val ownBeliefs: TruthValueAssignment,
    override val sharedBeliefs: TruthValueAssignment,
    communicativeIntent: TruthValueAssignment,
    override val previousState: Option[Initiator] = None,
    previousPerspectiveState: Option[Responder] = None,
    override val maxUtteranceLength: Option[Int] = None
) extends Interlocutor {
  // Ensure that the beliefs of the interlocutor stay the same, even when multiple optimal truth-value assignments
  // are possible that are equally similar to the previous state.
  override lazy val allBeliefs: TruthValueAssignment =
    if (previousState.isDefined) previousState.get.allBeliefs
    else beliefInference()

  val perspectiveState: Option[Responder] =
    if (previousPerspectiveState.isDefined) previousPerspectiveState
    else
      Some(
        Responder(
          graph = graph,
          negativeConstraints = negativeConstraints,
          ownBeliefs = TruthValueAssignment.empty,
          sharedBeliefs = TruthValueAssignment.empty,
          previousState = None,
          maxUtteranceLength = maxUtteranceLength
        )
      )

  assert(
    communicativeIntent.beliefs.forall(graph.vertices.contains),
    "Communicative intent contains beliefs not present in the belief network."
  )

  override val foundationalBeliefNetwork: FoundationalBeliefNetwork = FoundationalBeliefNetwork(
    graph = graph,
    negativeConstraints = negativeConstraints,
    priorBeliefs = ownBeliefs.beliefs \/ sharedBeliefs.beliefs \/ communicativeIntent.beliefs,
    priorBeliefsAssignment = ownBeliefs ++ sharedBeliefs ++ communicativeIntent
  )

  /** Computes a belief inference for an utterance from the other agents perspective by ignoring the
    * [[Initiator]]s own beliefs.
    * @param utterance
    *   The utterance to compute the belief inference for.
    * @return
    */
  private def perspectiveTaking(utterance: TruthValueAssignment): TruthValueAssignment =
    perspectiveState.get.addSharedBeliefs(sharedBeliefs ++ utterance).allBeliefs

  /** Computes <span style="font-variant-caps: normal;">Produce Utterance</span> for this
    * [[Initiator]].
    *
    * TODO Include updated LaTeX definition.
    * @return
    */
  def produceUtterance(): TruthValueAssignment = {
    val allPossibleUtteranceBeliefs: Set[TruthValueAssignment] =
      if (maxUtteranceLength.isDefined)
        (powersetUp(graph.vertices \ sharedBeliefs.beliefs, maxUtteranceLength.get) - Set.empty)
          .map(beliefSet => allBeliefs.subAssignment(beliefSet)) // Map the belief set to a tva
      else
        (powerset(graph.vertices \ sharedBeliefs.beliefs) - Set.empty)
          .map(beliefSet => allBeliefs.subAssignment(beliefSet)) // Map the belief set to a tva

    def relativeStructuralSimilarity(utterance: TruthValueAssignment): Double = {
      val perspective = perspectiveTaking(utterance)
      val similarity  = communicativeIntent ~ perspective
      if (similarity == 0) 0.0
      else (1.0 / utterance.size) * similarity
    }

//    val bla = argMax(allPossibleUtteranceBeliefs, relativeStructuralSimilarity)
    val bla  = allPossibleUtteranceBeliefs.map(u => u -> relativeStructuralSimilarity(u))
    val max  = bla.map(_._2).max
    val bla2 = bla.filter(_._2 == max).map(_._1)
    if (bla2.isEmpty) bla.foreach(println)
    bla2.random.get
  }

  /** Computes <span style="font-variant-caps: normal;">Perceived Mutual Understanding</span> for
    * this [[Initiator]], relative to an optional offer from the [[Responder]].
    *
    * TODO Include updated LaTeX definition.
    *
    * @param offer
    *   An optional restricted offer from a [[Responder]].
    * @return
    *   Yes, YesLiteral, NotYet, or No understanding.
    */
  def perceivedMutualUnderstanding(offer: Option[TruthValueAssignment]): Understanding = {
    val possibleReply = repairSolution(offer.getOrElse(TruthValueAssignment.empty))
    if (forall(communicativeIntent.beliefs, (sharedBeliefs ++ possibleReply).contains))
      // All intention beliefs have been literally communicated already or will be after reply to this offer
      Understandings.YesLiteral
    else {
      val perspective =
        if (offer.isDefined) perspectiveState.get.addSharedBeliefs(repairSolution(offer.get))
        else perspectiveState.get
      if (forall(communicativeIntent.beliefs, (b: Belief) => perspective.allBeliefs(b) == communicativeIntent(b) ))
        Understandings.YesPerceived
      else Understandings.NotYet
    }

//    if (offer.isEmpty && forall(communicativeIntent.beliefs, sharedBeliefs.contains))
//      Understandings.YesLiteral
//    else if (offer.isEmpty) {
//      val perspective = perspectiveTaking(TruthValueAssignment.empty)
//      val perspectiveUnderstanding =
//        forall(communicativeIntent.beliefs, (b: Belief) => communicativeIntent(b) == perspective(b))
//      if (perspectiveUnderstanding) Understandings.YesPerceived
//      else Understandings.NoPerceived
//    } else {
//      val allIntentionsShared: Boolean =
//        forall(communicativeIntent.beliefs, (offer.get.beliefs \/ sharedBeliefs.beliefs).contains
//
//      lazy val perspectiveUnderstanding: Boolean =
//        forall(communicativeIntent.beliefs, (b: Belief) => communicativeIntent(b) == perspectiveTaking(offer.get)(b))
//
//      if (allIntentionsShared || perspectiveUnderstanding) Understandings.YesConfirmed
//      else Understandings.No
//    }
  }

  def repairSolution(offer: TruthValueAssignment): TruthValueAssignment = {
//    println(offer)
    assert(
      offer.beliefs /\ sharedBeliefs.beliefs == Set.empty,
      "Restricted offer contains previously communicated beliefs, something went wrong."
    )
    allBeliefs.subAssignment(offer.beliefs)
  }

  override def addSharedBeliefs(utterance: TruthValueAssignment): Initiator = Initiator(
    graph = graph,
    negativeConstraints = negativeConstraints,
    ownBeliefs = ownBeliefs,
    sharedBeliefs = sharedBeliefs ++ utterance,
    communicativeIntent = communicativeIntent,
    previousState = Some(this),
    previousPerspectiveState = previousPerspectiveState,
    maxUtteranceLength = maxUtteranceLength
  )
}
