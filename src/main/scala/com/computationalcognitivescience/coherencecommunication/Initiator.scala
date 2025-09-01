package com.computationalcognitivescience.coherencecommunication

import com.computationalcognitivescience.coherencecommunication.datastructures.Understandings._
import com.computationalcognitivescience.coherencecommunication.coherence.Belief.Belief
import com.computationalcognitivescience.coherencecommunication.util.SetTheoryDev._
import com.computationalcognitivescience.coherencecommunication.coherence.{
  FoundationalBeliefNetwork,
  TruthValueAssignment
}
import com.computationalcognitivescience.coherencecommunication.datastructures.Understandings
import mathlib.set.SetTheory._
import mathlib.graph._

/** Class that implements all functions and data structures related to simulating the initiator.
  *
  * @param graph
  *   The graph representing the belief network.
  * @param negativeConstraints
  *   The set of negative constraints.
  * @param ownBeliefs
  *   The set of the interlocutors own beliefs $T_{own}$.
  * @param sharedBeliefs
  *   The set of the shared (communicated) beliefs $T_{shared}$.
  * @param communicativeIntent
  *   The truth-value assignment representing the initator's communicative intent.
  * @param previousState
  *   An optional previous state of the initiator to provide access to the responder's previous
  *   truth-value assignment $T_{prev}$.
  * @param previousPerspectiveState
  *   An optional previous state of the simulated perspective of the responder.
  * @param maxUtteranceLength
  *   The maximum number of beliefs that can be communicated in one utterance $k$.
  */
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

  /** The simulated perspective of the responder. */
  val perspectiveState: Option[Responder] =
    if (previousPerspectiveState.isDefined) previousPerspectiveState
    else
      Some(
        Responder(
          graph = graph,
          negativeConstraints = negativeConstraints,
          ownBeliefs = ownBeliefs,
          sharedBeliefs = TruthValueAssignment.empty,
          previousState = None,
          maxUtteranceLength = maxUtteranceLength
        )
      )

  assert(
    communicativeIntent.beliefs.forall(graph.vertices.contains),
    "[ERROR] Communicative intent contains beliefs not present in the belief network."
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
    perspectiveState.get.addSharedBeliefs(utterance).allBeliefs

  /** Computes <span style="font-variant-caps: small-caps;">Produce Utterance</span> for this
    * [[Initiator]].
    *
    * @return
    */
  def produceUtterance(): TruthValueAssignment = {
    val k = maxUtteranceLength.getOrElse(graph.size)

    val allPossibleUtteranceSubsets =
      powersetUp(graph.vertices \ sharedBeliefs.beliefs, k) - Set.empty
    val allPossibleUtterances: Set[TruthValueAssignment] = allPossibleUtteranceSubsets
      .map(beliefSet => allBeliefs.subAssignment(beliefSet)) // Map the belief set to a tva

    def relativeStructuralSimilarity(utterance: TruthValueAssignment): Double = {
      val perspective: TruthValueAssignment = perspectiveTaking(utterance)
      val similarity: Int                   = communicativeIntent ~ perspective
      if (similarity == 0) 0.0
      else (1.0 / utterance.size) * similarity
    }

    val allPossibleUtteranceBeliefsWithSimilarity =
      allPossibleUtterances.map(u => u -> relativeStructuralSimilarity(u))
    val max = allPossibleUtteranceBeliefsWithSimilarity.map(_._2).max
    val allPossibleUtteranceBeliefsWithMaxSimilarity =
      allPossibleUtteranceBeliefsWithSimilarity.filter(_._2 == max).map(_._1)
    allPossibleUtteranceBeliefsWithMaxSimilarity.random.get
  }

  /** Computes <span style="font-variant-caps: small-caps;">Perceived Mutual Understanding</span>
    * for this [[Initiator]], relative to an optional offer from the [[Responder]].
    *
    * @param reply
    *   A (possibly empty) reply to a restricted offer from a [[Responder]].
    * @return
    *   Yes, YesLiteral or NotYet understanding.
    */
  def perceivedMutualUnderstanding(reply: TruthValueAssignment): Understanding = {

    if (reply.isEmpty && communicativeIntent <= sharedBeliefs)
      // Case 1a: Literal understanding
      Understandings.YesLiteral
    else if (reply.nonEmpty && communicativeIntent <= sharedBeliefs ++ reply)
      // Case 1b: Literal understanding after reply
      Understandings.YesLiteral
    else if (reply.nonEmpty && communicativeIntent <= perspectiveTaking(reply))
      // Case 2: Perceived understanding after perspective taking
      Understandings.YesPerceived
    else
      // Otherwise: Not yet
      Understandings.NotYet
  }

  /** Calculates a reply to a restricted offer. If the offer is empty, the reply is empty.
    *
    * @param offer
    *   A (possibly empty) restricted offer.
    * @throws java.lang.AssertionError
    *   Restricted offer should not contain previously communicated beliefs.
    * @return
    *   A reply to the restricted offer.
    */
  @throws(classOf[AssertionError])
  def repairSolution(offer: TruthValueAssignment): TruthValueAssignment = {
    assert(
      offer.beliefs /\ sharedBeliefs.beliefs == Set.empty,
      "[ERROR] Restricted offer contains previously communicated beliefs, something went wrong."
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
    previousPerspectiveState = Some(perspectiveState.get.addSharedBeliefs(utterance)),
    maxUtteranceLength = maxUtteranceLength
  )
}
