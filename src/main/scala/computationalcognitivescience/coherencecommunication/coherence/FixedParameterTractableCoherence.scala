package computationalcognitivescience.coherencecommunication.coherence

import computationalcognitivescience.coherencecommunication.coherence.Belief.Belief
import mathlib.graph._
import mathlib.set.SetTheory._

trait FixedParameterTractableCoherence extends BaseBeliefNetwork {
  val graph: WUnDiGraph[String]
  val negativeConstraints: Set[WUnDiEdge[Belief]]
  override val positiveConstraints: Set[WUnDiEdge[Belief]] = graph.edges \ negativeConstraints

  /** Given a random negative belief, it can either be true or false.
    *
    * van Rooij, I. (2008). The Tractable Cognition Thesis. _Cognitive Science: A Multidisciplinary
    * Journal, 32_(6), 939–984. [[https://doi.org/10.1080/03640210801897856]]
    *
    * @return
    *   Two paths in the search tree.
    */
  def ac1(): Option[(BaseBeliefNetwork, BaseBeliefNetwork)]

  /** ''For constraints with both endpoints in $A' \cup R$ we can simply check whether or not they
    * are satisfied by the assignment $A' \cup R'$, delete them from the network, and update c
    * accordingly.''
    *
    * van Rooij, I. (2008). The Tractable Cognition Thesis. _Cognitive Science: A Multidisciplinary
    * Journal, 32_(6), 939–984. [[https://doi.org/10.1080/03640210801897856]]
    *
    * @return
    */
  def ac2(): Option[BaseBeliefNetwork]

  /** ''(AC 3), is applied only if rule (AC 2) does not apply. The rule (AC 3) is based on the
    * observation that for all elements [that are assigned true] we can merge them into one single
    * element s without affecting the amount of coherence in the network; similarly, for all
    * elements in [that are assigned false] we can merge them into one single element t.''
    *
    * @return
    */
  def ac3(): Option[BaseBeliefNetwork]

}
