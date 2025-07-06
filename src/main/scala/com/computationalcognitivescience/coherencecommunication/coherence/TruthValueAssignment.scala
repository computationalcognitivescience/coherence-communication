package com.computationalcognitivescience.coherencecommunication.coherence

import com.computationalcognitivescience.coherencecommunication.coherence.Belief.Belief
import com.computationalcognitivescience.coherencecommunication.util.SetTheoryDev._
import mathlib.set.SetTheory._

import scala.annotation.tailrec

case class TruthValueAssignment(
    beliefs: Set[Belief],
    truthValueAssignment: Set[(Belief, Boolean)]
) {

  /** Truth-value assignment merge as defined in Definition 1. Here, $T_A$ is `this` instance and
    * $T_B$ is the `that` argument, and $A$ and $B$ are the sets of beliefs respectively:
    *
    * Let $T_A:A\rightarrow\{true,false\}$ and $T_B:B\rightarrow\{true,false\}$ be two truth-value
    * assignments. We define the merger $T_A\oplus T_B$ as an ordered relationship with $T_A$ taking
    * precedence if a belief $x\in A$ and $x\in B$: $$ (T_A\oplus T_B)(x) \begin{cases} T_A(x) &
    * \text{if } x\in A\\ T_B(x) & \text{if } x\in B\\ \text{undefined} & \text{thatwise}
    * \end{cases} $$
    *
    * @param that
    *   The truth-value assignment to merge with `this`.
    * @return
    */
  def merge(that: TruthValueAssignment): TruthValueAssignment = {
    val overlap = beliefs.intersect(that.beliefs)
    TruthValueAssignment(
      beliefs.union(that.beliefs),
      (this -- overlap).truthValueAssignment // Remove beliefs from this overlapping with that
        \/ that.truthValueAssignment         // Add beliefs from that
    )
  }

  /** Returns an option containing the associated truth value for `belief` if the belief is in this
    * truth-value assignment.
    * @param belief
    *   The belief for which the truth value is to be returned.
    * @return
    *   An option with the assigned truth value or None.
    */
  def apply(belief: Belief): Option[Boolean] =
    if (truthValueAssignment.exists(_._1 == belief))
      Some(truthValueAssignment.find(_._1 == belief).get._2)
    else None

  /** Is true is this truth-value assignment is not empty. */
  def nonEmpty: Boolean = beliefs.nonEmpty

  /** Is true is this truth-value assignment is empty. */
  def isEmpty: Boolean = beliefs.isEmpty

  /** Adds the truth value for a belief, will overwrite existing truth value is belief is in the
    * current truth value assignment.
   *
   * @param belief
    *   The belief to be added.
    * @param truthValue
    *   The truth value.
    * @return
    *   The updated truth value assignment.
    */
  def +(belief: Belief, truthValue: Boolean): TruthValueAssignment =
    TruthValueAssignment(
      beliefs + belief,
      truthValueAssignment.filter(_._1 == belief) + (belief -> truthValue)
    )

  /** Returns a truth-value assignment that contains only the beliefs in `utteranceBeliefs`. This is
   * the productive version of Definition 2 and [[subsetEquivalence()]]. Note that this returns an
   * empty truth-value assignment if none of the beliefs in `utteranceBeliefs` are in
   * `this.beliefs`.
   *
   * @param utteranceBeliefs
   * The subset of beliefs to return the value assignments for.
   * @return
   */
  def subAssignment(utteranceBeliefs: Set[Belief]): TruthValueAssignment = TruthValueAssignment(
    beliefs = utteranceBeliefs,
    truthValueAssignment = truthValueAssignment.filter(_._1 in utteranceBeliefs)
  )

  /** Returns the number of beliefs in the truth-value assignment. */
  def size: Int = beliefs.size

  /** Does this truth value assignment contain `belief`?
    *
    * @param belief
    *   The belief to test existence for.
    * @return
    */
  def contains(belief: Belief): Boolean = beliefs.contains(belief)

  /** Removed the truth value assignment for belief.
    *
    * @param belief
    *   The belief to be removed.
    * @return
    *   The updated truth value assignment.
    */
  def -(belief: Belief): TruthValueAssignment =
    TruthValueAssignment(beliefs - belief, truthValueAssignment.filter(_._1 == belief))

  /** Removed the beliefs from `that` truth-value assignment from `this` one.
    *
    * @param that
    *   The truth-value assignment whose beliefs are to be removed.
    * @return
    *   The updated truth value assignment.
    */
  def \(that: TruthValueAssignment): TruthValueAssignment =
    TruthValueAssignment(beliefs \ that.beliefs, truthValueAssignment.filter(_._1 in that.beliefs))

  /** Truth-value assignment merge as defined in Definition 1. Here, $T_A$ is `this` instance and
    * $T_B$ is the `that` argument, and $A$ and $B$ are the sets of beliefs respectively:
    *
    * Let $T_A:A\rightarrow\{true,false\}$ and $T_B:B\rightarrow\{true,false\}$ be two truth-value
    * assignments. We define the merger $T_A\oplus T_B$ as an ordered relationship with $T_A$ taking
    * precedence if a belief $x\in A$ and $x\in B$: $$ (T_A\oplus T_B)(x) \begin{cases} T_A(x) &
    * \text{if } x\in A\\ T_B(x) & \text{if } x\in B\\ \text{undefined} & \text{thatwise}
    * \end{cases} $$
    *
    * @param truthValueAssignment
    *   The truth-value assignment to merge with `this`.
    * @return
    */
  def ++(truthValueAssignment: TruthValueAssignment): TruthValueAssignment =
    this.merge(truthValueAssignment)

  /** Removed all beliefs from the truth-value assignment.
    * @param beliefs
    *   The set of beliefs to be removed.
    * @return
    *   The updated truth value assignment.
    */
  @tailrec
  final def --(beliefs: Set[Belief]): TruthValueAssignment = {
    if(beliefs.isEmpty) this
    else {
      val (head, tail) = beliefs.splitAt(1)
      if (tail.isEmpty)
        return this - head.head
      (this - head.head) -- tail
    }
  }

  /** Subset equivalence as defined in Definition 2. Returns true if and only if all beliefs in
    * `subset` are contained in both `this` and `that`, and the truth-value assignments are equal.
    *
    * Let $T_A:A\rightarrow\{true,false\}$ be a truth-value assignment and let $B\subseteq V_A$. We
    * define the subset $T_B\mathrel{\overset{B}{\subset}} T_A$ as $T_B(x)=T_A(x)$, for all $x\in
    * B$.
    * @param that
    *   The truth-value assignment to evaluate subset equivalence for.
    * @return
    */
  def subsetEquivalence(that: TruthValueAssignment): Boolean = {
    (that.beliefs subsetOf this.beliefs) &&
    forall(that.beliefs, (b: Belief) => this(b) == that(b))
  }

  /** Subset equivalence as defined in Definition 2. Returns true if and only if all beliefs in
    * `subset` are contained in both `this` and `that`, and the truth-value assignments are equal.
    *
    * Let $T_A:A\rightarrow\{true,false\}$ be a truth-value assignment and let $B\subseteq V_A$. We
    * define the subset $T_B\mathrel{\overset{B}{\subset}} T_A$ as $T_B(x)=T_A(x)$, for all $x\in
    * B$.
    * @param that
    *   The truth-value assignment to evaluate subset equivalence for.
    * @return
    */
  def <=(that: TruthValueAssignment): Boolean = this.subsetEquivalence(that)

  /** Structural similarity as defined in Definition 3. Returns the number of beliefs that have the
    * same truth value and are in both `this` and `that`.
    *
    * Structural similarity $\sim$] Let $T_A:A\rightarrow\{true,false\}$ and
    * $T_B:B\rightarrow\{true,false\}$ be two truth-value assignments. We define the structural
    * similarity relative to the intersection of $A\cap B$ as the number of equivalent truth-value
    * assignments: $|\left\{x\in A \cap B \middle| T_A(x)=T_B(x)\right\}|$
    *
    * @param that
    *   The truth-value assignment to compute similarity against.
    * @return
    *   The number of equivalent beliefs.
    */
  def structuralSimilarity(that: TruthValueAssignment): Int = {
    val intersectingBeliefs              = beliefs /\ that.beliefs
    def compare(belief: Belief): Boolean = this(belief) == that(belief)
    (intersectingBeliefs | compare _).size
  }

  /** Structural similarity as defined in Definition 3. Returns the number of beliefs that have the
    * same truth value and are in both `this` and `that`.
    *
    * Structural similarity $\sim$] Let $T_A:A\rightarrow\{true,false\}$ and
    * $T_B:B\rightarrow\{true,false\}$ be two truth-value assignments. We define the structural
    * similarity relative to the intersection of $A\cap B$ as the number of equivalent truth-value
    * assignments: $|\left\{x\in A \cap B \middle| T_A(x)=T_B(x)\right\}|$
    *
    * @param that
    *   The truth-value assignment to compute similarity against.
    * @return
    *   The number of equivalent beliefs.
    */
  def ~(that: TruthValueAssignment): Int = structuralSimilarity(that)

  /** Relative structural similarity as defined in Definition 4. Returns the number of beliefs that
    * have the same truth value and are in both `this` and `that` and in `subset`.
    * @param that
    *   The truth-value assignment to compute similarity against.
    * @param subset
    *   The subset to compute structural similarity for.
    * @return
    *   The number of equivalent beliefs.
    */
  def structuralSimilarity(that: TruthValueAssignment, subset: Set[Belief]): Int = {
    val intersectingBeliefs              = beliefs /\ that.beliefs /\ subset
    def compare(belief: Belief): Boolean = this(belief) == that(belief)
    (intersectingBeliefs | compare _).size
  }

  /** Relative structural similarity as defined in Definition 4. Returns the number of beliefs that
    * have the same truth value and are in both `this` and `that` and in `subset`.
    * @param that
    *   The truth-value assignment to compute similarity against.
    * @param subset
    *   The subset to compute structural similarity for.
    * @return
    *   The number of equivalent beliefs.
    */
  def ~(subset: Set[Belief])(that: TruthValueAssignment): Int =
    this.structuralSimilarity(that, subset)


}

object TruthValueAssignment {

  /** Constructs a truth value assignment based on a set of pairs only.
    * @param truthValueAssignment
    *   Set of belief-Boolean pairs.
    * @return
    */
  def apply(truthValueAssignment: Set[(Belief, Boolean)]): TruthValueAssignment =
    TruthValueAssignment(truthValueAssignment.map(_._1), truthValueAssignment)

  /** Constructs an empty truth value assignment.
    * @return
    */
  def emtpy: TruthValueAssignment = TruthValueAssignment(Set.empty, Set.empty)

  implicit class ImplMap(map: Map[Belief, Boolean]) {
    def toTruthValueAssignment: TruthValueAssignment = TruthValueAssignment(
      beliefs = map.keySet,
      truthValueAssignment = map.toSet
    )
  }
}


