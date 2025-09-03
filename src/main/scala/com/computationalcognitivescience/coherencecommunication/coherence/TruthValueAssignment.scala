package com.computationalcognitivescience.coherencecommunication.coherence

import com.computationalcognitivescience.coherencecommunication.coherence.Belief.Belief
import com.computationalcognitivescience.coherencecommunication.util.SetTheoryDev._
import mathlib.set.SetTheory._

/** A datatype for truth-value assignments, consisting of a set of beliefs and a set of belief,
  * boolean pairs.
  * @param beliefs
  *   The set of beliefs.
  * @param truthValueAssignment
  *   The set of truth values for each belief.
  */
case class TruthValueAssignment(
    beliefs: Set[Belief],
    truthValueAssignment: Set[(Belief, Boolean)]
) {

  /** Truth-value assignment merge $++$ as defined in Definition 1. Here, $T_A$ is `this` instance
    * and $T_B$ is the `that` argument, and $A$ and $B$ are the sets of beliefs respectively.
    *
    * $$ (T_A ++ T_B)(x) \begin{cases} T_B(x) & \text{if}~x\in A~\text{and}~x\in B\\ T_A(x) &
    * \text{if}~x\in A\\ T_B(x) & \text{if}~x\in B\\ \text{undefined} & \text{otherwise} \end{cases}
    * $$
    *
    * @param that
    *   The truth-value assignment to merge with `this`.
    * @return
    */
  def merge(that: TruthValueAssignment): TruthValueAssignment =
    that.truthValueAssignment.foldLeft(this)(_ + _)

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
    *   The belief and its associated truth value to be added.
    * @return
    *   The updated truth value assignment.
    */
  def +(belief: (Belief, Boolean)): TruthValueAssignment =
    TruthValueAssignment(
      beliefs + belief._1,
      truthValueAssignment.filterNot(_._1 == belief._1) + belief
    )

  /** Returns a truth-value assignment that contains only the beliefs in `utteranceBeliefs`. This is
    * an implementation of Definition 2. Note that this returns an
    * empty truth-value assignment if none of the beliefs in `utteranceBeliefs` are in
    * `this.beliefs`.
    *
    * @param subset
    *   The subset of beliefs to return the value assignments for.
    * @return
    */
  def subAssignment(subset: Set[Belief]): TruthValueAssignment = TruthValueAssignment(
    beliefs = subset,
    truthValueAssignment = truthValueAssignment.filter(_._1 in subset)
  )

  /** Returns a truth-value assignment that contains only the beliefs in `utteranceBeliefs`. This is
   * an implementation of Definition 2. Note that this returns an
   * empty truth-value assignment if none of the beliefs in `utteranceBeliefs` are in
   * `this.beliefs`.
   *
   * @param subset
   *   The subset of beliefs to return the value assignments for.
   * @return
   */
  def <<(subset: Set[Belief]): TruthValueAssignment = subAssignment(subset)

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
    TruthValueAssignment(beliefs - belief, truthValueAssignment.filterNot(_._1 == belief))

  /** Removed the beliefs from `that` truth-value assignment from `this` one.
    *
    * @param that
    *   The truth-value assignment whose beliefs are to be removed.
    * @return
    *   The updated truth value assignment.
    */
  def \(that: TruthValueAssignment): TruthValueAssignment =
    this -- that.beliefs

  /** Shorthand for `merge`, truth-value assignment merge as defined in Definition 1.
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
  final def --(beliefs: Set[Belief]): TruthValueAssignment = {
    TruthValueAssignment(
      this.beliefs \ beliefs,
      this.truthValueAssignment.filterNot(_._1 in beliefs)
    )
  }

  /** Subset equivalence $\subset$.
    *
    * Let $T_A:A\rightarrow\{true,false\}$ be a truth-value assignment and let $B\subseteq V_A$.
    *
    * @param that
    *   The truth-value assignment to evaluate subset equivalence for.
    * @return
    *   Returns true if and only if all beliefs in `subset` are contained in both `this` and `that`,
    *   and the truth-value assignments are equal.
    */
  def subsetEquivalence(that: TruthValueAssignment): Boolean = {
    (this.beliefs subsetOf that.beliefs) &&
    forall(this.beliefs, (b: Belief) => this(b) == that(b))
  }

  /** Shorthand for `subsetEquivalence`, subset equivalence.
    * @param that
    *   The truth-value assignment to evaluate subset equivalence for.
    * @return
    *   Returns true if and only if all beliefs in `subset` are contained in both `this` and `that`,
    *   and the truth-value assignments are equal.
    */
  def <=(that: TruthValueAssignment): Boolean = this.subsetEquivalence(that)

  /** Structural similarity $\sim$ as defined in Definition 2. Returns the number of beliefs that
    * have the same truth value and are in both `this` and `that`. Here, $T_A$ is `this` instance
    * and $T_B$ is the `that` argument
    *
    * We define the structural similarity relative to the intersection of $A\cap B$ as the number of
    * equivalent truth-value assignments: $|\left\{x\in A \cap B \middle| T_A(x)=T_B(x)\right\}|$
    *
    * @param that
    *   The truth-value assignment to compute similarity against.
    * @return
    *   The number of equivalent beliefs.
    */
  def structuralSimilarity(that: TruthValueAssignment): Int =
    structuralSimilarity(that, this.beliefs)

  /** Shorthand for `structuralSimilarity`, as defined in Definition 2. Returns the number of
    * beliefs that have the same truth value and are in both `this` and `that`.
    *
    * @param that
    *   The truth-value assignment to compute similarity against.
    * @return
    *   The number of equivalent beliefs.
    */
  def ~(that: TruthValueAssignment): Int = structuralSimilarity(that)

  /** Relative structural similarity $\overset{C}{\sim}$. Returns the
    * number of beliefs in `subset` that have the same truth value and are in both `this` and
    * `that`. Here, $T_A$ is `this` instance and $T_B$ is the `that` argument: $|\left\{x\in C
    * \middle| T_A(x)=T_B(x)\right\}|$.
    *
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

  /** Asymmetry between two truth-value assignments, normalized by the size of the intersection:
    * $asymmetry(this, other)=1-\frac{this ~ other}{|this \cap other|}$
    * @param other
    *   The truth-value assignment to compute asymmetry with.
    * @return
    *   The normalized asymmetry.
    */
  def asymmetry(other: TruthValueAssignment): Double = {
    val intersection = this.beliefs /\ other.beliefs
    if (intersection.isEmpty) 0.0
    else 1.0 - (this ~ other).doubleValue / intersection.size
  }

  override def toString: String = this.truthValueAssignment.toList
    .sortBy(_._1.toString)
    .mkString("TruthValueAssignment(", ",", ")")

}

object TruthValueAssignment {

  /** Constructs a truth value assignment from on a set of pairs only.
    * @param truthValueAssignment
    *   Set of belief-Boolean pairs.
    * @return
    */
  def apply(truthValueAssignment: Set[(Belief, Boolean)]): TruthValueAssignment =
    TruthValueAssignment(truthValueAssignment.map(_._1), truthValueAssignment)

  /** Constructs an empty truth value assignment.
    * @return
    */
  def empty: TruthValueAssignment = TruthValueAssignment(Set.empty, Set.empty)

  implicit class ImplMap(map: Map[Belief, Boolean]) {

    /** Converts a map of beliefs and boolean values to a [[TruthValueAssignment]].
      * @return
      */
    def toTruthValueAssignment: TruthValueAssignment = TruthValueAssignment(
      beliefs = map.keySet,
      truthValueAssignment = map.toSet
    )
  }
}
