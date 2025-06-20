package com.computationalcognitivescience.coherencecommunication.coherence

import com.computationalcognitivescience.coherencecommunication.coherence.Belief.Belief
import mathlib.graph.Node

case class TruthValueAssignment(
    beliefs: Set[Belief],
    truthValueAssignment: Set[(Belief, Boolean)]
) {

  /** Truth-value assignment merge as defined in Definition 1. Here, $T_A$ is `this` instance and
    * $T_B$ is the `other` argument, and $A$ and $B$ are the sets of beliefs respectively:
    *
    * Let $T_A:A\rightarrow\{true,false\}$ and $T_B:B\rightarrow\{true,false\}$ be two truth-value
    * assignments. We define the merger $T_A\oplus T_B$ as an ordered relationship with $T_A$ taking
    * precedence if a belief $x\in A$ and $x\in B$: $$ (T_A\oplus T_B)(x) \begin{cases} T_A(x) &
    * \text{if } x\in A\\ T_B(x) & \text{if } x\in B\\ \text{undefined} & \text{otherwise}
    * \end{cases} $$
    *
    * @param other
    *   The truth-value assignment to merge with `this`.
    * @return
    */
  def merge(other: TruthValueAssignment): TruthValueAssignment = {
    val overlap = beliefs.intersect(other.beliefs)
    TruthValueAssignment(
      beliefs.union(other.beliefs),
      other.truthValueAssignment
        .filterNot(tva => overlap.contains(tva._1))
        .union(truthValueAssignment)
    )
  }

  // TODO Replace with mathlib import when update is published.
  private def forall[A](set: Set[A], f: A => Boolean): Boolean = set.forall(f)
  private def exists[A](set: Set[A], f: A => Boolean): Boolean = set.exists(f)

  /** Subset equivalence returns true if and only if all beliefs in `subset` are contained in both
    * `this` and `other`, and the truth-value assignments are equal.
    *
    * Let $T_A:A\rightarrow\{true,false\}$ be a truth-value assignment and let $B\subseteq V_A$. We
    * define the subset $T_B\mathrel{\overset{B}{\subset}} T_A$ as $T_B(x)=T_A(x)$, for all $x\in
    * B$.
    * @param other
    *   The truth-value assignment to evaluate subset equivalence for.
    * @return
    */
  def subsetEquivalence(other: TruthValueAssignment): Boolean = {
    (other.beliefs subsetOf this.beliefs) &&
    forall(other.beliefs, (b: Belief) => this(b) == other(b))
  }

  /** Returns an option containing the associated truth value for `belief` if the belief is in this
    * truth-value assignment.
    * @param belief
    *   The belief for which the truth value is to be returned.
    * @return
    *   An option with the assigned truth value or None.
    */
  def apply(belief: Node[String]): Option[Boolean] =
    if (truthValueAssignment.exists(_._1 == belief))
      Some(truthValueAssignment.find(_._1 == belief).get._2)
    else None
}
