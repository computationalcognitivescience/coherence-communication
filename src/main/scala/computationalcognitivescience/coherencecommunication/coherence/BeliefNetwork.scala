package computationalcognitivescience.coherencecommunication.coherence

import Belief.Belief
import mathlib.graph.GraphImplicits.{EdgeImpl, WUnDiEdgeImpl}
import mathlib.graph._
import mathlib.set.SetTheory._

/** A belief network represented by a weighted undirected graph with negative cohering beliefs.
  * Positive constraints are defined as all non-negative constraints.
  * @param graph
  *   A weighted directed graph, where the vertices with string values represent beliefs.
  * @param negativeConstraints
  *   A subset of the graph's edges that represent negative constraints.
  */
case class BeliefNetwork(
    graph: WUnDiGraph[String],
    negativeConstraints: Set[WUnDiEdge[Belief]],
    biasBeliefs: Set[Belief],
    biasAssignment: TruthValueAssignment
) extends BaseBeliefNetwork
    with FixedParameterTractableCoherence {
  private val unassignedBeliefs: Set[Node[String]] = graph.vertices \ biasBeliefs

  /** Given a random unassigned belief connected to a negative constraint, it can either be true or
    * false.
    *
    * van Rooij, I. (2008). The Tractable Cognition Thesis. _Cognitive Science: A Multidisciplinary
    * Journal, 32_(6), 939–984. [[https://doi.org/10.1080/03640210801897856]]
    *
    * @return
    *   Two paths in the search tree.
    */
  override def ac1(): Option[(BeliefNetwork, BeliefNetwork)] = {
    val b = unassignedBeliefs.find(ub => negativeConstraints.exists(nc => nc.contains(ub)))

    if (b.isEmpty) None
    else {
      val leftPath = BeliefNetwork(
        graph,
        negativeConstraints,
        biasBeliefs + b.get,
        biasAssignment + (b.get -> true)
      )
      val rightPath = BeliefNetwork(
        graph,
        negativeConstraints,
        biasBeliefs + b.get,
        biasAssignment + (b.get -> false)
      )
      Some((leftPath, rightPath))
    }
  }

  /** ''For constraints with both endpoints [having a truth-value assignment] we can simply check
    * whether or not they are satisfied by the assignment [having a truth-value assignment], delete
    * them from the network, and update c accordingly.''
    *
    * van Rooij, I. (2008). The Tractable Cognition Thesis. _Cognitive Science: A Multidisciplinary
    * Journal, 32_(6), 939–984. [[https://doi.org/10.1080/03640210801897856]]
    *
    * @return
    */
  override def ac2(): Option[BeliefNetwork] = {
    val constraintsWithBothBeliefsAssigned = graph.edges
      .filter((constraint: WUnDiEdge[Belief]) =>
        biasAssignment.contains(constraint.left) && biasAssignment.contains(constraint.right)
      )
    if (constraintsWithBothBeliefsAssigned.isEmpty) None
    else
      Some(
        BeliefNetwork(
          graph - constraintsWithBothBeliefsAssigned,
          negativeConstraints \ constraintsWithBothBeliefsAssigned,
          biasBeliefs,
          biasAssignment
        )
      )
  }

  /** ''(AC 3), is applied only if rule (AC 2) does not apply. The rule (AC 3) is based on the
    * observation that for all elements [that are assigned true] we can merge them into one single
    * element s without affecting the amount of coherence in the network; similarly, for all
    * elements in [that are assigned false] we can merge them into one single element t.''
    *
    * @return
    */
  override def ac3(): Option[BeliefNetwork] = {
    val trueBeliefs  = biasAssignment.truthValueAssignment.filter(_._2 == true).map(_._1)
    val falseBeliefs = biasAssignment.truthValueAssignment.filter(_._2 == false).map(_._1)

    if (trueBeliefs.isEmpty && falseBeliefs.isEmpty) None
    else {
      val nextTrueMergeNodeID =
        graph.vertices.map(_.label).filter(_.startsWith("M")).map(_.toInt).max + 1
      val nextFalseMergeNodeID = nextTrueMergeNodeID + 1

      val nextMergedTrueBelief  = Belief(s"M$nextTrueMergeNodeID")
      val nextMergedFalseBelief = Belief(s"M$nextFalseMergeNodeID")

      val mergedBeliefs =
        graph.vertices \ trueBeliefs \ falseBeliefs + nextMergedTrueBelief + nextMergedFalseBelief
      val mergedNegativeConstraints = negativeConstraints.map((constraint: WUnDiEdge[Belief]) => {
        if (constraint.left in trueBeliefs)
          constraint.right ~ nextMergedTrueBelief % constraint.weight
        else if (constraint.right in trueBeliefs)
          constraint.left ~ nextMergedTrueBelief % constraint.weight
        else if (constraint.left in falseBeliefs)
          constraint.right ~ nextMergedFalseBelief % constraint.weight
        else if (constraint.right in falseBeliefs)
          constraint.left ~ nextMergedFalseBelief % constraint.weight
        else constraint
      })
      val mergedPositiveConstraints = positiveConstraints.map((constraint: WUnDiEdge[Belief]) => {
        if (constraint.left in trueBeliefs)
          constraint.right ~ nextMergedTrueBelief % constraint.weight
        else if (constraint.right in trueBeliefs)
          constraint.left ~ nextMergedTrueBelief % constraint.weight
        else if (constraint.left in falseBeliefs)
          constraint.right ~ nextMergedFalseBelief % constraint.weight
        else if (constraint.right in falseBeliefs)
          constraint.left ~ nextMergedFalseBelief % constraint.weight
        else constraint
      })

      Some(
        BeliefNetwork(
          new WUnDiGraph(mergedBeliefs, mergedNegativeConstraints \/ mergedPositiveConstraints),
          mergedNegativeConstraints,
          biasBeliefs + nextMergedTrueBelief + nextMergedFalseBelief,
          biasAssignment + (nextMergedTrueBelief -> true) + (nextMergedFalseBelief -> false)
        )
      )
    }
  }

  def cMin(): Set[BeliefNetwork] = {
    def searchTree(currentNetwork: BeliefNetwork): Set[BeliefNetwork] = {
      val next = currentNetwork.ac1()
      if(next.isEmpty) Set(currentNetwork)
      else {
        val (left, right) = next.get
        Set(currentNetwork) \/ searchTree(left) \/ searchTree(right)
      }
    }
    searchTree(this)
  }

}
