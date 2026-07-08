package computationalcognitivescience.coherencecommunication.coherence

import Belief.Belief
import mathlib.graph.GraphImplicits.{EdgeImpl, EdgeImpl2, WUnDiEdgeImpl}
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
      val mergedNodeSet       = graph.vertices.map(_.label).filter(_.startsWith("M"))
      val nextTrueMergeNodeID = if (mergedNodeSet.isEmpty) 0 else mergedNodeSet.map(_.toInt).max + 1
      val nextFalseMergeNodeID = nextTrueMergeNodeID + 1

      val nextMergedTrueBelief  = Belief(s"M$nextTrueMergeNodeID")
      val nextMergedFalseBelief = Belief(s"M$nextFalseMergeNodeID")

      val mergedBeliefs =
        graph.vertices \ trueBeliefs \ falseBeliefs + nextMergedTrueBelief + nextMergedFalseBelief

      val mergedConstraints = unassignedBeliefs
        .flatMap(ub => {
          val neighbours = graph.adjacencyList(ub)
          val trueNeighbours = neighbours.filter(trueBeliefs contains _.node)
          val mergedTrueConstraint = ub ~ nextMergedTrueBelief % trueNeighbours.toList.map(_.weight).sum
          val falseNeighbours = neighbours.filter(falseBeliefs contains _.node)
          val mergedFalseConstraint = ub ~ nextMergedFalseBelief % falseNeighbours.toList.map(_.weight).sum
          val regularConstraints = (neighbours \ trueNeighbours \ falseNeighbours).map(un =>
          {ub ~ un.node % un.weight})

          regularConstraints + mergedTrueConstraint + mergedFalseConstraint
        })


      Some(
        BeliefNetwork(
          new WUnDiGraph(mergedBeliefs, mergedConstraints),
          Set.empty,
          biasBeliefs + nextMergedTrueBelief + nextMergedFalseBelief,
          biasAssignment + (nextMergedTrueBelief -> true) + (nextMergedFalseBelief -> false)
        )
      )
    }
  }

  def toDOTString: String = {
    val vertices = graph.vertices
      .map(b => {
        val fillColor =
          if (biasAssignment.contains(b) && biasAssignment(b).get) "palegreen4"
          else if (biasAssignment.contains(b) && !biasAssignment(b).get) "lightsalmon"
          else "white"
        val color = if (b.label.startsWith("M")) "mediumslateblue" else "black"
        "\t" + b.label + s"[style=filled,fillcolor=$fillColor,color=$color];"
      })
      .mkString("\n")
    val constraints = edges
      .map(edge => {
        val style = if (negativeConstraints.contains(edge)) "dashed" else "solid"
        "\t" + edge.left.label + " -- " + edge.right.label +
          " [label=" + edge.weight + "style=" + style + "]"
      })
      .mkString("\n")
    s"""graph G {
        | layout=circo;
        | $vertices
        | $constraints
      }""".stripMargin

  }

  def cMin(): Set[TruthValueAssignment] = {
    def searchTree(searchTreeNode: BeliefNetwork): Set[BeliefNetwork] = {
      val pathsOption = searchTreeNode.ac1()
      if(pathsOption.isEmpty) Set(searchTreeNode)
      else {
        val (leftPath, rightPath) = pathsOption.get
        searchTree(leftPath) \/ searchTree(rightPath)
      }

    }
    val searchSpace = searchTree(this)
    searchSpace
      .map(network => network.ac2().getOrElse(network).ac3())
      .filter(_.isDefined)
      .map(_.get)
      .flatMap(bn => {
        val allMinCuts = MinCut.minCut(bn.graph)
        allMinCuts.map(mc => {
          val trueBeliefs = mc._1.map(b => b -> true)
          val falseBeliefs = mc._2.map(b => b -> false)
          TruthValueAssignment(trueBeliefs \/ falseBeliefs)
        })
      })
  }

}
