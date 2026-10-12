package computationalcognitivescience.coherencecommunication.coherence

import Belief.Belief
import computationalcognitivescience.coherencecommunication.coherence.graph.WUnDiCut.ImplWUnDiGraph
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

  def mapBeliefs(f: Belief => Belief): BeliefNetwork = BeliefNetwork(
    WUnDiGraph(graph.vertices.map(f), graph.edges),
    negativeConstraints,
    biasBeliefs,
    biasAssignment
  )

  def mapConstraints(f: WUnDiEdge[Belief] => WUnDiEdge[Belief]): BeliefNetwork = BeliefNetwork(
    WUnDiGraph(graph.vertices, graph.edges.map(f)),
    negativeConstraints,
    biasBeliefs,
    biasAssignment
  )

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
    val b = unassignedBeliefs.find(ub => negativeConstraints.exists(_.contains(ub)))
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
    * whether or not they are satisfied by the assignment, delete them from the network, and update
    * c accordingly.''
    *
    * van Rooij, I. (2008). The Tractable Cognition Thesis. _Cognitive Science: A Multidisciplinary
    * Journal, 32_(6), 939–984. [[https://doi.org/10.1080/03640210801897856]]
    *
    * @return
    */
  override def ac2(): BeliefNetwork = {
    val constraintsWithBothBeliefsAssigned = graph.edges
      .filter((constraint: WUnDiEdge[Belief]) =>
        biasAssignment.contains(constraint.left) && biasAssignment.contains(constraint.right)
      )
    if (constraintsWithBothBeliefsAssigned.isEmpty) this
    else
      BeliefNetwork(
        graph - constraintsWithBothBeliefsAssigned,
        negativeConstraints \ constraintsWithBothBeliefsAssigned,
        biasBeliefs,
        biasAssignment
      )

  }

  /** ''The rule (AC 3) is based on the observation that for all elements [that are assigned true]
    * we can merge them into one single element s without affecting the amount of coherence in the
    * network; similarly, for all elements in [that are assigned false] we can merge them into one
    * single element t.''
    *
    * @return
    */
  override def ac3(): BeliefNetwork = {
    val trueBeliefs  = biasAssignment.trueBeliefs
    val falseBeliefs = biasAssignment.falseBeliefs

    if (trueBeliefs.isEmpty && falseBeliefs.isEmpty) {
      println("shouldn't happen")
      this // shouldn't happen unless network has no constraints
    } else {
      val mergeNodeTrue  = graph.vertices.find(_.label == "T").getOrElse(Belief("T"))
      val mergeNodeFalse = graph.vertices.find(_.label == "F").getOrElse(Belief("F"))

      println(this.toDOTString)
      val mergedGraph = graph
        .mergeVertices(falseBeliefs, mergeNodeFalse)
        .mergeVertices(trueBeliefs, mergeNodeTrue)

      val mergedNet = BeliefNetwork(
        mergedGraph,
        Set.empty,
        biasBeliefs + mergeNodeFalse + mergeNodeTrue,
        biasAssignment + (mergeNodeFalse -> false) + (mergeNodeTrue -> true)
      )
      println(mergedNet.toDOTString)
      mergedNet
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
          " [label=" + edge.weight + ",style=" + style + "]"
      })
      .mkString("\n")
    s"""graph G {
        | layout=circo;
        | $vertices
        | $constraints
      }""".stripMargin

  }

  def cMin(): Set[TruthValueAssignment] = {
    def branchOnNegativeConstraints(
        searchTreeNode: BeliefNetwork
    ): Set[BeliefNetwork] = {
      searchTreeNode.ac1() match {
        case None => Set(searchTreeNode)
        case Some((leftBranch, rightBranch)) =>
          branchOnNegativeConstraints(leftBranch) \/ branchOnNegativeConstraints(rightBranch)
      }
    }

    println(s"bias = $biasBeliefs")
    println(s"ncs  = $negativeConstraints")

    val kernels = branchOnNegativeConstraints(this).map(network => network.ac2().ac3())

    kernels.flatMap(kernel => {
      val minCutSolutions = kernel.graph
        .minCut()
        .filter(cut => {
          (Belief("T") in cut.left.vertices) && (Belief("F") in cut.right.vertices) ||
          (Belief("T") in cut.right.vertices) && (Belief("F") in cut.left.vertices)
        })

      minCutSolutions.map(minCut => {

        val leftBeliefs =
          if (Belief("T") in minCut.left.vertices)
            minCut.left.vertices.map(b => b -> true)
          else
            minCut.left.vertices.map(b => b -> false)

        val rightBeliefs =
          if (Belief("T") in minCut.right.vertices)
            minCut.right.vertices.map(b => b -> true)
          else
            minCut.right.vertices.map(b => b -> false)

        kernel.biasAssignment // The truth-value assignment resulting from ac1-3
          .merge(
            TruthValueAssignment(leftBeliefs \/ rightBeliefs)
          ) // Add truth-values derived by minCut
          .filter(belief =>
            !(belief.label == "T" || belief.label == "F")
          ) // Remove the merged nodes added by ac3
      })
    })
  }

}
