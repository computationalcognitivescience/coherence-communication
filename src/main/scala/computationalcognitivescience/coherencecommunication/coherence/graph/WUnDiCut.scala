package computationalcognitivescience.coherencecommunication.coherence.graph

import mathlib.graph.{Node, NodeWeightPair, WUnDiEdge, WUnDiGraph}
import mathlib.set.SetTheory._

case class WUnDiCut[T](left: WUnDiGraph[T], right: WUnDiGraph[T], cut: Set[WUnDiEdge[Node[T]]])
    extends Cut[T, WUnDiEdge[Node[T]], WUnDiGraph[T]]

case object WUnDiCut {
  def apply[T](
      phase: Phase[T],
      graph: WUnDiGraph[T]
  ): WUnDiCut[T] = {
    val leftVertices: Set[Node[T]]  = phase.s.label
    val rightVertices: Set[Node[T]] = phase.t.label
    val left: WUnDiGraph[T]         = graph - leftVertices
    val right: WUnDiGraph[T]        = graph - rightVertices
    val cut: Set[WUnDiEdge[Node[T]]] = graph.edges.filter(e =>
      (e.left in leftVertices)
        || (e.right in leftVertices)
        || (e.left in rightVertices)
        || (e.right in rightVertices)
    )
    WUnDiCut(left, right, cut)
  }

  implicit class ImplWUnDiGraph[T](graph: WUnDiGraph[T]) {

    private lazy val initializeMergeGraph: WUnDiGraph[Set[Node[T]]] = {
      val mergeVertices: Set[Node[Set[Node[T]]]] = graph.vertices.map(v => Node(Set(v)))
      val mergeEdges: Set[WUnDiEdge[Node[Set[Node[T]]]]] =
        graph.edges.map(e => WUnDiEdge(Node(Set(e.left)), Node(Set(e.right)), e.weight))
      WUnDiGraph(mergeVertices, mergeEdges)
    }

    private def minCutPhase(
        mergeGraph: WUnDiGraph[Set[Node[T]]],
        a: Node[Set[Node[T]]],
        foundSet: List[Node[Set[Node[T]]]] = List.empty
    ): Set[Phase[T]] = {
      val adjList = mergeGraph.adjacencyList

      val _foundSet = foundSet :+ a
      def vertexTightness(v: Node[Set[Node[T]]]): Double =
        sum(
          adjList(v).filter(nwp => _foundSet.contains(nwp.node)),
          (nwp: NodeWeightPair[Set[Node[T]]]) => nwp.weight
        )

      val tightestVertices =
        argMax(
          mergeGraph.vertices \ _foundSet.toSet,
          vertexTightness
        )
      //      println(s"\tfs: ${_foundSet}")
      //      println(s"\ttv: $tightestVertices")
      if (tightestVertices.isEmpty) {
        val t = _foundSet.init.last
        val s = _foundSet.last
        val cutWeight = sum(
          adjList(t).filter(nwp => _foundSet.dropRight(2).contains(nwp.node)),
          (nwp: NodeWeightPair[Set[Node[T]]]) => nwp.weight
        )
        //        println(_foundSet)
        Set(Phase(s, t, cutWeight))
      } else {
        tightestVertices.flatMap(t => minCutPhase(mergeGraph, t, _foundSet))
      }
    }

    def mergeCut(
        mergeGraph: WUnDiGraph[Set[Node[T]]],
        cut: Phase[T]
    ): WUnDiGraph[Set[Node[T]]] = {
      val st = Node(cut.s.label \/ cut.t.label)
      val toBeMergedEdges = mergeGraph.edges
        .filter((e: WUnDiEdge[Node[Set[Node[T]]]]) =>
          ((e contains cut.s) || (e contains cut.t)) && !((e contains cut.s) && (e contains cut.t))
        ) // edges that connect to s or t, but not both
      val mergedEdges = toBeMergedEdges
        .groupBy(e => {
          if (e.left == cut.s || e.left == cut.t) e.right
          else e.left
        })
        .map(nodeEdgeSet => {
          val linkPoint = nodeEdgeSet._1
          val sumWeight = nodeEdgeSet._2.toSeq.map(_.weight).sum
          WUnDiEdge(st, linkPoint, sumWeight)
        })
        .toSet
      val nonMergedEdges = mergeGraph.edges
        .filter(e => !((e contains cut.s) || (e contains cut.t)))

      WUnDiGraph(mergedEdges \/ nonMergedEdges)
    }

    def minCuts(
        mergeGraph: WUnDiGraph[Set[Node[T]]] = initializeMergeGraph,
        minCut: Option[Phase[T]] = None
    ): Set[WUnDiCut[T]] = {
      if (mergeGraph.isEmpty) Set(WUnDiCut(minCut.get, graph))
      else {
        val cutOfPhases: Set[Phase[T]] =
          mergeGraph.vertices
            .flatMap(minCutPhase(mergeGraph, _))
        val minCutOfPhaseWeight: Double = cutOfPhases.map(_.w).min
        val minCutOfPhases: Set[Phase[T]] =
          cutOfPhases.filter(_.w == minCutOfPhaseWeight)

        minCutOfPhases.flatMap((cut: Phase[T]) => {
          if (minCut.isEmpty || minCut.get.w > cut.w) {
            minCuts(mergeCut(mergeGraph, cut), Some(cut))
          } else if (minCut.get.w == cut.w) {
            minCuts(mergeCut(mergeGraph, cut), Some(cut)) \/ minCuts(
              mergeCut(mergeGraph, cut),
              Some(minCut.get)
            )
          } else {
            minCuts(mergeCut(mergeGraph, cut), Some(minCut.get))
          }
        })
      }
    }
  }
}
