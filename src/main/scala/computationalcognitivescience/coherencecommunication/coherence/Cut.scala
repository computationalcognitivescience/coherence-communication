package computationalcognitivescience.coherencecommunication.coherence

import Cut.ImplWUnDiGraph
import mathlib.graph.GraphImplicits._
import mathlib.graph.properties.Edge
import mathlib.graph._
import mathlib.set.SetTheory._

case class Cut[T, E <: Edge[Node[T]], G <: Graph[T, E]](left: G, right: G, cut: Set[E]) {}

case object Cut {

  def main(args: Array[String]): Unit = {
    val test1 = WUnDiGraph(
      Set(
        N("1") ~ N("2") % 2,
        N("1") ~ N("5") % 3,
        N("2") ~ N("3") % 3,
        N("2") ~ N("5") % 2,
        N("2") ~ N("6") % 2,
        N("3") ~ N("4") % 4,
        N("3") ~ N("7") % 2,
        N("4") ~ N("7") % 2,
        N("4") ~ N("8") % 2,
        N("5") ~ N("6") % 3,
        N("6") ~ N("7") % 1,
        N("7") ~ N("8") % 3
      )
    )

//    test1.minCuts().foreach(println)

    println(
      toDOTStringCorrect(
        test1.mergeCut(
          test1.initializeMergeGraph,
          Phase(
            Node(Set("1")),
            Node(Set("5")),
            5
          )
        )
      )
    )

    def toDOTStringCorrect[T](g: WUnDiGraph[T]): String = {
      val vertexIds = g.vertices.toSeq.zipWithIndex

      "graph G {\n" +
        vertexIds.map(nid => s"\tN${nid._2} [label=\"${nid._1}\"]").mkString("","\n","\n") +
        g.edges
          .map(edge => {
            "\tN" + vertexIds.find(_._1 == edge.left).get._2 +
              " -- N" + vertexIds.find(_._1 == edge.right).get._2 +
              " [label=" + edge.weight + "]"
          })
          .mkString("\n") +
        "\n}"
    }
  }

  case class Phase[T](s: Node[T], t: Node[T], w: Double) {
    override def canEqual(obj: Any): Boolean =
      obj.isInstanceOf[Phase[_]]

    override def equals(obj: Any): Boolean = {

      obj match {
        case obj: Phase[_] => obj.s == s && obj.t == t
        case _             => false
      }
    }

    override def hashCode: Int = {
      val prime  = 31
      var result = 1
      result = prime * result + s.hashCode();
      result = prime * result + t.hashCode();
      result
    }
  }

  implicit class ImplWUnDiGraph[T](graph: WUnDiGraph[T]) {
    private def minCutPhase(
        mergeGraph: WUnDiGraph[Set[T]],
        a: Node[Set[T]],
        foundSet: List[Node[Set[T]]] = List.empty
    ): Set[Phase[Set[T]]] = {
      val adjList = mergeGraph.adjacencyList

      val _foundSet = foundSet :+ a
      def vertexTightness(v: Node[Set[T]]): Double =
        sum(
          adjList(v).filter(nwp => _foundSet.contains(nwp.node)),
          (nwp: NodeWeightPair[Set[T]]) => nwp.weight
        )

      val tightestVertices = argMax(mergeGraph.vertices \ _foundSet.toSet, vertexTightness)
//      println(s"\tfs: ${_foundSet}")
//      println(s"\ttv: $tightestVertices")
      if (tightestVertices.isEmpty) {
        val t = _foundSet.init.last
        val s = _foundSet.last
        val cutWeight = sum(
          adjList(t).filter(nwp => _foundSet.dropRight(2).contains(nwp.node)),
          (nwp: NodeWeightPair[Set[T]]) => nwp.weight
        )
//        println(_foundSet)
        Set(Phase(s, t, cutWeight))
      } else {
        tightestVertices.flatMap(t => minCutPhase(mergeGraph, t, _foundSet))
      }
    }

    def mergeCut(graph: WUnDiGraph[Set[T]], cut: Phase[Set[T]]): WUnDiGraph[Set[T]] = {
      val st = Node(cut.s.label \/ cut.t.label)
      val toBeMergedEdges = graph.edges
        .filter(e =>
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
      val nonMergedEdges = graph.edges
        .filter(e => !((e contains cut.s) || (e contains cut.t)))

      WUnDiGraph(mergedEdges \/ nonMergedEdges)
    }

    lazy val initializeMergeGraph: WUnDiGraph[Set[T]] = {
      val mergeVertices = graph.vertices.map(v => Node(Set(v.value)))
      val mergeEdges =
        graph.edges.map(e => WUnDiEdge(Node(Set(e.left.value)), Node(Set(e.right.value)), e.weight))
      WUnDiGraph(mergeVertices, mergeEdges)
    }

    def minCuts(
        mergeGraph: WUnDiGraph[Set[T]] = initializeMergeGraph,
        minCut: Option[Phase[Set[T]]] = None
    ): Set[Phase[Set[T]]] = {
      if (mergeGraph.isEmpty) Set(minCut.get)
      else {
        val cutOfPhases: Set[Phase[Set[T]]] =
          mergeGraph.vertices
            .flatMap(minCutPhase(mergeGraph, _))
        val minCutOfPhaseWeight: Double = cutOfPhases.map(_.w).min
        val minCutOfPhases: Set[Phase[Set[T]]] =
          cutOfPhases.filter(_.w == minCutOfPhaseWeight)

        minCutOfPhases.flatMap((cut: Phase[Set[T]]) => {
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
