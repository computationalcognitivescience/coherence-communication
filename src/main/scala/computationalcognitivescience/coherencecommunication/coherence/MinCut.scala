package computationalcognitivescience.coherencecommunication.coherence

import computationalcognitivescience.coherencecommunication.coherence.Belief.Belief
import mathlib.graph.{Node, NodeWeightPair, WUnDiEdge, WUnDiGraph}
import mathlib.set.SetTheory._

import scala.annotation.tailrec

// https://blog.thomasjungblut.com/graph/mincut/mincut/
object MinCut {

  // UPDATED GRAPH FUNCTIONS
  def toDOTString[T](g: WUnDiGraph[T]): String = {
    "graph G {\n" +
      g.edges.map(edge => "\t\"" + edge.left.label + "\" -- \"" + edge.right.label + "\" [label="+edge.weight+"]").mkString("\n") +
      "\n}"
  }

  def getWeight[T](g: WUnDiGraph[T], left: Node[T], right: Node[T]): Double = {
    val optEdge = g.edges.find(e => (e contains left) && (e contains right))
    if(optEdge.isEmpty) 0.0
    else optEdge.get.weight
  }

  // END UPDATED GRAPH FUNCTIONS


  def minCutValue[T](graph: WUnDiGraph[T], cut: (Set[Node[T]], Set[Node[T]])): Double = {
    (cut._1 x cut._2).toSeq.map(pair => getWeight(graph, pair._1, pair._2)).sum
  }

  def minCut[T](graph: WUnDiGraph[T]): Set[(Set[Node[T]], Set[Node[T]])] = {
    val mergeGraph = WUnDiGraph[Set[T]](
      graph.vertices.map(v => Node(Set(v.label))),
      graph.edges.map(e => WUnDiEdge(Node(Set(e.left.label)), Node(Set(e.right.label)), e.weight))
    )

    @tailrec
    def maxAdjacencySearch(
        _mergeGraph: WUnDiGraph[Set[T]],
        foundSet: Seq[Node[Set[T]]]
    ): (Node[Set[T]], Node[Set[T]], Double) = {
      val remainingVertices = _mergeGraph.vertices \ foundSet.toSet
      def weightTowardsFoundSet(vertex: Node[Set[T]]): Double = {
        _mergeGraph
          .adjacencyList(vertex) // get all edges connecting to vertex
          .filter(
            _.node in foundSet.toSet
          ) // get the edges that connect between vertex and any other in foundSet
          .toSeq
          .map(_.weight)
          .sum
      }

      if (remainingVertices.size == 1)
        (foundSet.head, remainingVertices.head, weightTowardsFoundSet(foundSet.head))
      else {
        val next = argMax(remainingVertices, weightTowardsFoundSet).head
        maxAdjacencySearch(_mergeGraph, next +: foundSet)
      }
    }

    def minCutRec(_mergeGraph: WUnDiGraph[Set[T]]): Set[(Set[Node[T]], Set[Node[T]])] = {
      println("---")
      println(_mergeGraph.size)
      println(toDOTString(_mergeGraph))


      if (_mergeGraph.size == 2)
        Set((_mergeGraph.vertices.head.label.map(Node(_)), _mergeGraph.vertices.last.label.map(Node(_))))
      else {
        val allMaxAdejencyPairs: Set[(Node[Set[T]], Node[Set[T]], Double)] =
          _mergeGraph.vertices.map(v => maxAdjacencySearch(_mergeGraph, Seq(v)))

        // branch into all possible next maximum min cut searches
        val nextMergeGraphs = allMaxAdejencyPairs.map(stw => {
          val (s, t, _) = stw
          // merge s and t
          println(s"s $s and t $t")
          val st = Node(s.label \/ t.label)

          val toBeMergedEdges = _mergeGraph.edges
            .filter(e =>
              ((e contains s) || (e contains t)) && !((e contains s) && (e contains t))
            ) // edges that connect to s or t, but not both
          val mergedEdges = toBeMergedEdges
            .groupBy(e => {
              if (e.left == s || e.left == t) e.right
              else e.left
            })
            .map(nodeEdgeSet => {
              val linkPoint = nodeEdgeSet._1
              val sumWeight = nodeEdgeSet._2.toSeq.map(_.weight).sum
              WUnDiEdge(st, linkPoint, sumWeight)
            })
            .toSet
          val nonMergedEdges = _mergeGraph.edges
            .filter(e => !((e contains s) || (e contains t)))

          println(mergedEdges \/ nonMergedEdges == _mergeGraph.edges)


          WUnDiGraph(mergedEdges \/ nonMergedEdges)
        })
        nextMergeGraphs.flatMap(minCutRec)
      }
    }

    minCutRec(mergeGraph)
  }
}
