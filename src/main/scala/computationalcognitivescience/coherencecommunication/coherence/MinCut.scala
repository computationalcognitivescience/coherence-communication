package computationalcognitivescience.coherencecommunication.coherence

import computationalcognitivescience.coherencecommunication.coherence.Belief.Belief
import mathlib.graph.{Node, NodeWeightPair, WUnDiEdge, WUnDiGraph}
import mathlib.set.SetTheory._

import scala.annotation.tailrec

// https://blog.thomasjungblut.com/graph/mincut/mincut/
object MinCut {

  def minCut[T](graph: WUnDiGraph[T]): Set[(Set[T], Set[T])] = {
    val mergeGraph = WUnDiGraph[Set[T]](
      graph.vertices.map(v => Node(Set(v.label))),
      graph.edges.map(e => WUnDiEdge(Node(Set(e.left.label)), Node(Set(e.right.label)), e.weight))
    )

    @tailrec
    def maxAdjacencySearch(
        foundSet: Seq[Node[Set[T]]]
    ): (Node[Set[T]], Node[Set[T]], Double) = {
      val remainingVertices = mergeGraph.vertices \ foundSet.toSet
      def weightTowardsFoundSet(vertex: Node[Set[T]]): Double = {
        mergeGraph
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
        maxAdjacencySearch(next +: foundSet)
      }
    }

    def minCutRec(mergeGraph: WUnDiGraph[Set[T]]): Set[(Set[T], Set[T])] = {
      if(mergeGraph.size == 2) Set((mergeGraph.vertices.head.label, mergeGraph.vertices.last.label))
      else {
        val allMaxAdejencyPairs: Set[(Node[Set[T]], Node[Set[T]], Double)] =
          mergeGraph.vertices.map(v => maxAdjacencySearch(Seq(v)))

        // branch into all possible next maximum min cut searches
        val nextMergeGraphs = allMaxAdejencyPairs.map(stw => {
          val (s, t, w) = stw
          // merge s and t
          val st = Node(s.label \/ t.label)

          val mergedEdges = mergeGraph.edges
            .filter(e => ((e contains s) || (e contains t)) && !((e contains s) && (e contains t))) // edges that connect to s or t, but not both
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
          val nonMergedEdges =  mergeGraph.edges
            .filter(e => !((e contains s) || (e contains t)))

                  println("---")
                  println(stw)
                  mergedEdges.foreach(println)
                  println("")
                  nonMergedEdges.foreach(println)

          WUnDiGraph(mergedEdges \/ nonMergedEdges)
        })
        nextMergeGraphs.flatMap(minCutRec)
      }
    }

    minCutRec(mergeGraph)
  }
}
