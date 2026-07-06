package computationalcognitivescience.coherencecommunication.coherence

import computationalcognitivescience.coherencecommunication.coherence.Belief.Belief
import mathlib.graph.{Node, NodeWeightPair, WUnDiEdge, WUnDiGraph}
import mathlib.set.SetTheory._

import scala.annotation.tailrec

// https://blog.thomasjungblut.com/graph/mincut/mincut/
object MinCut {

  def minCut(graph: WUnDiGraph[String], start: Belief): (Set[Belief], Set[Belief]) = {
    val mergeGraph = WUnDiGraph(
      graph.vertices.map(v => Node(Set(v.label))),
      graph.edges.map(e => WUnDiEdge(Node(Set(e.left.label)), Node(Set(e.right.label)), e.weight))
    )

//    def minCutPhase(g: WUnDiGraph[Set[String]], acc: Set[Set[String]])

    @tailrec
    def maxAdjacencySearch(
        foundSet: Seq[Node[Set[String]]]
    ): (Node[Set[String]], Node[Set[String]], Double) = {
      val remainingVertices = mergeGraph.vertices \ foundSet.toSet
      def weightTowardsFoundSet(vertex: Node[Set[String]]): Double = {
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

    val allMaxAdejencyPairs: Set[(Node[Set[String]], Node[Set[String]], Double)] =
      mergeGraph.vertices.map(v => maxAdjacencySearch(Seq(v)))

    println(allMaxAdejencyPairs)
//    println(mergeGraph)
    ???
  }
}
