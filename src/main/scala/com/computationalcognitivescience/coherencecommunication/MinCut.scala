package com.computationalcognitivescience.coherencecommunication

import mathlib.graph.{Node, NodeWeightPair, WUnDiEdge, WUnDiGraph}
import mathlib.graph.GraphImplicits._
import mathlib.set.SetTheory._

object MinCut {

  private def mergeVertices[T](u: Node[Set[T]], v: Node[Set[T]]): Node[Set[T]] = Node(
    u.label \/ v.label
  )

  private def mergeEdge[T](edge: WUnDiEdge[Node[Set[T]]]): Node[Set[T]] =
    mergeVertices(edge.left, edge.right)

  def contract2[T](
      graph: WUnDiGraph[Set[T]],
      edge: WUnDiEdge[Node[Set[T]]]
  ): WUnDiGraph[Set[T]] = {
    val mergedVertex: Node[Set[T]]          = mergeEdge(edge)
    val contractedGraph: WUnDiGraph[Set[T]] = graph - edge.left - edge.right + mergedVertex
    val neighboursOnTheLeft =
      graph.adjacencyList(edge.left) - NodeWeightPair(edge.right, edge.weight)
    val neighboursOnTheRight =
      graph.adjacencyList(edge.right) - NodeWeightPair(edge.left, edge.weight)
    val reconnectOnTheLeft  = neighboursOnTheLeft.map(n => n.node ~ mergedVertex % n.weight)
    val reconnectOnTheRight = neighboursOnTheRight.map(n => n.node ~ mergedVertex % n.weight)

    val overlappingReconnections = reconnectOnTheLeft /\ reconnectOnTheRight
    val bla =
      overlappingReconnections.map(edge => {
        val sum = reconnectOnTheLeft.filter(_ == edge).toList.map(_.weight).sum +
          reconnectOnTheRight.filter(_ == edge).toList.map(_.weight).sum
        edge.left ~ edge.right % sum
      })

    val reconnectEdges: Set[WUnDiEdge[Node[Set[T]]]] =
      (reconnectOnTheLeft \/ reconnectOnTheRight \ overlappingReconnections) \/ bla

    contractedGraph + reconnectEdges
  }

  private def toContractedNode[T](node: Node[T]): Node[Set[T]] = Node(Set(node.label))
  def contract[T](
      graph: WUnDiGraph[T],
      edge: WUnDiEdge[Node[T]]
  ): WUnDiGraph[Set[T]] = {
    contract2(
      WUnDiGraph(
        graph.vertices.map(toContractedNode),
        graph.edges.map(edge =>
          WUnDiEdge(toContractedNode(edge.left), toContractedNode(edge.right), edge.weight)
        )
      ),
      WUnDiEdge(toContractedNode(edge.left), toContractedNode(edge.right), edge.weight)
    )
  }

//  def tighten[T](graph: WUnDiGraph[T], a: Set[Node[T]]): Set[Node[T]] = {
//    // a contains all but one vertex
//    def sumWeight(v: Node[T]): Double = graph.edges.filter(_.contains(v)).toList.map(_.weight).sum
//    if(a.size < graph.size - 1) {
//      val z = argMax(graph.vertices \ a, sumWeight)
//      a + z
//    } else {
//      a
//    }
//  }

//  def minCut[T](graph: WUnDiGraph[T], a: Node[T], cut: Set[WUnDiEdge[Node[T]]]): Set[WUnDiEdge[Node[T]]] = {
//    val mostTightlyConnectedSubgraph = tighten(graph, Set(a))
//    val last = graph.vertices \ mostTightlyConnectedSubgraph
//    assert(last.size == 1, "mostTightlyConnectedSubgraph is too small: "+mostTightlyConnectedSubgraph.size)
//    val minEdge = graph.edges.filter(_.contains(last.head)).minBy(_.weight)
//
//    if(graph.size > 1) {
//      minCut(
//        contract(graph, minEdge), a, cut + minEdge
//      )
//    }
//  }

  def main(args: Array[String]): Unit = {
    val test1 = WUnDiGraph(
      Set(
        "1" ~ "2" % 5,
        "1" ~ "3" % 1,
        "1" ~ "4" % 2,
        "2" ~ "4" % 3,
        "3" ~ "4" % 7
      )
    )
    println(test1.toDOTString)
    println(contract(test1, "1" ~ "2" % 5).toDOTString)
  }

}
