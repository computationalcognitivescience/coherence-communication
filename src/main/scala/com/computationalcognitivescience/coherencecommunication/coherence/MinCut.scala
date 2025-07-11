//package com.computationalcognitivescience.coherencecommunication.coherence
//
//import com.computationalcognitivescience.coherencecommunication.coherence.Belief.Belief
//import mathlib.graph.{Node, NodeWeightPair, WUnDiEdge, WUnDiGraph}
//import mathlib.set.SetTheory._
//
//import scala.annotation.tailrec
//
//object MinCut {
//
//  def mergeNodes(
//      graph: WUnDiGraph[String],
//      a: Node[String],
//      b: Node[String],
//      label: String
//  ): WUnDiGraph[String] = {
//    val ab = Node(label)
//    val reducedGraph = graph - a - b + ab
//
//    val aEdges = graph.edges.filter(_.contains(a))
//    val aNeighbors = aEdges.map(_.getNeighborOf(a).get)
//    val bEdges = graph.edges.filter(_.contains(b))
//    val bNeighbors = bEdges.map(_.getNeighborOf(b).get)
//
//    val abNeighbors = aNeighbors /\ bNeighbors
//
//    val aOnlyEdges = aEdges.filterNot(_.contains())
//    val bOnlyEdges = bNeighbors \ abNeighbors
//
//  }
//
//  def phase(
//      graph: WUnDiGraph[String]
//  ): (Belief, Belief) = {
//    @tailrec
//    def phaseRec(
//        graph: WUnDiGraph[String],
//        phaseGraph: WUnDiGraph[String],
//        superNode: List[Belief]
//    ): (Belief, Belief) = {
//      if (superNode.toSet == graph.vertices) (superNode.head, superNode.tail.head)
//      else {
//        // here is possibility to get multiple solutions
//        val max = argMax(
//          phaseGraph.adjacencyList(Node("super")),
//          (pair: NodeWeightPair[String]) => pair.weight
//        )
//        val n = max.random.get.node
//        phaseRec(
//          graph,
//          phaseGraph,
//          n +: superNode
//        )
//      }
//    }
//
//    // here is possibility to get multiple solutions
//    phaseRec(graph, graph, List(graph.vertices.random.get))
//  }
//
//  def stoerWagner(graph: WUnDiGraph[String]): (Set[Belief], Set[Belief]) = {}
//
//}
