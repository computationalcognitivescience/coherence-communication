package computationalcognitivescience.coherencecommunication.coherence

import computationalcognitivescience.coherencecommunication.util.SetTheoryDev._
import Belief.Belief
import mathlib.graph.GraphImplicits._
import mathlib.graph.{Node, NodeWeightPair, WDiEdge, WDiGraph, WUnDiEdge, WUnDiGraph}
import mathlib.set.SetTheory._

import scala.annotation.tailrec

object MaxFlow {
//  def maxFlow(graph: WUnDiGraph[String]): WDiGraph[String] = {
//    ???
//  }

  /** Dijkstra's algortihm.
    * @param graph
    * @param startNode
    * @param targetNode
    * @return
    */
  def shortestPaths[T](
      graph: WDiGraph[T],
      startNode: Node[T],
      targetNode: Node[T]
  ): Set[List[WDiEdge[Node[T]]]] = {
    assert(
      startNode in graph.vertices,
      s"startNode $startNode is not in the graph ${graph.vertices}"
    )
    assert(
      targetNode in graph.vertices,
      s"targetNode targetNode is not in the graph ${graph.vertices}"
    )

    val q = graph.vertices
    val initialDistances: Map[Node[T], Double] =
      Map(startNode -> 0.0)
    val initialPrevious: Map[Node[T], Set[Node[T]]] =
      Map(startNode -> Set(startNode))

    @tailrec def search(
        q: Set[Node[T]],
        distances: Map[Node[T], Double],
        previous: Map[Node[T], Set[Node[T]]]
    ): (Map[Node[T], Double], Map[Node[T], Set[Node[T]]]) =
      if ((q /\ distances.keySet).isEmpty) (distances, previous)
      else {
        val u = argMin(q /\ distances.keySet, distances(_)).head
        if (u == targetNode) (distances, previous)
        else {
          val routes: Set[(Node[T], Double)] = argMin(
            graph.edges
              .filter(_.left == u)                           // all edges leaving u
              .map(e => (e.right, e.weight + distances(u))), // route lengths if going to v from u
            _._2                                             // all shortest routes
          )
          val shortestRoutes: Map[Node[T], Double] = routes
            .filter(route => {
              // if there is no previously discovered route to v
              // or if the new routes are shorted
              !distances.contains(route._1) || (route._2 < distances(route._1))
            })
            .toMap

          val updatedPrevious = shortestRoutes.keySet
            .map(v => {
              val currentPrevious = previous.getOrElse(v, Set.empty[Node[T]])
              v -> (currentPrevious + u)
            })
            .toMap

          println(s"q:\t\t $q")
          println(s"u:\t\t $u")
          println(s"dist:\t $distances")
          println(s"prev:\t $previous")

          if (shortestRoutes.nonEmpty) {
            println("Recursion..")
            search(
              q - u,
              distances ++ shortestRoutes,
              updatedPrevious
            )
          } else {
            println("Recursion (no new routes)..")
            search(
              q - u,
              distances,
              previous
            )
          }
        }

      }

    println(search(graph.vertices, initialDistances, initialPrevious))

    Set()
  }
}
