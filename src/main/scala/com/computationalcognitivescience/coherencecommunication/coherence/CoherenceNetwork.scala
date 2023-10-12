package com.computationalcognitivescience.coherencecommunication.coherence

import mathlib.set.SetTheory._
import mathlib.graph._

class CoherenceNetwork {

  type Vertex = String
  // type Edge = (Vertex, Vertex)

  case class Edge(left: Vertex, right: Vertex) {
    def canEqual(a: Any): Boolean = a.isInstanceOf[Edge]

    override def equals(other: Any): Boolean = {
      other match {
        case otherEdge: Edge =>
          this.left == otherEdge.left && this.right == otherEdge.right ||
            this.right == otherEdge.left && this.left == otherEdge.right

        case otherEdge: (_, _) =>
          this.left == otherEdge._1 && this.right == otherEdge._2 ||
            this.left == otherEdge._2 && this.right == otherEdge._1

        case _ => false
      }
    }
  }

  type Graph = (Set[Vertex], Set[Edge])

  // Auxiliary functions for interactions between Edge types and tuples of Vertices
  def toEdge(V: (Vertex, Vertex)): Edge = new Edge(V._1, V._2)

  def toVertexPair(e: Edge): (String, String) = (e.left, e.right)

  // Based on Thagard & Verbeurgt, 1998
  // Based on Blokpoel, M. & van Rooij, I. (2021). Theoretical modeling for cognitive science and psychology Chapter 5
  def dCoherence(vertices: Set[Vertex], edges: Set[Edge],
                 discriminatingVertices: Set[Vertex],
                 positiveConstraints: Set[Edge],
                 negativeConstraints: Set[Edge],
                 vertexWeights: Map[Vertex, Int],
                 edgeWeights: Map[Edge, Int]): Map[Vertex, Boolean]  = {
    // Input
    // All edges should contain vertices which are in this graph
    assert(edges <= (vertices x vertices).map(toEdge), "E contains an edge with vertices not in V")

    // All edges are either positive constraints or negative constraints
    assert(positiveConstraints \/ negativeConstraints == edges, "E is not equal to the union of P_C and N_C")

    // Edges may not be both positive and negative
    assert(positiveConstraints /\ negativeConstraints == Set.empty, "There is an edge that is both in P_C AND in N_C")

    // All edges in E must be present in w AND all vertices in D must be present in w
    assert(vertexWeights.keySet == discriminatingVertices, "D and vertex set of w are not equal")
    assert(edgeWeights.keySet == edges, "E and edge set of w are not equal")

    // Output
    // The number of edges that are satisfied
    def coh(assignment: Map[Vertex, Boolean]): Int = {
      // Check the discriminating vertices d in D, add w(d) if d is accepted
      val satisfiedVertices: Int = {discriminatingVertices | ((d: Vertex) => assignment(d))}.map(vertexWeights).sum

      // Map all satisfied edges to their weights in w
      val satisfiedEdges = {edges | ((edge: Edge) =>
        (
          positiveConstraints.contains(edge) // This edge is a positive constraint
            && // and
            assignment(edge.left) == assignment(edge.right) // Both vertices are in the same set (A or R)
            || // or
            negativeConstraints.contains(edge) // This edge is a negative constraint
              && // and
              assignment(edge.left) != assignment(edge.right) // Vertices are in different sets
          ))}.map(edgeWeights).sum

      satisfiedVertices + satisfiedEdges
    }

    val allAssignments = vertices allMappings Set(true, false)
    allAssignments.argMax(coh).random.get
  }

  // Based on Blokpoel, M. & van Rooij, I. (2021). Theoretical modeling for cognitive science and psychology Chapter 5
  def coherence(vertices: Set[Vertex], edges: Set[Edge],
                positiveConstraints: Set[Edge],
                negativeConstraints: Set[Edge],
                weights: Map[Edge, Int]): Map[Vertex, Boolean]  = {

    // All edges should contain vertices which are in this graph
    assert(edges <= (vertices x vertices).map(toEdge), "E contains an edge with vertices not in V")

    // All edges are either positive constraints or negative constraints
    assert(positiveConstraints \/ negativeConstraints == edges, "There is an edge in E which is not P_C or N_C OR there is an edge in P_C or N_C not in E")

    // Edges may not be both positive and negative
    assert(positiveConstraints /\ negativeConstraints == Set.empty, "There is an edge that is both positively AND negatively constrained")

    //     // The network contains no double connections
    //     assert((E /\ {(V x V) | ((edge: (Edge)) => E.contains((edge.left, edge.right)))}) == Set.empty,"There exists a double connection in E")

    // Output
    // Calculate the coherence value as the number of edges that are satisfied
    def coh(assignment: Map[Vertex, Boolean]): Int = {
      val satisfiedEdges =
        { positiveConstraints | ((e: Edge) => assignment(e.left) == assignment(e.right))} \/
          { negativeConstraints | ((e: Edge) => assignment(e.left) != assignment(e.right))}
      satisfiedEdges.map(weights).sum
    }

    val allAssignments = vertices allMappings Set(true, false) // Generate all possible truth-value assignments
    allAssignments.argMax(coh).random.get // Return the truth-value assignment that maximizes coherence value
  }

  val vertices: Set[Vertex] = Set("a", "b", "c", "d", "e", "f", "g", "h", "i")
  val discriminatingVertices: Set[Vertex] = Set("a", "b")
  val edges: Set[Edge] = Set(("a","b"), ("c","a"), ("c", "d"), ("a","d"), ("d", "e"), ("b","e"),
    ("c","f"),("d", "g"),("e","h"),("c","e"),("f","h"),("f", "g"),("g", "h"),
    ("f", "i"), ("i","g"), ("i","h")).map(toEdge)
  val positiveConstraints: Set[Edge] = Set(("c","f"),("a","d"),("b","e"),("d","g"),("e","h"),("f","i")).map(toEdge)
  val negativeConstraints: Set[Edge] = Set(("c","a"), ("a","b"),("c","d"),("d","e"),("c","e"),("f","h"),
    ("f","g"),("g","h"),("i","g"),("i","h")).map(toEdge)

  val vertexWeights: Map[Vertex, Int] = Map("a" -> 2, "b" -> 3)

  val edgeWeights: Map[Edge, Int] = (edges allMappings Set(1)).random.get + (new Edge("a","b") -> 2) + (new Edge("c","e") -> 4)


  val output1: Map[Vertex,Boolean] = coherence(vertices, edges, positiveConstraints, negativeConstraints, edgeWeights)
  val output2: Map[Vertex, Boolean] = dCoherence(vertices, edges, discriminatingVertices, positiveConstraints, negativeConstraints, vertexWeights, edgeWeights)

  println(output1)

}




