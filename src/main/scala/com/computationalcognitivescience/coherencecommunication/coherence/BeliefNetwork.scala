package com.computationalcognitivescience.coherencecommunication.coherence

import mathlib.graph._
import mathlib.set.SetTheory._

import scala.annotation.tailrec
import scala.util.Random
class BeliefNetwork(
    val network: WUnDiGraph[String],
    val negativeConstraints: Set[WUnDiEdge[Node[String]]]
) extends WUnDiGraph[String](network.vertices, network.edges) {

  val positiveConstraints: Set[WUnDiEdge[Node[String]]] = network.edges \ negativeConstraints

  /** Check if in the given truth-value assignment a positive constraint is satisfied
    *
    * @param assignment
    *   The truth-value assignment over vertices
    * @param edge
    *   A positive constraint
    * @return
    *   True if the constraint is satisfied, false otherwise
    */
  protected def isSatisfiedPositiveConstraint(assignment: Map[Node[String], Boolean])(
      edge: WUnDiEdge[Node[String]]
  ): Boolean = assignment(edge.left) == assignment(edge.right)

  /** Check if in the given truth-value assignment a negative constraint is satisfied
    *
    * @param assignment
    *   The truth-value assignment over vertices
    * @param edge
    *   A negative constraint
    * @return
    *   True if the constraint is satisfied, false otherwise
    */
  protected def isSatisfiedNegativeConstraint(assignment: Map[Node[String], Boolean])(
      edge: WUnDiEdge[Node[String]]
  ): Boolean = assignment(edge.left) != assignment(edge.right)

  /** Check if in the given truth-value assignment a positive constraint is determined
    *
    * @param assignment
    *   The truth-value assignment over vertices
    * @param edge
    *   A constraint
    * @return
    *   True if both endpoints of the edge have been assigned, false otherwise
    */
  def isDeterminedConstraint(assignment: Map[Node[String], Boolean])(
      edge: WUnDiEdge[Node[String]]
  ): Boolean = assignment.keySet.contains(edge.left) || assignment.keySet.contains(edge.right)

  /** Calculate the optimal truth-value assignment of this BeliefNetwork
    *
    * Based on Blokpoel, M. & van Rooij, I. (2021). Theoretical modeling for cognitive science and
    * psychology Chapter 5
    *
    * @return
    *   A truth-value assignment over vertices that results in maximum coherence If multiple maximal
    *   truth-value assignments exists, get a random maximal one.
    */
  def coherence(): Map[Node[String], Boolean] = {

    // Output
    // Get the truth-assignment that maximizes coherence
    val allAssignments =
      vertices allMappings Set(true, false) // Generate all possible truth-value assignments
    allAssignments
      .argMax(coh)
      .random
      .get // Return the truth-value assignment that maximizes coherence value
  }

  /** Calculate the coherence-value from positive constraints with a given truth-value assignment
    *
    * @param assignment
    *   A truth-value assignment over vertices
    * @return
    *   The weighted sum over satisfied positive constraints
    */
  protected def cohPlus(assignment: Map[Node[String], Boolean]): Double = {
    val satisfiedPositiveConstraints: Set[WUnDiEdge[Node[String]]] =
      positiveConstraints.filter(isSatisfiedPositiveConstraint(assignment))

    satisfiedPositiveConstraints
      .map((edge: WUnDiEdge[Node[String]]) => edge.weight) // Get weights
      .sum                                                 // Sum weights
  }

  /** Calculate the coherence-value from negative constraints with a given truth-value assignment
    *
    * @param assignment
    *   A truth-value assignment over vertices
    * @return
    *   The weighted sum over satisfied negative constraints
    */
  protected def cohMin(assignment: Map[Node[String], Boolean]): Double = {
    val satisfiedNegativeConstraints: Set[WUnDiEdge[Node[String]]] =
      negativeConstraints.filter(isSatisfiedNegativeConstraint(assignment))

    satisfiedNegativeConstraints
      .map((edge: WUnDiEdge[Node[String]]) => edge.weight) // Get weights
      .sum                                                 // Sum weights
  }

  /** Calculate the coherence-value from all constraints with given truth-value assignment
    *
    * @param assignment
    *   A truth-value assignment over vertices
    * @return
    *   The weighted sum over all satisfied constraints
    */
  def coh(assignment: Map[Node[String], Boolean]): Double =
    cohPlus(assignment) + cohMin(assignment)

  /** {C-}-FPT algorithm for coherence as presented by van Rooij (1998)
    *
    * @return
    *   A truth-value assignment over vertices that results in maximum coherence If multiple maximal
    *   truth-value assignments exists, get a random maximal one.
    */
  def cMinusCoherence(): (Map[Node[String], Boolean], Double) = {

    // Get all vertices incident to a negative constraint
    val unassignedMinus: Set[Node[String]] = negativeConstraints.flatMap(e => Set(e.left, e.right))
    // Get all vertices incident to a positive constraint
    val unassignedPlus: Set[Node[String]] = positiveConstraints.flatMap(e => Set(e.left, e.right))

    // Apply AC1 exhaustively
    val assignmentMinusSet: Set[Map[Node[String], Boolean]] = ac1(unassignedMinus)

    // Apply AC2 where possible
    val (graphPrime, assignmentCoherence)
        : (WUnDiGraph[String], Set[(Map[Node[String], Boolean], Double)]) = ac2(assignmentMinusSet)

    //    // Select the instance that has the highest coherence value already satisfied
    //    val highestCoherence = argMax(determinedEdgeRemoved, (instance: (CoherenceNetwork, Double)) => -instance._2)

    // Apply AC3 where possible
    val maxFlowGraphs: Set[(WUnDiGraph[String], Double)] =
      // Apply AC3 to the graph, pass the coherence value as is
      assignmentCoherence.map((instance: (Map[Node[String], Boolean], Double)) =>
        (ac3(graphPrime, instance._1), instance._2)
      )

    // Get the optimal truth-value distribution and coherence value of all graphs
    def getPartition(
        instance: (WUnDiGraph[String], Double)
    ): (Map[Node[String], Boolean], Double) = {
      val (partition, coh) = maxFlow(instance._1)
      (partition, coh + instance._2)
    }
    val partitions = maxFlowGraphs.map(getPartition)

    // Get the partition with the highest coherence value
    partitions.argMax((e: (Map[Node[String], Boolean], Double)) => e._2).random.get
  }

  // Branching rule
  // Observation: for an optimal partition, any vertex that is connected by a negative constraint must either be accepted or rejected
  // Therefore, branch on unassigned vertices incident to a negative constraint such that we have 2 graphs
  // On graph where the vertex is accepted, and one where it is rejected
  // Effectively this generates all possible truth-value assignments over vertices incident to a negative edge
  def ac1(
      unassignedMinus: Set[Node[String]] // All nodes incident to a negative constraint
  ): Set[Map[Node[String], Boolean]] =
    unassignedMinus.allMappings(Set(true, false))

  // Remove determined constraints rule
  def ac2(
      assignmentSet: Set[Map[Node[String], Boolean]]
  ): (WUnDiGraph[String], Set[(Map[Node[String], Boolean], Double)]) = {

    // Because the set of *determined* constraints (positive or negative) is the same for all truth-value assignments
    // We can take a any truth-value assignment to determine the determined constraints
    val randomAssignment: Map[Node[String], Boolean] = assignmentSet.iterator.next()

    // For positive constraints we need to check if they've been determined already
    val dPosConstraints: Set[WUnDiEdge[Node[String]]] =
      positiveConstraints.filter(isDeterminedConstraint(randomAssignment))

    // Because of the application of AC1 we know all negative constraints are already satisfied
    val dNegConstraints: Set[WUnDiEdge[Node[String]]] = negativeConstraints

    // For a set of determined positive constraints, get the coherence value
    def cohDPlus(
        edgeSet: Set[WUnDiEdge[Node[String]]],
        assignment: Map[Node[String], Boolean]
    ): Double = {
      val satisfiedPositiveConstraints: Set[WUnDiEdge[Node[String]]] =
        edgeSet.filter(isSatisfiedPositiveConstraint(assignment))

      satisfiedPositiveConstraints
        .map((edge: WUnDiEdge[Node[String]]) => edge.weight) // Get weights
        .sum                                                 // Sum weights
    }

    // For a set of determined negative constraints, get the coherence value
    def cohDMin(
        edgeSet: Set[WUnDiEdge[Node[String]]],
        assignment: Map[Node[String], Boolean]
    ): Double = {
      val satisfiedNegativeConstraints: Set[WUnDiEdge[Node[String]]] =
        edgeSet.filter(isSatisfiedNegativeConstraint(assignment))

      satisfiedNegativeConstraints
        .map((edge: WUnDiEdge[Node[String]]) => edge.weight) // Get weights
        .sum                                                 // Sum weights
    }

    // For each truth-value assignment, get the coherence from already determined constraints
    val assignmentCoherence: Set[(Map[Node[String], Boolean], Double)] = {
      assignmentSet.map((assignment: Map[Node[String], Boolean]) =>
        (assignment, cohDPlus(dPosConstraints, assignment) + cohDMin(dNegConstraints, assignment))
      )
    }

    // Remove determined edges
    val edgesPrime = edges -- dPosConstraints -- dNegConstraints

    // Create new graph with new edge set
    val networkPrime: WUnDiGraph[String] = WUnDiGraph(vertices, edgesPrime)

    (networkPrime, assignmentCoherence)
  }

  // Merge Accepted and Rejected vertices rule
  def ac3(
      network: WUnDiGraph[String],
      assignment: Map[Node[String], Boolean]
  ): WUnDiGraph[String] = {
    val acceptedNode: Node[String] = Node("a")
    val rejectedNode: Node[String] = Node("r")

    def sortIncidentEdges(network: WUnDiGraph[String], assignment: Map[Node[String], Boolean]): (
        Set[WUnDiEdge[Node[String]]],
        Set[WUnDiEdge[Node[String]]],
        Set[WUnDiEdge[Node[String]]]
    ) = {
      val edgeList: List[WUnDiEdge[Node[String]]]   = network.edges.toList
      val incidentToA: Set[WUnDiEdge[Node[String]]] = Set.empty
      val incidentToR: Set[WUnDiEdge[Node[String]]] = Set.empty
      val notIncident: Set[WUnDiEdge[Node[String]]] = Set.empty

      @tailrec
      def sortIncidentEdgesPrime(
          edgeList: List[WUnDiEdge[Node[String]]],
          incidentToA: Set[WUnDiEdge[Node[String]]],
          incidentToR: Set[WUnDiEdge[Node[String]]],
          notIncident: Set[WUnDiEdge[Node[String]]]
      ): (
          Set[WUnDiEdge[Node[String]]],
          Set[WUnDiEdge[Node[String]]],
          Set[WUnDiEdge[Node[String]]]
      ) = {
        if (edgeList.isEmpty) (incidentToA, incidentToR, notIncident)
        else {
          // If the edgeList is non-empty
          val edge: WUnDiEdge[Node[String]] = edgeList.head
          if (assignment.contains(edge.left)) {
            if (assignment(edge.left))
              sortIncidentEdgesPrime(edgeList.tail, incidentToA + edge, incidentToR, notIncident)
            else sortIncidentEdgesPrime(edgeList.tail, incidentToA, incidentToR + edge, notIncident)
          } else if (assignment.contains(edge.right)) {
            if (assignment(edge.right))
              sortIncidentEdgesPrime(edgeList.tail, incidentToA + edge, incidentToR, notIncident)
            else sortIncidentEdgesPrime(edgeList.tail, incidentToA, incidentToR + edge, notIncident)
          } else sortIncidentEdgesPrime(edgeList.tail, incidentToA, incidentToR, notIncident + edge)
        }
      }

      if (edgeList.isEmpty) (incidentToA, incidentToR, notIncident)
      else {
        // If the edgeList is non-empty
        val edge: WUnDiEdge[Node[String]] = edgeList.head
        if (assignment.contains(edge.left)) {
          if (assignment(edge.left))
            sortIncidentEdgesPrime(edgeList.tail, incidentToA + edge, incidentToR, notIncident)
          else sortIncidentEdgesPrime(edgeList.tail, incidentToA, incidentToR + edge, notIncident)
        } else if (assignment.contains(edge.right)) {
          if (assignment(edge.right))
            sortIncidentEdgesPrime(edgeList.tail, incidentToA + edge, incidentToR, notIncident)
          else sortIncidentEdgesPrime(edgeList.tail, incidentToA, incidentToR + edge, notIncident)
        } else sortIncidentEdgesPrime(edgeList.tail, incidentToA, incidentToR, notIncident + edge)
      }
    }

    // Collect all constraints that are incident to an accepted node
    val (constraintsAPrime, constraintsRPrime, notIncidentConstraints): (
        Set[WUnDiEdge[Node[String]]],
        Set[WUnDiEdge[Node[String]]],
        Set[WUnDiEdge[Node[String]]]
    ) =
      sortIncidentEdges(network, assignment)

    // Replace a constraint that is incident to an accepted node with one that is connected to new node 'a'
    def replaceConstraintAPrime(
        assignment: Map[Node[String], Boolean],
        edge: WUnDiEdge[Node[String]]
    ): WUnDiEdge[Node[String]] = {
      if (assignment.contains(edge.left)) {
        WUnDiEdge(left = edge.left, right = acceptedNode, weight = edge.weight)
      } else {
        WUnDiEdge(left = edge.right, right = acceptedNode, weight = edge.weight)
      }
    }

    // Replace a constraint that is incident to a rejected node with one that is connected to new node 'r'
    def replaceConstraintRPrime(
        assignment: Map[Node[String], Boolean],
        edge: WUnDiEdge[Node[String]]
    ): WUnDiEdge[Node[String]] = {
      if (assignment.contains(edge.left)) {
        WUnDiEdge(left = edge.left, right = rejectedNode, weight = edge.weight)
      } else {
        WUnDiEdge(left = edge.right, right = rejectedNode, weight = edge.weight)
      }
    }

    // Replace a constraint from A-Prime with a new constraint in A-Star (A-star is a subset of P' x {a})
    val newConstraintsA = constraintsAPrime.map(replaceConstraintAPrime(assignment, _))
    // Replace a constraint from R-Prime with a new constraint in R-Star (R-star is a subset of P' x {r})
    val newConstraintsR = constraintsRPrime.map(replaceConstraintRPrime(assignment, _))

    // The new graph only has the old unassigned nodes plus the "a" and "r" nodes
    val newNodes = network.vertices -- assignment.keySet ++ Set(acceptedNode, rejectedNode)

    // Replace all constraints that were incident to A' and R' with their replacing constraints connecting to 'a' and 'r'.
    val newConstraints =
      notIncidentConstraints ++ newConstraintsA ++ newConstraintsR

    WUnDiGraph(newNodes, newConstraints)
  }

  // Ford-Fulkerson
  def maxFlow(network: WUnDiGraph[String]): (Map[Node[String], Boolean], Double) = {
    val sourceNode: Node[String] =
      network.vertices.filter((v: Node[String]) => v.label == "a").random.get // {a}
    val targetNode: Node[String] =
      network.vertices.filter((v: Node[String]) => v.label == "r").random.get // {r}

    // Create directed edges from an undirected edge
    def createDirectedEdges(
        edge: WUnDiEdge[Node[String]]
    ): Set[WDiEdge[Node[String]]] = {
      val firstEdge  = WDiEdge(edge.left, edge.right, edge.weight)
      val secondEdge = WDiEdge(edge.right, edge.left, edge.weight)
      Set(firstEdge, secondEdge)
    }

    // Create directed graph
    val edges: Set[WDiEdge[Node[String]]] = network.edges.flatMap(createDirectedEdges)
    val dirGraph: WDiGraph[String]        = WDiGraph(network.vertices, edges)

    // TODO: Make this recursive until no more path can be found
    def recursiveFindAugmentingPath(
        network: WDiGraph[String]
    ): WDiGraph[Node[String]] = {
      // Find path from a to r
      val augmentingPath: List[Node[String]] = bfs(network, sourceNode, targetNode)
      assert(augmentingPath.length > 1)

      // TODO: Implement some way to make this faster (A trait of network that maps a node 2-tuple to an edge?)

      // Get all edges on this path
      val pathNodes: Set[(Node[String], Node[String])] =
        Range(1, augmentingPath.length - 1)
          .map((i: Int) => (augmentingPath(i), augmentingPath(i + 1)))
          .toSet // get (leftNode, RightNode tuples)
      val pathEdges: Set[WDiEdge[Node[String]]] = {
        // Check for each edge if its endpoints are part of the nodes in the path
        network.edges.filter((e: WDiEdge[Node[String]]) => pathNodes.contains((e.left, e.right)))
      }

      // Find the minimum capacity through this path
      val minCapacity: Double = pathEdges.map(_.weight).min

      // Get reverse path
      val reversePathEdges: Set[WDiEdge[Node[String]]] = {
        // Check for each edge if its endpoints are part of the nodes in the path
        network.edges.filter((e: WDiEdge[Node[String]]) => pathNodes.contains((e.right, e.left)))
      }

      // Adjust all path capacities

      ???
    }

    val finalGraph = recursiveFindAugmentingPath(dirGraph)
    ???

  }

  // Find shortest path from startNode to targetNode O(|E| + |V|)
  def bfs(
      network: WDiGraph[String],
      startNode: Node[String],
      targetNode: Node[String]
  ): List[Node[String]] = {
    assert(network.vertices.contains(startNode))
    assert(network.vertices.contains(targetNode))

    class NodeWithParent(val self: Node[String], val parent: Node[String])

    def findNeighboursInGraph(
        network: WDiGraph[String],
        node: Node[String]
    ): Set[Node[String]] = {
      assert(network.vertices.contains(node))

      def getNeighbourIfIncident(
          edge: WDiEdge[Node[String]],
          self: Node[String]
      ): Set[Node[String]] = {
        if (edge.left == self && edge.weight > 0) Set(edge.right)
        else if (edge.right == self && edge.weight > 0) Set(edge.left)
        else Set.empty
      }

      network.edges.map(getNeighbourIfIncident(_, node))
    }

    @tailrec
    def bfsPrime(
        network: WDiGraph[String],
        startNode: Node[String],
        targetNode: Node[String],
        queue: List[NodeWithParent],
        explored: Set[Node[String]],
        parents: Set[NodeWithParent]
    ): (Set[NodeWithParent], Boolean) = {

      val neighbours: Set[Node[String]] = findNeighboursInGraph(network, startNode)
        .filterNot(queue.contains) // Remove nodes that are already in the queue
        .diff(explored)            // Remove nodes that have already been explored

      // If the target has been found
      if (neighbours.contains(targetNode)) {
        // Return the list of parents
        (parents + new NodeWithParent(startNode, targetNode), true)
      } else {
        // Keep track of the neighbours' parents
        val newParents: Set[NodeWithParent] =
          parents ++ neighbours.map(new NodeWithParent(_, startNode))
        // We've explored the current node
        val newExplored: Set[Node[String]] = explored + startNode
        // Add neighbours to the end of the queue
        val newQueue: List[NodeWithParent] =
          queue + neighbours.map(new NodeWithParent(_, startNode)).toList

        // If the queue is empty at this point, there is no more path from startNode to targetNode
        if (newQueue.isEmpty) {
          (newParents, false)
        } else {
          // Keep searching
          bfsPrime(
            network,            // network
            newQueue.head.self, // startNode
            targetNode,         // targetNode
            newQueue.tail,      // queue
            newExplored,        // explored
            newParents          // parents
          )
        }
      }
    }

    val parents = findNeighboursInGraph(network, startNode)
      .map(new NodeWithParent(_, startNode))
    val explored: Set[Node[String]] = Set(startNode)
    val queue: List[NodeWithParent] = parents.toList

    // No path can be found
    if (queue.isEmpty) List.empty
    // The target has been immediately found
    else if (queue.head.self == targetNode) List(startNode, targetNode)
    // Continue searching from next node
    else {
      val (parentsPrime, foundPath) = bfsPrime(
        network,
        queue.head.self,
        targetNode,
        queue,
        explored,
        parents
      ) // (Set[NodeWithParent], boolean)
      if (!foundPath) {
        List.empty
      } else {
        // TODO: reconstruct path from parent list
        ???
      }
    }
  }
}

// NOTES:
object RandomBeliefNetwork {

  /** Generate a belief network from a fixed number of vertices, but a (semi) random number of edges
    * based on an edge density and a (semi) random number of negative edges. Edges are generated
    * with probability equal to density and the positive-negative ratio determines how many of these
    * will be considered negative constraints.
    *
    * [KNOWN BUG]: Generated networks allow for duplicate edges (A, B) (B,A) and self edges (A, A)
    *
    * @param size
    *   The number of edges in the network
    * @param density
    *   The (expected) density of edges in the network
    * @param ratioPosNeg
    *   The target ratio of positive edges to negative edges
    * @return
    *   A randomly generated BeliefNetwork
    */
  def random(
      size: Int,
      density: Double,
      ratioPosNeg: Double
//              we: Option[Double] = None,
  ): BeliefNetwork = {
    assert(0 <= density && density <= 1)
    assert(0 <= ratioPosNeg && ratioPosNeg <= 1)
    val network: WUnDiGraph[String] = WUnDiGraph.random(size - 1, density)
    val nrNegativeConstraints: Int  = (network.edges.size * (1 - ratioPosNeg)).intValue
    val negativeConstraints: Set[WUnDiEdge[Node[String]]] =
      Random.shuffle(network.edges.toList).take(nrNegativeConstraints).toSet

    new BeliefNetwork(network, negativeConstraints)
  }

  /** Generate a belief network from a fixed number of vertices, edges and negative constraints with
    * edges having random (uniformly drawn) weights between 0 and 1.
    *
    * @param size
    *   The number of vertices in the network
    * @param nrEdges
    *   The number of edges in the network
    * @param nrNegativeEdges
    *   The number of negative constraints in the belief network where The number of negative
    *   constraints must be smaller than or equal to the total number of edges
    * @return
    *   A randomly generated BeliefNetwork
    */
  def random(
      size: Int,
      nrEdges: Int,
      nrNegativeEdges: Int
      //              we: Option[Double] = None,
  ): BeliefNetwork = {
    assert(nrEdges <= size * size)
    assert(nrNegativeEdges <= nrEdges)

    // Remove edge candidates that result int self-edges or are reversed duplicates of other edge candidates
    def noDuplicateEdges(endpoints: (Int, Int)): Boolean = {
      endpoints._1 < endpoints._2
    }

    // Generate vertex and edge candidates (candidates stay as Int for easier processing)
    val vertexCandidates: Set[Int] = (0 until size).toSet
    val edgeCandidates: Set[(Int, Int)] =
      (vertexCandidates x vertexCandidates).filter(noDuplicateEdges)

    // Generate vertices and edges
    val vertices: Set[Node[String]] = vertexCandidates.map("N" + _).map(Node(_))
    val edges: Set[WUnDiEdge[Node[String]]] = edgeCandidates
      .take(nrEdges)
      .map((e: (Int, Int)) => ("N" + e._1, "N" + e._2))
      .map((e: (String, String)) => (Node(e._1), Node(e._2)))
      .map((e: (Node[String], Node[String])) => WUnDiEdge(e._1, e._2, Random.nextDouble()))

    // Determine which edges are negative
    val negativeConstraints: Set[WUnDiEdge[Node[String]]] = edges.take(nrNegativeEdges)

    // Generate graph and belief network
    val network: WUnDiGraph[String] = WUnDiGraph(vertices, edges)
    new BeliefNetwork(network, negativeConstraints)
  }
}
