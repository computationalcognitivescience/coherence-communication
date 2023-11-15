package com.computationalcognitivescience.coherencecommunication.coherence

import mathlib.graph._
import mathlib.set.SetTheory._

import scala.util.Random
class BeliefNetwork(
    val network: WUnDiGraph[String],
    val negativeConstraints: Set[WUnDiEdge[Node[String]]]
) extends WUnDiGraph[String](network.vertices, network.edges) {

  val positiveConstraints: Set[WUnDiEdge[Node[String]]] = network.edges \ negativeConstraints

  /** Check if in the given truth-value assignment a positive constraint is satisfied
   *
   * @param assignment
   * The truth-value assignment over vertices
   * @param edge
   * A positive constraint
   * @return
   * True if the constraint is satisfied, false otherwise
   */
  protected def isSatisfiedPositiveConstraint(assignment: Map[Node[String], Boolean])(
    edge: WUnDiEdge[Node[String]]
  ): Boolean = assignment(edge.left) == assignment(edge.right)

  /** Check if in the given truth-value assignment a negative constraint is satisfied
   *
   * @param assignment
   * The truth-value assignment over vertices
   * @param edge
   * A negative constraint
   * @return
   * True if the constraint is satisfied, false otherwise
   */
  protected def isSatisfiedNegativeConstraint(assignment: Map[Node[String], Boolean])(
    edge: WUnDiEdge[Node[String]]
  ): Boolean = assignment(edge.left) != assignment(edge.right)

  // Based on Blokpoel, M. & van Rooij, I. (2021). Theoretical modeling for cognitive science and psychology Chapter 5
  // Standard coherence

  /** Calculate the optimal truth-value assignment of this BeliefNetwork
   *
   * @return
   * A truth-value assignment over vertices that results in maximum coherence
   * If multiple maximal truth-value assignments exists, get a random maximal one.
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
   * A truth-value assignment over vertices
   * @return
   * The weighted sum over satisfied positive constraints
   */
  protected def cohPlus(assignment: Map[Node[String], Boolean]): Double = {
    val satisfiedPositiveConstraints: Set[WUnDiEdge[Node[String]]] =
      positiveConstraints.filter(isSatisfiedPositiveConstraint(assignment))

    satisfiedPositiveConstraints
      .map((edge: WUnDiEdge[Node[String]]) => edge.weight) // Get weights
      .sum // Sum weights
  }

  /** Calculate the coherence-value from negative constraints with a given truth-value assignment
   *
   * @param assignment
   * A truth-value assignment over vertices
   * @return
   * The weighted sum over satisfied negative constraints
   */
  protected def cohMin(assignment: Map[Node[String], Boolean]): Double = {
    val satisfiedNegativeConstraints: Set[WUnDiEdge[Node[String]]] =
      negativeConstraints.filter(isSatisfiedNegativeConstraint(assignment))

    satisfiedNegativeConstraints
      .map((edge: WUnDiEdge[Node[String]]) => edge.weight) // Get weights
      .sum // Sum weights
  }

  /** Calculate the coherence-value from all constraints with given truth-value assignment
   *
   * @param assignment
   * A truth-value assignment over vertices
   * @return
   * The weighted sum over all satisfied constraints
   */
  def coh(assignment: Map[Node[String], Boolean]): Double =
    cohPlus(assignment) + cohMin(assignment)
}
//  /** {C-}-FPT algorithm for coherence as presented by van Rooij (1998)
//   *
//   * @return
//   *  A truth-value assignment over vertices that results in maximum coherence
//   *  If multiple maximal truth-value assignments exists, get a random maximal one.
//   */
//  def cMinusCoherence(): (Map[Node[WeightedBelief], Boolean], Double) = {
//
//    // Get all vertices incident to a negative constraint
//    val unassignedMinus: Set[Node[String]] = negativeConstraints.flatMap(e => Set(e.left, e.right))
//    // Get all vertices incident to a positive constraint
//    val unassignedPlus: Set[Node[String]]  = positiveConstraints.flatMap(e => Set(e.left, e.right))
//
//    // Apply AC1 exhaustively
//    val assignmentMinusSet: Set[Map[Node[String], Boolean]] = ac1(unassignedMinus)
//
//    // Apply AC2 where possible
//    val determinedEdgeRemoved: Set[(BeliefNetwork, Double)] =
//      assignmentMinusSet.map(ac2(this, _, unassignedPlus))
//
//    //    // Select the instance that has the highest coherence value already satisfied
//    //    val highestCoherence = argMax(determinedEdgeRemoved, (instance: (CoherenceNetwork, Double)) => -instance._2)
//
//    // Apply AC3 where possible
//    val maxFlowGraphs: Set[(BeliefNetwork, Double)] =
//    // Apply AC3 to the graph, pass the threshold value as is
//      determinedEdgeRemoved.map(instance => (ac3(instance._1), instance._2))
//
//    // Get the optimal truth-value distribution and coherence value of all graphs
//    def getPartition(
//                      instance: (BeliefNetwork, Double)
//                    ): (Map[Node[String], Boolean], Double) = {
//      val (partition, coh) = maxFlow(instance._1)
//      (partition, coh + instance._2)
//    }
//    val partitions = maxFlowGraphs.map(getPartition)
//
//    // Get the partition with the highest coherence value
//    partitions.argMax((e: (Map[Node[String], Boolean], Double)) => e._2).random.get
//  }
//
//  // Branching rule
//  // Observation: for an optimal partition, any vertex that is connected by a negative constraint must either be accepted or rejected
//  // Therefore, branch on unassigned vertices incident to a negative constraint such that we have 2 graphs
//  // On graph where the vertex is accepted, and one where it is rejected
//  // Effectively this generates all possible truth-value assignments over vertices incident to a negative edge
//  def ac1(
//           unassignedMinus: Set[Node[String]],      // All nodes incident to a negative constraint
//         ): Set[Map[Node[String], Boolean]] =
//    unassignedMinus.allMappings(Set(true, false))
//
//
//
//  // Remove determined constraints rule
//  def ac2(
//           network: BeliefNetwork,
//           assignmentMinus: Map[Node[String], Boolean],
//           unassignedPlus: Set[Node[String]]
//         ): (BeliefNetwork, Double) = {
//
//    // Collect all positive constraints that are already determined (i.e. find those where none of the endpoints are unassigned)
//    def isPositiveConstraintDetermined(edge: WUnDiEdge[Node[String]]): Boolean = {
//      assignmentMinus.keySet.contains(edge.left) || assignmentMinus.keySet.contains(edge.right)
//    }
//
//    // For positive constraints we need to check if they've been determined already
//    val dPosConstraints: Set[WUnDiEdge[Node[String]]] =
//      network.positiveConstraints.filter(isPositiveConstraintDetermined)
//
//    // Because of the application of AC1 we know all negative constraints are already satisfied
//    val dNegConstraints: Set[WUnDiEdge[Node[String]]] = network.negativeConstraints
//
//    // If both edges are in the accepted or the rejected set, return true
//    // ASSUMPTION: edge is determined (i.e. neither end is unassigned)
//    def satisfiedPositiveConstraint(edge: WUnDiEdge[Node[String]]): Boolean =
//      assignmentMinus(edge.left) == assignmentMinus(edge.right)
//
//
//    def satisfiedNegativeConstraint(edge: WUnDiEdge[Node[String]]): Boolean = {
//      assignmentMinus(edge.left) != assignmentMinus(edge.right)
//    }
//
//    // Get coherence from satisfied positive constraints
//    val dPosCoherence = dPosConstraints
//      .filter(satisfiedPositiveConstraint)
//      .map(e => e.weight)
//      .sum
//
//    // Get coherence from satisfied negative constraints
//    val dNegCoherence = dNegConstraints
//      .filter(satisfiedNegativeConstraint)
//      .map(e => e.weight)
//      .sum
//
//    // Determine total coherence from already satisfied edges
//    val coh: Double = dPosCoherence + dNegCoherence
//
//    // Remove determined edges
//    val edgesPrime = network.edges -- dPosConstraints -- dNegConstraints
//
//    // Create new graph with new edge set
//    val networkPrime: WUnDiGraph[String] = WUnDiGraph(network.vertices, edgesPrime)
//
//
//    (new BeliefNetwork(networkPrime, Set.empty), coh)
//  }
//
//  // Merge Accepted and Rejected vertices rule
//  def ac3(network: CoherenceNetwork): CoherenceNetwork = {
//    val acceptedNode: Node[WeightedBelief] = Node(WeightedBelief("a", 0))
//    val rejectedNode: Node[WeightedBelief] = Node(WeightedBelief("r", 0))
//
//    // Check if a constraint is incident to an accepted node
//    def incidentToA(edge: WUnDiEdge[Node[WeightedBelief]]): Boolean = {
//      network.accepted.contains(edge.left) || network.accepted.contains(edge.right)
//    }
//
//    // Check if a constraint is incident to a rejected node
//    def incidentToR(edge: WUnDiEdge[Node[WeightedBelief]]): Boolean = {
//      network.rejected.contains(edge.left) || network.rejected.contains(edge.right)
//    }
//
//    // Collect all constraints that are incident to an accepted node
//    val constraintsAPrime: Set[WUnDiEdge[Node[WeightedBelief]]] =
//      network.positiveConstraints.filter(incidentToA)
//
//    // Collect all constraints that are incident to an rejected node
//    val constraintsRPrime: Set[WUnDiEdge[Node[WeightedBelief]]] =
//      network.positiveConstraints.filter(incidentToR)
//
//    // Replace a constraint that is incident to an accepted node with one that is connected to new node 'a'
//    def replaceConstraintAPrime(
//                                 edge: WUnDiEdge[Node[WeightedBelief]]
//                               ): WUnDiEdge[Node[WeightedBelief]] = {
//      if (network.unassigned.contains(edge.left)) {
//        WUnDiEdge(left = edge.left, right = acceptedNode, weight = edge.weight)
//      } else {
//        WUnDiEdge(left = edge.right, right = acceptedNode, weight = edge.weight)
//      }
//    }
//
//    // Replace a constraint that is incident to a rejected node with one that is connected to new node 'r'
//    def replaceConstraintRPrime(
//                                 edge: WUnDiEdge[Node[WeightedBelief]]
//                               ): WUnDiEdge[Node[WeightedBelief]] = {
//      if (network.unassigned.contains(edge.left)) {
//        WUnDiEdge(left = edge.left, right = rejectedNode, weight = edge.weight)
//      } else {
//        WUnDiEdge(left = edge.right, right = rejectedNode, weight = edge.weight)
//      }
//    }
//
//    // Replace a constraint from A-Prime with a new constraint in A-Star (A-star is a subset of P' x {a})
//    val newConstraintsAStar = constraintsAPrime.map(replaceConstraintAPrime)
//    // Replace a constraint from R-Prime with a new constraint in R-Star (R-star is a subset of P' x {r})
//    val newConstraintsRStar = constraintsRPrime.map(replaceConstraintRPrime)
//
//    // The new graph only has the old unassigned nodes plus the "a" and "r" nodes
//    val newNodes = network.unassigned ++ Set(acceptedNode, rejectedNode)
//
//    // Replace all constraints that were incident to A' and R' with their replacing constraints connecting to 'a' and 'r'.
//    val newConstraints =
//      network.positiveConstraints -- (constraintsAPrime ++ constraintsRPrime) ++ newConstraintsAStar ++ newConstraintsRStar
//
//    new CoherenceNetwork(
//      newNodes,          // Vertices
//      newConstraints,    // PositiveConstraints
//      Set.empty,         // NegativeConstraints
//      Set(acceptedNode), // AcceptedVertices
//      Set(rejectedNode), // RejectedVertices
//      network.unassigned // UnassignedVertices
//    )
//  }
//
//  // Ford-Fulkerson
//  def maxFlow(network: CoherenceNetwork): (Map[Node[WeightedBelief], Boolean], Double) = {
//    assert(network.accepted.size == 1)
//    assert(network.rejected.size == 1)
//    val sourceNode: Node[WeightedBelief] = network.accepted.random.get // {a}
//    val targetNode: Node[WeightedBelief] = network.rejected.random.get // {r}
//
//    // Create directed edges from an undirected edge
//    def createDirectedEdges(
//                             edge: WUnDiEdge[Node[WeightedBelief]]
//                           ): Set[WDiEdge[Node[WeightedBelief]]] = {
//      val firstEdge  = WDiEdge(edge.left, edge.right, edge.weight)
//      val secondEdge = WDiEdge(edge.right, edge.left, edge.weight)
//      Set(firstEdge, secondEdge)
//    }
//
//    // Create directed graph
//    val edges: Set[WDiEdge[Node[WeightedBelief]]] = network.edges.flatMap(createDirectedEdges)
//    val dirGraph: WDiGraph[WeightedBelief]        = WDiGraph(network.vertices, edges)
//
//    // TODO: Make this recursive until no more path can be found
//    def recursiveFindAugmentingPath(
//                                     network: WDiGraph[WeightedBelief]
//                                   ): WDiGraph[Node[WeightedBelief]] = {
//      // Find path from a to r
//      val augmentingPath: List[Node[WeightedBelief]] = bfs(network, sourceNode, targetNode)
//      assert(augmentingPath.length > 1)
//
//      // TODO: Implement some way to make this faster (A trait of network that maps a node 2-tuple to an edge?)
//
//      // Get all edges on this path
//      val pathNodes: Set[(Node[WeightedBelief], Node[WeightedBelief])] =
//        Range(1, augmentingPath.length - 1)
//          .map((i: Int) => (augmentingPath(i), augmentingPath(i + 1)))
//          .toSet // get (leftNode, RightNode tuples)
//      val pathEdges: Set[WDiEdge[Node[WeightedBelief]]] = {
//        // Check for each edge if its endpoints are part of the nodes in the path
//        network.edges.filter((e: WDiEdge[Node[WeightedBelief]]) =>
//          pathNodes.contains((e.left, e.right))
//        )
//      }
//
//      // Find the minimum capacity through this path
//      val minCapacity: Double = pathEdges.map(_.weight).min
//
//      // Get reverse path
//      val reversePathEdges: Set[WDiEdge[Node[WeightedBelief]]] = {
//        // Check for each edge if its endpoints are part of the nodes in the path
//        network.edges.filter((e: WDiEdge[Node[WeightedBelief]]) =>
//          pathNodes.contains((e.right, e.left))
//        )
//      }
//
//      // Adjust all path capacities
//
//      ???
//    }
//
//    val finalGraph = recursiveFindAugmentingPath(dirGraph)
//    ???
//
//  }
//
//  // Find shortest path from startNode to targetNode O(|E| + |V|)
//  def bfs(
//           network: WDiGraph[WeightedBelief],
//           startNode: Node[WeightedBelief],
//           targetNode: Node[WeightedBelief]
//         ): List[Node[WeightedBelief]] = {
//    assert(network.vertices.contains(startNode))
//    assert(network.vertices.contains(targetNode))
//
//    class NodeWithParent(val self: Node[WeightedBelief], val parent: Node[WeightedBelief])
//
//    def findNeighboursInGraph(
//                               network: WDiGraph[WeightedBelief],
//                               node: Node[WeightedBelief]
//                             ): Set[Node[WeightedBelief]] = {
//      assert(network.vertices.contains(node))
//
//      def getNeighbourIfIncident(
//                                  edge: WDiEdge[Node[WeightedBelief]],
//                                  self: Node[WeightedBelief]
//                                ): Set[Node[WeightedBelief]] = {
//        if (edge.left == self && edge.weight > 0) Set(edge.right)
//        else if (edge.right == self && edge.weight > 0) Set(edge.left)
//        else Set.empty
//      }
//
//      network.edges.map(getNeighbourIfIncident(_, node))
//    }
//
//    @tailrec
//    def bfsPrime(
//                  network: WDiGraph[WeightedBelief],
//                  startNode: Node[WeightedBelief],
//                  targetNode: Node[WeightedBelief],
//                  queue: List[NodeWithParent],
//                  explored: Set[Node[WeightedBelief]],
//                  parents: Set[NodeWithParent]
//                ): (Set[NodeWithParent], Boolean) = {
//
//      val neighbours: Set[Node[WeightedBelief]] = findNeighboursInGraph(network, startNode)
//        .filterNot(queue.contains) // Remove nodes that are already in the queue
//        .diff(explored)            // Remove nodes that have already been explored
//
//      // If the target has been found
//      if (neighbours.contains(targetNode)) {
//        // Return the list of parents
//        (parents + new NodeWithParent(startNode, targetNode), true)
//      } else {
//        // Keep track of the neighbours' parents
//        val newParents: Set[NodeWithParent] =
//          parents ++ neighbours.map(new NodeWithParent(_, startNode))
//        // We've explored the current node
//        val newExplored: Set[Node[WeightedBelief]] = explored + startNode
//        // Add neighbours to the end of the queue
//        val newQueue: List[NodeWithParent] = queue + neighbours.map(new NodeWithParent(_, startNode)).toList
//
//        // If the queue is empty at this point, there is no more path from startNode to targetNode
//        if (newQueue.isEmpty) {
//          (newParents, false)
//        } else {
//          // Keep searching
//          bfsPrime(
//            network,            // network
//            newQueue.head.self, // startNode
//            targetNode,         // targetNode
//            newQueue.tail,      // queue
//            newExplored,        // explored
//            newParents          // parents
//          )
//        }
//      }
//    }
//
//    val parents = findNeighboursInGraph(network, startNode)
//      .map(new NodeWithParent(_, startNode))
//    val explored: Set[Node[WeightedBelief]] = Set(startNode)
//    val queue: List[NodeWithParent]         = parents.toList
//
//    // No path can be found
//    if (queue.isEmpty) List.empty
//    // The target has been immediately found
//    else if (queue.head.self == targetNode) List(startNode, targetNode)
//    // Continue searching from next node
//    else {
//      val (parentsPrime, foundPath) = bfsPrime(
//        network,
//        queue.head.self,
//        targetNode,
//        queue,
//        explored,
//        parents
//      ) // (Set[NodeWithParent], boolean)
//      if (!foundPath) {
//        List.empty
//      } else {
//        // TODO: reconstruct path from parent list
//        ???
//      }
//    }
//  }
//}

// NOTES:
object RandomBeliefNetwork {
  /** Generate a belief network from a fixed number of vertices, but a (semi) random number of edges based
   * on an edge density and a (semi) random number of negative edges. Edges are generated with probability
   * equal to density and the positive-negative ratio determines how many of these will be considered
   * negative constraints.
   *
   * [KNOWN BUG]: Generated networks allow for duplicate edges (A, B) (B,A) and self edges (A, A)
   *
   * @param size
   *  The number of edges in the network
   * @param density
   *  The (expected) density of edges in the network
   * @param ratioPosNeg
   *  The target ratio of positive edges to negative edges
   * @return
   *  A randomly generated BeliefNetwork
   */
  def random(
              size: Int,
              density: Double,
              ratioPosNeg: Double,
//              we: Option[Double] = None,
            ): BeliefNetwork = {
    assert(0 <= density && density <= 1)
    assert(0 <= ratioPosNeg && ratioPosNeg <= 1)
    val network: WUnDiGraph[String] = WUnDiGraph.random(size-1, density)
    val nrNegativeConstraints: Int = (network.edges.size * (1 - ratioPosNeg)).intValue
    val negativeConstraints: Set[WUnDiEdge[Node[String]]] = Random.shuffle(network.edges.toList).take(nrNegativeConstraints).toSet

    new BeliefNetwork(network, negativeConstraints)
  }

  /** Generate a belief network from a fixed number of vertices, edges and negative constraints
   * with edges having random (uniformly drawn) weights between 0 and 1.
   *
   * @param size
   *  The number of vertices in the network
   * @param nrEdges
   *  The number of edges in the network
   * @param nrNegativeEdges
   *  The number of negative constraints in the belief network where
   *  The number of negative constraints must be smaller than or equal to the total number of edges
   * @return
   *  A randomly generated BeliefNetwork
   */
  def random(
              size: Int,
              nrEdges: Int,
              nrNegativeEdges: Int,
              //              we: Option[Double] = None,
            ): BeliefNetwork = {
    assert(nrEdges <= size*size)
    assert(nrNegativeEdges <= nrEdges)

    // Remove edge candidates that result int self-edges or are reversed duplicates of other edge candidates
    def noDuplicateEdges(endpoints: (Int, Int)): Boolean = {
      endpoints._1 < endpoints._2
    }

    // Generate vertex and edge candidates (candidates stay as Int for easier processing)
    val vertexCandidates: Set[Int] = (0 until size).toSet
    val edgeCandidates: Set[(Int, Int)] = (vertexCandidates x vertexCandidates).filter(noDuplicateEdges)

    // Generate vertices and edges
    val vertices: Set[Node[String]] = vertexCandidates.map("N" + _).map(Node(_))
    val edges: Set[WUnDiEdge[Node[String]]] = edgeCandidates.take(nrEdges)
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

