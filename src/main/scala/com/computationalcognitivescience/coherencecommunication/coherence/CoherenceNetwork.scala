//package com.computationalcognitivescience.coherencecommunication.coherence
//
//import mathlib.graph._
//import mathlib.set.SetTheory._
//
//import scala.annotation.tailrec
//
//class CoherenceNetwork(
//    override val vertices: Set[Node[WeightedBelief]],
//    val positiveConstraints: Set[WUnDiEdge[Node[WeightedBelief]]],
//    val negativeConstraints: Set[WUnDiEdge[Node[WeightedBelief]]],
//    val accepted: Set[Node[WeightedBelief]],
//    val rejected: Set[Node[WeightedBelief]],
//    val unassigned: Set[Node[WeightedBelief]]
//) extends WUnDiGraph[WeightedBelief](vertices, positiveConstraints /\ negativeConstraints) {
//
//  // Based on Thagard & Verbeurgt, 1998
//  // Based on Blokpoel, M. & van Rooij, I. (2021). Theoretical modeling for cognitive science and psychology Chapter 5
//  // Generalized discriminating coherence
//  def dCoherence(
//      preferredAssignment: Map[Node[WeightedBelief], Boolean]
//  ): Map[Node[WeightedBelief], Boolean] = {
//
//    // Output
//    val allAssignments = vertices allMappings Set(true, false)
//    allAssignments.argMax(dcoh(_, preferredAssignment)).random.get
//  }
//
//  // Generalized Foundational Coherence (Annotated Coherence?)
//  def fCoherence(
//      requiredAssignment: Map[Node[WeightedBelief], Boolean]
//  ): Map[Node[WeightedBelief], Boolean] = {
//
//    // Check if truth-value assignment is valid (i.e. all foundational vertices have their required truth-value)
//    def isValidAssignment(assignment: Map[Node[WeightedBelief], Boolean]): Boolean = {
//
//      // Check if foundational vertex has its required truth-value
//      def isSatisfied(vertex: Node[WeightedBelief]): Boolean = {
//        requiredAssignment(vertex) == assignment(vertex)
//      }
//
//      requiredAssignment.keySet.forall(isSatisfied)
//    }
//
//    // Output
//    val allAssignments =
//      vertices allMappings Set(true, false) // Generate all possible truth-value assignments
//    val validAssignments = allAssignments.filter(
//      isValidAssignment
//    ) // Filter those truth-value assignments that are invalid
//    validAssignments
//      .argMax(coh)
//      .random
//      .get // Return the truth-value assignment that maximizes coherence value
//  }
//
//  // Based on Blokpoel, M. & van Rooij, I. (2021). Theoretical modeling for cognitive science and psychology Chapter 5
//  // Standard coherence
//  def coherence(): Map[Node[WeightedBelief], Boolean] = {
//
//    // Output
//    // Get the truth-assignment that maximizes coherence
//    val allAssignments =
//      vertices allMappings Set(true, false) // Generate all possible truth-value assignments
//    allAssignments
//      .argMax(coh)
//      .random
//      .get // Return the truth-value assignment that maximizes coherence value
//  }
//
//  // Calculate the (generalized) discriminating coherence value of a truth-value assignment
//  def dcoh(
//      assignment: Map[Node[WeightedBelief], Boolean],
//      preferredAssignment: Map[Node[WeightedBelief], Boolean]
//  ): Double = {
//
//    // Check for (u,v) if T(u) == T(v)
//    def satisfiedPositiveConstraint(edge: WUnDiEdge[Node[WeightedBelief]]): Boolean = {
//      assignment(edge.left) == assignment(edge.right)
//    }
//
//    // Check for (u,v) if T(u) != T(v)
//    def satisfiedNegativeConstraint(edge: WUnDiEdge[Node[WeightedBelief]]): Boolean = {
//      assignment(edge.left) != assignment(edge.right)
//    }
//
//    // Check for v if T(v) == T_pref(v)
//    def satisfiedVertex(vertex: Node[WeightedBelief]): Boolean = {
//      assignment(vertex) == preferredAssignment(vertex)
//    }
//
//    // Sum weights of satisfied positive constraints
//    def coh_plus(): Double = {
//      { positiveConstraints | satisfiedPositiveConstraint _ }
//        .map((edge: WUnDiEdge[Node[WeightedBelief]]) => edge.weight)
//        .sum
//    }
//
//    // Sum weights of satisfied negative constraints
//    def coh_min(): Double = {
//      { negativeConstraints | satisfiedNegativeConstraint }
//        .map((edge: WUnDiEdge[Node[WeightedBelief]]) => edge.weight)
//        .sum
//    }
//
//    // Sum weights of satisfied vertices in the preferred set
//    val preferredSet: Set[Node[WeightedBelief]] = preferredAssignment.keySet
//    def coh_p(): Double = {
//      { preferredSet | satisfiedVertex }
//        .map((vertex: Node[WeightedBelief]) => vertex.label.weight)
//        .sum
//    }
//
//    coh_plus + coh_min + coh_p
//  }
//
//  // Calculate the coherence value as the weighted sum of satisfied edges
//  def coh(assignment: Map[Node[WeightedBelief], Boolean]): Double = {
//
//    def satisfiedPositiveConstraint(edge: WUnDiEdge[Node[WeightedBelief]]): Boolean = {
//      assignment(edge.left) == assignment(edge.right)
//    }
//
//    def satisfiedNegativeConstraint(edge: WUnDiEdge[Node[WeightedBelief]]): Boolean = {
//      assignment(edge.left) != assignment(edge.right)
//    }
//
//    def coh_plus(): Double = {
//      { positiveConstraints | satisfiedPositiveConstraint }
//        .map((edge: WUnDiEdge[Node[WeightedBelief]]) => edge.weight)
//        .sum
//    }
//
//    def coh_min(): Double = {
//      { negativeConstraints | satisfiedNegativeConstraint }
//        .map((edge: WUnDiEdge[Node[WeightedBelief]]) => edge.weight)
//        .sum
//    }
//
//    coh_plus + coh_min
//  }
//
//  // FPT algorithm for the decision version of pos-annotated coherence
//  def cMinusCoherence(
//      threshold: Double
//  ): (Map[Node[WeightedBelief], Boolean], Double) = {
//    // Get all unassigned nodes incident to a negative constraint
//    def unassignedNegativeConstraints(): Set[Node[WeightedBelief]] = {
//      negativeConstraints
//        .flatMap(e => Set(e.left, e.right))
//        .intersect(unassigned)
//    }
//
//    val unassignedMinus: Set[Node[WeightedBelief]] = unassignedNegativeConstraints()
//    val unassignedPlus: Set[Node[WeightedBelief]]  = unassigned -- unassignedMinus
//
//    // Apply AC1 exhaustively
//    val negEdgeAssigned: Set[CoherenceNetwork] = ac1(this, unassignedMinus)
//
//    // Apply AC2 where possible
//    val determinedEdgeRemoved: Set[(CoherenceNetwork, Double)] =
//      negEdgeAssigned.map(ac2(_, unassignedPlus, threshold))
//
////    // Select the instance that has the highest coherence value already satisfied
////    val highestCoherence = argMax(determinedEdgeRemoved, (instance: (CoherenceNetwork, Double)) => -instance._2)
//
//    // Apply AC3 where possible
//    val maxFlowGraphs: Set[(CoherenceNetwork, Double)] =
//      // Apply AC3 to the graph, pass the threshold value as is
//      determinedEdgeRemoved.map(instance => (ac3(instance._1), instance._2))
//
//    // Get the optimal truth-value distribution and coherence value of all graphs
//    def getPartition(
//        instance: (CoherenceNetwork, Double)
//    ): (Map[Node[WeightedBelief], Boolean], Double) = {
//      val (partition, coh) = maxFlow(instance._1)
//      (partition, coh + instance._2)
//    }
//    val partitions = maxFlowGraphs.map(getPartition)
//
//    // Get the partition with the highest coherence value
//    partitions.argMax((e: (Map[Node[WeightedBelief], Boolean], Double)) => e._2).random.get
//  }
//
//  // Branching rule
//  // Observation: for an optimal partition, any vertex that is connected by a negative constraint must either be accepted or rejected
//  // Therefore, branch on unassigned vertices incident to a negative constraint such that we have 2 graphs
//  // On graph where the vertex is accepted, and one where it is rejected
//  def ac1(
//      network: CoherenceNetwork,                 // The coherence network
//      unassignedMinus: Set[Node[WeightedBelief]] // All nodes incident to a negative constraint
//  ): Set[CoherenceNetwork] = {
//    if (unassignedMinus.isEmpty) {
//      Set(network)
//    } else {
//      val unassignedNode: Node[WeightedBelief]            = unassignedMinus.random.get
//      val unassignedMinusPrime: Set[Node[WeightedBelief]] = unassignedMinus - unassignedNode
//
//      // New Coherence network
//      // Assign random unassigned node to the accepted set
//      val nodeAcceptedNetwork: CoherenceNetwork = new CoherenceNetwork(
//        network.vertices,
//        network.positiveConstraints,
//        network.negativeConstraints,
//        network.accepted + unassignedNode,
//        network.rejected,
//        network.unassigned - unassignedNode
//      )
//
//      // New Coherence network
//      // Assign random unassigned node to the rejected set
//      val nodeRejectedNetwork: CoherenceNetwork = new CoherenceNetwork(
//        network.vertices,
//        network.positiveConstraints,
//        network.negativeConstraints,
//        network.accepted,
//        network.rejected + unassignedNode,
//        network.unassigned - unassignedNode
//      )
//
//      // Call AC1 again on both new coherence networks
//      ac1(nodeAcceptedNetwork, unassignedMinusPrime) ++ ac1(
//        nodeRejectedNetwork,
//        unassignedMinusPrime
//      )
//    }
//  }
//
//  // Remove determined constraints rule
//  def ac2(
//      network: CoherenceNetwork,
//      unassignedPlus: Set[Node[WeightedBelief]],
//      threshold: Double
//  ): (CoherenceNetwork, Double) = {
//
//    // Collect all positive constraints that are already satisfied (i.e. find those where none of the endpoints are unassigned)
//    def isPositiveConstraintDetermined(edge: WUnDiEdge[Node[WeightedBelief]]): Boolean = {
//      !(unassignedPlus.contains(edge.left) || unassignedPlus.contains(edge.right))
//    }
//
//    // For positive constraints we need to check if they've been determined already
//    val dPosConstraints: Set[WUnDiEdge[Node[WeightedBelief]]] =
//      network.positiveConstraints.filter(isPositiveConstraintDetermined)
//
//    // Because of the application of AC1 we know all negative constraints are already satisfied
//    val dNegConstraints: Set[WUnDiEdge[Node[WeightedBelief]]] = network.negativeConstraints
//
//    // If both edges are in the accepted or the rejected set, return true
//    // ASSUMPTION: edge is determined (i.e. neither end is unassigned)
//    def satisfiedPositiveConstraint(edge: WUnDiEdge[Node[WeightedBelief]]): Boolean = {
//      network.accepted.contains(edge.left) == network.accepted.contains(edge.right)
//    }
//
//    def satisfiedNegativeConstraint(edge: WUnDiEdge[Node[WeightedBelief]]): Boolean = {
//      network.accepted.contains(edge.left) != network.accepted.contains(edge.right)
//    }
//
//    val dPosCoherence = dPosConstraints
//      .filter(satisfiedPositiveConstraint)
//      .map(e => e.weight)
//      .sum
//    val dNegCoherence = dNegConstraints
//      .filter(satisfiedNegativeConstraint)
//      .map(e => e.weight)
//      .sum
//
//    // Create new network with all determined constraints removed
//    val networkPrime: CoherenceNetwork = new CoherenceNetwork(
//      network.vertices,
//      network.positiveConstraints -- dPosConstraints,
//      Set.empty,
//      network.accepted,
//      network.rejected,
//      network.unassigned
//    )
//
//    // Subtract those weights from the threshold that are already determined
//    val thresholdPrime: Double = threshold - (dPosCoherence + dNegCoherence)
//
//    (networkPrime, thresholdPrime)
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
//        edge: WUnDiEdge[Node[WeightedBelief]]
//    ): WUnDiEdge[Node[WeightedBelief]] = {
//      if (network.unassigned.contains(edge.left)) {
//        WUnDiEdge(left = edge.left, right = acceptedNode, weight = edge.weight)
//      } else {
//        WUnDiEdge(left = edge.right, right = acceptedNode, weight = edge.weight)
//      }
//    }
//
//    // Replace a constraint that is incident to a rejected node with one that is connected to new node 'r'
//    def replaceConstraintRPrime(
//        edge: WUnDiEdge[Node[WeightedBelief]]
//    ): WUnDiEdge[Node[WeightedBelief]] = {
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
//        edge: WUnDiEdge[Node[WeightedBelief]]
//    ): Set[WDiEdge[Node[WeightedBelief]]] = {
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
//        network: WDiGraph[WeightedBelief]
//    ): WDiGraph[Node[WeightedBelief]] = {
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
//      network: WDiGraph[WeightedBelief],
//      startNode: Node[WeightedBelief],
//      targetNode: Node[WeightedBelief]
//  ): List[Node[WeightedBelief]] = {
//    assert(network.vertices.contains(startNode))
//    assert(network.vertices.contains(targetNode))
//
//    class NodeWithParent(val self: Node[WeightedBelief], val parent: Node[WeightedBelief])
//
//    def findNeighboursInGraph(
//        network: WDiGraph[WeightedBelief],
//        node: Node[WeightedBelief]
//    ): Set[Node[WeightedBelief]] = {
//      assert(network.vertices.contains(node))
//
//      def getNeighbourIfIncident(
//          edge: WDiEdge[Node[WeightedBelief]],
//          self: Node[WeightedBelief]
//      ): Set[Node[WeightedBelief]] = {
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
//        network: WDiGraph[WeightedBelief],
//        startNode: Node[WeightedBelief],
//        targetNode: Node[WeightedBelief],
//        queue: List[NodeWithParent],
//        explored: Set[Node[WeightedBelief]],
//        parents: Set[NodeWithParent]
//    ): (Set[NodeWithParent], Boolean) = {
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
//
//}
