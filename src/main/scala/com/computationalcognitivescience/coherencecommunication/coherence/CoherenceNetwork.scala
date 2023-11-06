package com.computationalcognitivescience.coherencecommunication.coherence

import jdk.jfr.Threshold
import mathlib.graph._
import mathlib.set.SetTheory._

class CoherenceNetwork(
    override val vertices: Set[Node[WeightedBelief]],
    val positiveConstraints: Set[WUnDiEdge[Node[WeightedBelief]]],
    val negativeConstraints: Set[WUnDiEdge[Node[WeightedBelief]]],
    val accepted: Set[Node[WeightedBelief]],
    val rejected: Set[Node[WeightedBelief]],
    val unassigned: Set[Node[WeightedBelief]]
) extends WUnDiGraph[WeightedBelief](vertices, positiveConstraints /\ negativeConstraints) {

  // Based on Thagard & Verbeurgt, 1998
  // Based on Blokpoel, M. & van Rooij, I. (2021). Theoretical modeling for cognitive science and psychology Chapter 5
  // Generalized discriminating coherence
  def dCoherence(
      preferredAssignment: Map[Node[WeightedBelief], Boolean]
  ): Map[Node[WeightedBelief], Boolean] = {

    // Output
    val allAssignments = vertices allMappings Set(true, false)
    allAssignments.argMax(dcoh(_, preferredAssignment)).random.get
  }

  // Generalized Foundational Coherence (Annotated Coherence?)
  def fCoherence(
      requiredAssignment: Map[Node[WeightedBelief], Boolean]
  ): Map[Node[WeightedBelief], Boolean] = {

    // Check if truth-value assignment is valid (i.e. all foundational vertices have their required truth-value)
    def isValidAssignment(assignment: Map[Node[WeightedBelief], Boolean]): Boolean = {

      // Check if foundational vertex has its required truth-value
      def isSatisfied(vertex: Node[WeightedBelief]): Boolean = {
        requiredAssignment(vertex) == assignment(vertex)
      }

      requiredAssignment.keySet.forall(isSatisfied)
    }

    // Output
    val allAssignments =
      vertices allMappings Set(true, false) // Generate all possible truth-value assignments
    val validAssignments = allAssignments.filter(
      isValidAssignment
    ) // Filter those truth-value assignments that are invalid
    validAssignments
      .argMax(coh)
      .random
      .get // Return the truth-value assignment that maximizes coherence value
  }

  // Based on Blokpoel, M. & van Rooij, I. (2021). Theoretical modeling for cognitive science and psychology Chapter 5
  // Standard coherence
  def coherence(): Map[Node[WeightedBelief], Boolean] = {

    // Output
    // Get the truth-assignment that maximizes coherence
    val allAssignments =
      vertices allMappings Set(true, false) // Generate all possible truth-value assignments
    allAssignments
      .argMax(coh)
      .random
      .get // Return the truth-value assignment that maximizes coherence value
  }

  // Calculate the (generalized) discriminating coherence value of a truth-value assignment
  def dcoh(
      assignment: Map[Node[WeightedBelief], Boolean],
      preferredAssignment: Map[Node[WeightedBelief], Boolean]
  ): Double = {

    // Check for (u,v) if T(u) == T(v)
    def satisfiedPositiveConstraint(edge: WUnDiEdge[Node[WeightedBelief]]): Boolean = {
      assignment(edge.left) == assignment(edge.right)
    }

    // Check for (u,v) if T(u) != T(v)
    def satisfiedNegativeConstraint(edge: WUnDiEdge[Node[WeightedBelief]]): Boolean = {
      assignment(edge.left) != assignment(edge.right)
    }

    // Check for v if T(v) == T_pref(v)
    def satisfiedVertex(vertex: Node[WeightedBelief]): Boolean = {
      assignment(vertex) == preferredAssignment(vertex)
    }

    // Sum weights of satisfied positive constraints
    def coh_plus(): Double = {
      { positiveConstraints | satisfiedPositiveConstraint }
        .map((edge: WUnDiEdge[Node[WeightedBelief]]) => edge.weight)
        .sum
    }

    // Sum weights of satisfied negative constraints
    def coh_min(): Double = {
      { negativeConstraints | satisfiedNegativeConstraint }
        .map((edge: WUnDiEdge[Node[WeightedBelief]]) => edge.weight)
        .sum
    }

    // Sum weights of satisfied vertices in the preferred set
    val preferredSet: Set[Node[WeightedBelief]] = preferredAssignment.keySet
    def coh_p(): Double = {
      { preferredSet | satisfiedVertex }
        .map((vertex: Node[WeightedBelief]) => vertex.label.weight)
        .sum
    }

    coh_plus + coh_min + coh_p
  }

  // Calculate the coherence value as the weighted sum of satisfied edges
  def coh(assignment: Map[Node[WeightedBelief], Boolean]): Double = {

    def satisfiedPositiveConstraint(edge: WUnDiEdge[Node[WeightedBelief]]): Boolean = {
      assignment(edge.left) == assignment(edge.right)
    }

    def satisfiedNegativeConstraint(edge: WUnDiEdge[Node[WeightedBelief]]): Boolean = {
      assignment(edge.left) != assignment(edge.right)
    }

    def coh_plus(): Double = {
      { positiveConstraints | satisfiedPositiveConstraint }
        .map((edge: WUnDiEdge[Node[WeightedBelief]]) => edge.weight)
        .sum
    }

    def coh_min(): Double = {
      { negativeConstraints | satisfiedNegativeConstraint }
        .map((edge: WUnDiEdge[Node[WeightedBelief]]) => edge.weight)
        .sum
    }

    coh_plus + coh_min
  }

  // FPT algorithm for the decision version of pos-annotated coherence
  def cMinusCoherenceDecision(
      threshold: Double
  ): Boolean = {
    // Get all unassigned nodes incident to a negative constraint
    def unassignedNegativeConstraints(): Set[Node[WeightedBelief]] = {
      negativeConstraints
        .flatMap(e => Set(e.left, e.right))
        .intersect(unassigned)
    }

    val unassignedMinus = unassignedNegativeConstraints()
    val unassignedPlus  = unassigned - unassignedMinus

    // Apply AC1 exhaustively
    val leaves: Set[CoherenceNetwork] = AC1(this, unassignedMinus)

    // Apply AC2 where possible
    ???
  }

  // Branching rule
  // Observation: for an optimal partition, any vertex that is connected by a negative constraint must either be accepted or rejected
  // Therefore, branch on unassigned vertices incident to a negative constraint such that we have 2 graphs
  // On graph where the vertex is accepted, and one where it is rejected
  def AC1(
      network: CoherenceNetwork,                 // The coherence network
      unassignedMinus: Set[Node[WeightedBelief]] // All nodes incident to a negative constraint
  ): Set[CoherenceNetwork] = {
    if (unassignedMinus.isEmpty) {
      Set(network)
    } else {
      val unassignedNode: Node[WeightedBelief]            = unassignedMinus.random.get
      val unassignedMinusPrime: Set[Node[WeightedBelief]] = unassignedMinus - unassignedNode

      // New Coherence network
      // Assign random unassigned node to the accepted set
      val nodeAcceptedNetwork: CoherenceNetwork = new CoherenceNetwork(
        network.vertices,
        network.positiveConstraints,
        network.negativeConstraints,
        network.accepted + unassignedNode,
        network.rejected,
        network.unassigned - unassignedNode
      )

      // New Coherence network
      // Assign random unassigned node to the rejected set
      val nodeRejectedNetwork: CoherenceNetwork = new CoherenceNetwork(
        network.vertices,
        network.positiveConstraints,
        network.negativeConstraints,
        network.accepted,
        network.rejected + unassignedNode,
        network.unassigned - unassignedNode
      )

      // Call AC1 again on both new coherence networks
      AC1(nodeAcceptedNetwork, unassignedMinusPrime) + AC1(nodeRejectedNetwork, unassignedMinusPrime)
    }
  }

  // Remove determined constraints rule
  def AC2(
      network: CoherenceNetwork,
      unassignedPlus: Set[Node[WeightedBelief]],
      threshold: Double
  ): (CoherenceNetwork, Double) = {

    // Collect all positive constraints that are already satisfied (i.e. find those where none of the endpoints are unassigned)
    def isPositiveConstraintDetermined(edge: WUnDiEdge[Node[WeightedBelief]]): Boolean = {
      !(unassignedPlus.contains(edge.left) || unassignedPlus.contains(edge.right))
    }

    // For positive constraints we need to check if they've been determined already
    val dPosConstraints: Set[WUnDiEdge[Node[WeightedBelief]]] =
      network.positiveConstraints.filter(isPositiveConstraintDetermined)

    // Because of the application of AC1 we know all negative constraints are already satisfied
    val dNegConstraints: Set[WUnDiEdge[Node[WeightedBelief]]] = network.negativeConstraints

    // If both edges are in the accepted or the rejected set, return true
    // ASSUMPTION: edge is determined (i.e. neither end is unassigned)
    def satisfiedPositiveConstraint(edge: WUnDiEdge[Node[WeightedBelief]]): Boolean = {
      network.accepted.contains(edge.left) == network.accepted.contains(edge.right)
    }

    def satisfiedNegativeConstraint(edge: WUnDiEdge[Node[WeightedBelief]]): Boolean = {
      network.accepted.contains(edge.left) != network.accepted.contains(edge.right)
    }

    val dPosCoherence = dPosConstraints
      .filter(satisfiedPositiveConstraint)
      .map(e => e.weight)
    val dNegCoherence = dNegConstraints
      .filter(satisfiedNegativeConstraint)
      .map(e => e.weight)

    // Create new network with all determined constraints removed
    val networkPrime: CoherenceNetwork = new CoherenceNetwork(
      network.vertices,
      network.positiveConstraints -- dPosConstraints,
      Set.empty,
      network.accepted,
      network.rejected,
      network.unassigned
    )

    // Subtract those weights from the threshold that are already determined
    val thresholdPrime: Double = threshold - (dPosCoherence + dNegCoherence)

    (networkPrime, thresholdPrime)
  }

  // Merge Accepted and Rejected vertices rule
  def AC3(network: CoherenceNetwork): CoherenceNetwork = {
    val acceptedNode: Node[WeightedBelief] = Node(WeightedBelief("a", 0))
    val rejectedNode: Node[WeightedBelief] = Node(WeightedBelief("r", 0))

    // Check if a constraint is incident to an accepted node
    def incidentToA(edge: WUnDiEdge[Node[WeightedBelief]]): Boolean = {
      network.accepted.contains(edge.left) || network.accepted.contains(edge.right)
    }

    // Check if a constraint is incident to a rejected node
    def incidentToR(edge: WUnDiEdge[Node[WeightedBelief]]): Boolean = {
      network.rejected.contains(edge.left) || network.rejected.contains(edge.right)
    }

    // Collect all constraints that are incident to an accepted node
    val constraintsAPrime: Set[WUnDiEdge[Node[WeightedBelief]]] = network.positiveConstraints.filter(incidentToA)

    // Collect all constraints that are incident to an rejected node
    val constraintsRPrime: Set[WUnDiEdge[Node[WeightedBelief]]] = network.positiveConstraints.filter(incidentToR)

    // Replace a constraint that is incident to an accepted node with one that is connected to new node 'a'
    def replaceConstraintAPrime(edge:WUnDiEdge[Node[WeightedBelief]]): WUnDiEdge[Node[WeightedBelief]] = {
      if (network.unassigned.contains(edge.left)) {
        WUnDiEdge(left = edge.left, right = acceptedNode, weight = edge.weight)
      }
      else {
        WUnDiEdge(left = edge.right, right = acceptedNode, weight = edge.weight)
      }
    }

    // Replace a constraint that is incident to a rejected node with one that is connected to new node 'r'
    def replaceConstraintRPrime(edge:WUnDiEdge[Node[WeightedBelief]]): WUnDiEdge[Node[WeightedBelief]] = {
      if (network.unassigned.contains(edge.left)) {
        WUnDiEdge(left = edge.left, right = rejectedNode, weight = edge.weight)
      }
      else {
        WUnDiEdge(left = edge.right, right = rejectedNode, weight = edge.weight)
      }
    }

    // Replace a constraint from A-Prime with a new constraint in A-Star (A-star is a subset of P' x {a})
    val newConstraintsAStar = constraintsAPrime.map(replaceConstraintAPrime)
    // Replace a constraint from R-Prime with a new constraint in R-Star (R-star is a subset of P' x {r})
    val newConstraintsRStar = constraintsRPrime.map(replaceConstraintRPrime)

    // The new graph only has the old unassigned nodes plus the "a" and "r" nodes
    val newNodes =  network.unassigned ++ Set(acceptedNode, rejectedNode)

    // Replace all constraints that were incident to A' and R' with their replacing constraints connecting to 'a' and 'r'.
    val newConstraints = network.positiveConstraints -- (constraintsAPrime ++ constraintsRPrime) ++ newConstraintsAStar ++ newConstraintsRStar

    new CoherenceNetwork(
      newNodes,
      newConstraints,
      Set.empty,
      Set(acceptedNode),
      Set(rejectedNode),
      network.unassigned
    )
  }

  // Ford-Fulkerson
  def minCut(network: CoherenceNetwork): Map[Node[WeightedBelief], Boolean] = {
    val acceptedSet: Set[Node[WeightedBelief]] = network.accepted // {a}
    val rejectedSet: Set[Node[WeightedBelief]] = network.rejected // {r}

    // Create directed graph
    def createDirectedEdges(edge: WUnDiEdge[Node[WeightedBelief]]): Set[WDiEdge[Node[WeightedBelief]]] = {
      ???
    }
    val edges = network.edges
    val dirGraph = ???
    ???
  }
}
