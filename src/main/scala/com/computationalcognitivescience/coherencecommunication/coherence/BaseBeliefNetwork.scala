package com.computationalcognitivescience.coherencecommunication.coherence

import com.computationalcognitivescience.coherencecommunication.coherence.Belief.Belief
import mathlib.graph._
import mathlib.set.SetTheory._

import scala.annotation.tailrec

trait BaseBeliefNetwork {

  val graph: WUnDiGraph[String]
  val negativeConstraints: Set[WUnDiEdge[Belief]]
  require(
    negativeConstraints isSubsetEqTo graph.edges,
    "The set of negative constraints is not a subset of or equal to the edges in the graph."
  )
  val positiveConstraints: Set[WUnDiEdge[Belief]] = graph.edges \ negativeConstraints

  def vertices: Set[Belief] = graph.vertices
  def edges: Set[WUnDiEdge[Belief]] = graph.edges
  def size: Int = graph.size

  /** Check if in the given truth-value assignment a positive constraint is satisfied
    *
    * @param assignment
    *   The truth-value assignment over vertices
    * @param edge
    *   A positive constraint
    * @return
    *   True if the constraint is satisfied, false otherwise
    */
  protected def isSatisfiedPositiveConstraint(assignment: TruthValueAssignment)(
      edge: WUnDiEdge[Belief]
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
  protected def isSatisfiedNegativeConstraint(assignment: TruthValueAssignment)(
      edge: WUnDiEdge[Belief]
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
  protected def isDeterminedConstraint(assignment: TruthValueAssignment)(
      edge: WUnDiEdge[Belief]
  ): Boolean = assignment.contains(edge.left) && assignment.contains(edge.right)

  /** Calculate the coherence-value from positive constraints with a given truth-value assignment
    *
    * @param assignment
    *   A truth-value assignment over vertices
    * @return
    *   The weighted sum over satisfied positive constraints
    */
  protected def cohPlus(assignment: TruthValueAssignment): Double = {
    sum(
      { positiveConstraints | isSatisfiedPositiveConstraint(assignment) _ },
      (edge: WUnDiEdge[Belief]) => edge.weight
    )
  }

  /** Calculate the coherence-value from negative constraints with a given truth-value assignment
    *
    * @param assignment
    *   A truth-value assignment over vertices
    * @return
    *   The weighted sum over satisfied negative constraints
    */
  protected def cohMin(assignment: TruthValueAssignment): Double = {
    val satisfiedNegativeConstraints: Set[WUnDiEdge[Belief]] =
      negativeConstraints.filter(isSatisfiedNegativeConstraint(assignment))

    satisfiedNegativeConstraints.toList
      .map((edge: WUnDiEdge[Belief]) => edge.weight) // Get weights
      .sum                                                 // Sum weights

  }

  /** Calculate the coherence-value from all constraints with given truth-value assignment
    *
    * @param assignment
    *   A truth-value assignment over vertices
    * @return
    *   The weighted sum over all satisfied constraints
    */
  def coh(assignment: TruthValueAssignment): Double =
    cohPlus(assignment) + cohMin(assignment)

  def coherence(): TruthValueAssignment =
    coherenceSolutions().random.get // Return the truth-value assignment that maximizes coherence value

  /** Calculate the optimal truth-value assignment of this BeliefNetwork
    *
    * Based on Blokpoel, M. & van Rooij, I. (2021). Theoretical modeling for cognitive science and
    * psychology Chapter 5
    *
    * @return
    *   A truth-value assignment over vertices that results in maximum coherence If multiple maximal
    *   truth-value assignments exists, get a random maximal one.
    */
  def coherenceSolutions(): Set[TruthValueAssignment] = {
    // Get the truth-assignment that maximizes coherence
    val allAssignments =
      (graph.vertices allMappings Set(true, false))               // Generate all possible truth-value assignments
        .map(tva => TruthValueAssignment(tva.keySet, tva.toSet))  // Convert Map to TruthValueAssignment
    allAssignments.argMax(coh)
  }

  //// FPT-ALGORITHM BLOW ////

  /** Generate all possible truth-value assignments over nodes incident to a negative constraint
    * O(pow(2,unassignedMinus))
    *
    * Branching rule Observation: for an optimal partition, any vertex that is connected by a
    * negative constraint must either be accepted or rejected Therefore, branch on unassigned
    * vertices incident to a negative constraint such that we have 2 graphs On graph where the
    * vertex is accepted, and one where it is rejected Effectively this generates all possible
    * truth-value assignments over vertices incident to a negative edge
    *
    * @param unassignedMinus
    *   Set of nodes incident to a negative constraint
    * @return
    *   All possible truth value assignments over unassignedMinus
    */
  protected def ac1(
      unassignedMinus: Set[Belief] // All nodes incident to a negative constraint
  ): Set[TruthValueAssignment] =
    (unassignedMinus.allMappings(Set(true, false)))
      .map(tva => TruthValueAssignment(tva.keySet, tva.toSet))

  /** Given a graph and a truth-value assignment, remove all determined constraints from the graph
    *
    * Remove determined constraints rule If an constraint is determined (i.e. both of its endpoints
    * have a truth-value assignment) then remove it from the graph, if it also satisfied, keep track
    * of the coherence-value that would be acquired with this constraint.
    *
    * @param assignmentSet
    *   A truth-value assignment over Nodes
    * @return
    *   A tuple containing
    *   1. A Weighted Undirected Graph with determined constraints removed 2. The truth-value
    *      assignment over Nodes 3. The sum coherence value of satisfied constraints
    */
  protected def ac2(
      assignmentSet: Set[TruthValueAssignment]
  ): (WUnDiGraph[String], Set[(TruthValueAssignment, Double)]) = {

    // Because the set of *determined* constraints (positive or negative) is the same for all truth-value assignments
    // We can take a any truth-value assignment to determine the determined constraints
    val randomAssignment: TruthValueAssignment = assignmentSet.random.get

    // For positive constraints we need to check if they've been determined already
    val dPosConstraints: Set[WUnDiEdge[Belief]] =
      positiveConstraints.filter(isDeterminedConstraint(randomAssignment))

    // Because of the application of AC1 we know all negative constraints are already determined
    val dNegConstraints: Set[WUnDiEdge[Belief]] = negativeConstraints

    // For a set of determined positive constraints, get the coherence value
    def cohDPlus(
        edgeSet: Set[WUnDiEdge[Belief]],
        assignment: TruthValueAssignment
    ): Double = {
      val satisfiedPositiveConstraints: Set[WUnDiEdge[Belief]] =
        edgeSet.filter(isSatisfiedPositiveConstraint(assignment))

      satisfiedPositiveConstraints.toList
        .map((edge: WUnDiEdge[Belief]) => edge.weight) // Get weights
        .sum                                                 // Sum weights
    }

    // For a set of determined negative constraints, get the coherence value

    /** Auxiliary function for calculating the coherence value over determined constraints
      *
      * @param edgeSet
      *   The set of edges that are determined
      * @param assignment
      *   Truth-value assignment over Nodes
      * @return
      *   The sum coherence value over satisfied determined constraints
      */
    def cohDMin(
        edgeSet: Set[WUnDiEdge[Belief]],
        assignment: TruthValueAssignment
    ): Double = {
      val satisfiedNegativeConstraints: Set[WUnDiEdge[Belief]] =
        edgeSet.filter(isSatisfiedNegativeConstraint(assignment))

      satisfiedNegativeConstraints.toList
        .map((edge: WUnDiEdge[Belief]) => edge.weight) // Get weights
        .sum                                                 // Sum weights
    }

    // For each truth-value assignment, get the coherence from already determined constraints
    val assignmentCoherence: Set[(TruthValueAssignment, Double)] = {
      assignmentSet.map((assignment: TruthValueAssignment) =>
        (assignment, cohDPlus(dPosConstraints, assignment) + cohDMin(dNegConstraints, assignment))
      )
    }

    // Remove determined edges
    val edgesPrime = graph.edges -- dPosConstraints -- dNegConstraints

    // Create new graph with new edge set
    val graphPrime: WUnDiGraph[String] = WUnDiGraph(graph.vertices, edgesPrime)

    (graphPrime, assignmentCoherence)
  }

  /** Merge accepted and rejected Nodes
    *
    * Merge Accepted and Rejected vertices rule Merge all determined rejected Nodes into a single
    * {"targetNode"} rejected Node, and all accepted Nodes into a single {"sourceNode"} accepted
    * Node. Edges going to the determined (removed) nodes are also transferred to the new nodes.
    *
    * @param graph
    *   Weighted Undirected Graph
    * @param assignment
    *   Truth-value assignment over Nodes
    * @return
    *   Weighted Undirected Graph
    */
  protected def ac3(
      graph: WUnDiGraph[String],
      assignment: TruthValueAssignment
  ): WUnDiGraph[String] = {
    val acceptedNode: Belief = Node("sourceNode")
    val rejectedNode: Belief = Node("targetNode")

    /** Divides edges into sets encoding incidence to an accepted Node, incidence to rejected Node,
      * or incidence to neither (both ends unassigned). ASSUMPTION: Input graph has all determined
      * edges removed (i.e. edges that have both endpoints be assigned)
      *
      * @param graph
      *   Weighted Undirected Graph
      * @param assignment
      *   Truth-value assignment over Nodes
      * @return
      *   a 3-tuple of sets of edges
      *   1. edges incident to an accepted Node 2. edges incident to an rejected Node 3. edges
      *      incident to neither
      */
    def sortIncidentEdges(graph: WUnDiGraph[String], assignment: TruthValueAssignment): (
        Set[WUnDiEdge[Belief]],
        Set[WUnDiEdge[Belief]],
        Set[WUnDiEdge[Belief]]
    ) = {
      val edgeList: List[WUnDiEdge[Belief]]   = graph.edges.toList
      val incidentToA: Set[WUnDiEdge[Belief]] = Set.empty
      val incidentToR: Set[WUnDiEdge[Belief]] = Set.empty
      val notIncident: Set[WUnDiEdge[Belief]] = Set.empty

      /** Recursive call of sortIncidentEdges
        *
        * @param edgeList
        *   List of edges left to consider
        * @param incidentToA
        *   Set of edges incident to an accepted Node (so far)
        * @param incidentToR
        *   Set of edges incident to an rejected Node (so far)
        * @param notIncident
        *   Set of edges incident to neither (so far)
        * @return
        *   a 3-tuple of sets of edges
        *   1. edges incident to an accepted Node 2. edges incident to an rejected Node 3. edges
        *      incident to neither
        */
      @tailrec
      def sortIncidentEdgesRecursive(
          edgeList: List[WUnDiEdge[Belief]],
          incidentToA: Set[WUnDiEdge[Belief]],
          incidentToR: Set[WUnDiEdge[Belief]],
          notIncident: Set[WUnDiEdge[Belief]]
      ): (
          Set[WUnDiEdge[Belief]],
          Set[WUnDiEdge[Belief]],
          Set[WUnDiEdge[Belief]]
      ) = {
        // If there are no more edges to sort
        if (edgeList.isEmpty) (incidentToA, incidentToR, notIncident)
        else {
          // If the edgeList is non-empty
          val edge: WUnDiEdge[Belief] = edgeList.head
          // if the left Node is assigned
          if (assignment.contains(edge.left)) {
            if (assignment(edge.left).get) // if the left Node is true
              sortIncidentEdgesRecursive(
                edgeList.tail,
                incidentToA + edge,
                incidentToR,
                notIncident
              ) // Assign edge to incidentToA
            else
              sortIncidentEdgesRecursive(
                edgeList.tail,
                incidentToA,
                incidentToR + edge,
                notIncident
              ) // else it must be false and therefore assign the edge to incident to R

            // if the right Node is assigned
          } else if (assignment.contains(edge.right)) {
            if (assignment(edge.right).get) // if the right Node is true
              sortIncidentEdgesRecursive(
                edgeList.tail,
                incidentToA + edge,
                incidentToR,
                notIncident
              ) // Assign edge to incidentToA
            else
              sortIncidentEdgesRecursive(
                edgeList.tail,
                incidentToA,
                incidentToR + edge,
                notIncident
              ) // else it must be false and therefore assign the edge to incident to R
          } else
            sortIncidentEdgesRecursive(
              edgeList.tail,
              incidentToA,
              incidentToR,
              notIncident + edge
            ) // else neither left or right is assigned, assign the edge to notIncident
        }
      }

      if (edgeList.isEmpty) (incidentToA, incidentToR, notIncident)
      else {
        // If the edgeList is non-empty
        val edge: WUnDiEdge[Belief] = edgeList.head
        if (assignment.contains(edge.left)) {
          if (assignment(edge.left).get)
            sortIncidentEdgesRecursive(edgeList.tail, incidentToA + edge, incidentToR, notIncident)
          else
            sortIncidentEdgesRecursive(edgeList.tail, incidentToA, incidentToR + edge, notIncident)
        } else if (assignment.contains(edge.right)) {
          if (assignment(edge.right).get)
            sortIncidentEdgesRecursive(edgeList.tail, incidentToA + edge, incidentToR, notIncident)
          else
            sortIncidentEdgesRecursive(edgeList.tail, incidentToA, incidentToR + edge, notIncident)
        } else
          sortIncidentEdgesRecursive(edgeList.tail, incidentToA, incidentToR, notIncident + edge)
      }
    }

    // Collect all constraints that are incident to an accepted node
    val (constraintsAPrime, constraintsRPrime, notIncidentConstraints): (
        Set[WUnDiEdge[Belief]],
        Set[WUnDiEdge[Belief]],
        Set[WUnDiEdge[Belief]]
    ) =
      sortIncidentEdges(graph, assignment)

    /** Replace the given edge with an edge connected to the special accepted Node {"sourceNode"}
      *
      * @param assignment
      *   Truth-Value assignment over Nodes
      * @param edge
      *   Weighted Undirected Edge
      * @return
      *   Weighted Undirected Edge connected to the special accepted Node
      */
    def replaceConstraintAPrime(
        assignment: TruthValueAssignment,
        edge: WUnDiEdge[Belief]
    ): WUnDiEdge[Belief] = {
      if (assignment.contains(edge.left)) {
        WUnDiEdge(left = edge.right, right = acceptedNode, weight = edge.weight)
      } else {
        WUnDiEdge(left = edge.left, right = acceptedNode, weight = edge.weight)
      }
    }

    /** Replace the given edge with an edge connected to the special rejected Node {"targetNode"}
      *
      * @param assignment
      *   Truth-Value assignment over Nodes
      * @param edge
      *   Weighted Undirected Edge
      * @return
      *   Weighted Undirected Edge connected to the special rejected Node
      */
    def replaceConstraintRPrime(
        assignment: TruthValueAssignment,
        edge: WUnDiEdge[Belief]
    ): WUnDiEdge[Belief] = {
      if (assignment.contains(edge.left)) {
        WUnDiEdge(left = edge.right, right = rejectedNode, weight = edge.weight)
      } else {
        WUnDiEdge(left = edge.left, right = rejectedNode, weight = edge.weight)
      }
    }

    /** Combine duplicate edges (add their weights together) itteratively builds a map containing
      * the (combined) weight of all edges incident to a Node an the TargetNode
      *
      * @param edgeList
      *   List of edges
      * @param targetNode
      *   Node that all edges connect to
      * @return
      *   Set of edges
      */
    def combineDuplicateEdges(
        edgeList: List[WUnDiEdge[Belief]],
        targetNode: Belief
    ): Set[WUnDiEdge[Belief]] = {

      /** Recursive call of combineDuplicateEdges
        *
        * @param edgeList
        *   list of edges
        * @param weightMap
        *   Map of Nodes to (found) edge weights
        * @param targetNode
        *   Node that all edges connect to
        * @return
        *   Set of edges
        */
      @tailrec
      def combineDuplicateEdgesRecursive(
          edgeList: List[WUnDiEdge[Belief]],
          weightMap: Map[Belief, Double],
          targetNode: Belief
      ): Set[WUnDiEdge[Belief]] = {
        if (edgeList.isEmpty) {
          weightMap
            .map((nodeWeightPair: (Belief, Double)) =>
              WUnDiEdge(nodeWeightPair._1, targetNode, nodeWeightPair._2)
            )
            .toSet
        } else {
          val edge: WUnDiEdge[Belief] = edgeList.head
          if (weightMap.contains(edge.left)) {
            val newMap: Map[Belief, Double] =
              weightMap + (edge.left -> (weightMap(edge.left) + edge.weight))
            combineDuplicateEdgesRecursive(edgeList.tail, newMap, targetNode)
          } else {
            val newMap: Map[Belief, Double] = weightMap + (edge.left -> edge.weight)
            combineDuplicateEdgesRecursive(edgeList.tail, newMap, targetNode)
          }
        }
      }

      val weightMap: Map[Belief, Double] = Map.empty
      if (edgeList.isEmpty) Set.empty
      else {
        val edge: WUnDiEdge[Belief]     = edgeList.head
        val newMap: Map[Belief, Double] = weightMap + (edge.left -> edge.weight)
        combineDuplicateEdgesRecursive(edgeList.tail, newMap, targetNode)
      }
    }

    // Replace a constraint from A-Prime with a new constraint in A-Star (A-star is a subset of P' x {a})
    val newConstraintsA: Set[WUnDiEdge[Belief]] = combineDuplicateEdges(
      constraintsAPrime.toList.map(replaceConstraintAPrime(assignment, _)),
      acceptedNode
    )
    // Replace a constraint from R-Prime with a new constraint in R-Star (R-star is a subset of P' x {r})
    val newConstraintsR: Set[WUnDiEdge[Belief]] = combineDuplicateEdges(
      constraintsRPrime.toList.map(replaceConstraintRPrime(assignment, _)),
      rejectedNode
    )

    // The new graph only has the old unassigned nodes plus the "sourceNode" and "targetNode" nodes
    val newNodes: Set[Belief] =
      graph.vertices -- assignment.beliefs ++ Set(acceptedNode, rejectedNode)

    // Replace all constraints that were incident to A' and R' with their replacing constraints connecting to 'a' and 'r'.
    val newConstraints: Set[WUnDiEdge[Belief]] =
      notIncidentConstraints ++ newConstraintsA ++ newConstraintsR

    WUnDiGraph(newNodes, newConstraints)
  }

  /** {C-}-FPT algorithm for coherence as presented by van Rooij (1998) {C-} representing the number
    * of negatively constrained edges
    *
    * @return
    *   A truth-value assignment over vertices that results in maximum coherence If multiple maximal
    *   truth-value assignments exists, get a random maximal one.
    */
  def cMinusCoherence(): TruthValueAssignment = {

    // Get all vertices incident to a negative constraint
    val unassignedMinus: Set[Belief] = negativeConstraints.flatMap(e => Set(e.left, e.right))

    // Apply AC1 exhaustively
    // Effectively: Get all possible truth-value assignments over the vertices incident to a negative constraint
    val assignmentMinusSet: Set[TruthValueAssignment] = ac1(unassignedMinus)

    // Apply AC2 where possible
    // For each truth-value assignment:
    // create a new graph wherein all edges that have a pre-determined truth-value assignment are removed
    val (graphPrime, assignmentCoherence)
        : (WUnDiGraph[String], Set[(TruthValueAssignment, Double)]) = ac2(assignmentMinusSet)

    // Apply AC3 where possible
    // Remove all nodes that have a pre-assigned truth-value assignment and replace them with a single true node and a single false node
    // with each of the edges that went to a removed node being replace with an edge with the same weight going to the single true/false nodes.
    val maxFlowGraphs: Set[
      (
          WUnDiGraph[String],         // Graph
          TruthValueAssignment, // Truth-value assignment of determined Nodes
          Double                      // Coherence value of determined edges
      )
    ] = // Coherence value of determined constraints
      // Apply AC3 to the graph, pass the truth-value assignment and coherence value as is
      assignmentCoherence.map(
        (instance: (
            TruthValueAssignment, // Truth-value assignment of determined Nodes
            Double
        )) => // Coherence value of determined constraints
          (ac3(graphPrime, instance._1), instance._1, instance._2)
      )

    /** Combine the results of getPartition (which applies maxFlow to maxFlowGraph) with
      * pre-determined assignments and coherence values
      *
      * @param maxFlowGraph
      *   Weighted Undirected Graph (representing the belief network after AC3 has been applied)
      * @param predeterminedAssignment
      *   Truth-value assignment of determined Nodes
      * @param predeterminedCoherence
      *   Coherence value of determined edges
      * @return
      *   Tuple of (Truth-value assignment, Coherence-value)
      */
    def combinePartitionWithPredetermined(
        maxFlowGraph: WUnDiGraph[String],
        predeterminedAssignment: TruthValueAssignment,
        predeterminedCoherence: Double
    ): (TruthValueAssignment, Double) = {
      val (assignment: TruthValueAssignment, coherenceValue: Double) = getPartition(
        maxFlowGraph
      )
      (
        assignment ++ predeterminedAssignment - Node("sourceNode") - Node("targetNode"),
        coherenceValue + predeterminedCoherence
      )
    }

    val partitionCoherenceTuples: Set[(TruthValueAssignment, Double)] =
      maxFlowGraphs.map(instance =>
        combinePartitionWithPredetermined(instance._1, instance._2, instance._3)
      )

    // Get the partition with the highest coherence value
    partitionCoherenceTuples
      .argMax((e: (TruthValueAssignment, Double)) => e._2)
      .map(_._1)
      .random
      .get
  }

  /** Perform the Edmonds-Karp algorithm on the given graph
    *
    * @param graph
    *   Weighted Undirected Graph with exactly 1 Node {"sourceNode"} (source node) and 1 Node
    *   {"targetNode"} target Node
    * @return
    *   Final residual Graph (Weighted Directed graph)
    */
  protected def maxFlow(graph: WUnDiGraph[String]): WDiGraph[String] = {
    val sourceNode: Belief = Node("sourceNode")
    val targetNode: Belief = Node("targetNode")

    /** Given an Weighted Undirected Edge, generate two Weighted Directed Edges
      *
      * @param edge
      *   Weighted Undirected Edge
      * @return
      *   A Set of two Weighted Directed Edges
      */
    def createDirectedEdges(
        edge: WUnDiEdge[Belief]
    ): Set[WDiEdge[Belief]] = {
      val firstEdge  = WDiEdge(edge.left, edge.right, edge.weight)
      val secondEdge = WDiEdge(edge.right, edge.left, edge.weight)
      Set(firstEdge, secondEdge)
    }

    // Create directed graph
    val edges: Set[WDiEdge[Belief]] = graph.edges.flatMap(createDirectedEdges)
    val dirGraph: WDiGraph[String]        = WDiGraph(graph.vertices, edges)

    /** Recursively finds the augmenting path through the given Weighted Directed Graph and updates
      * the graph by updating edges
      *
      * @param graph
      *   Weighted Directed Graph
      * @param aList
      *   Adjacency list
      * @param sourceNode
      *   Start Node of the path
      * @param targetNode
      *   End Node of the path
      * @return
      *   Weighted Directed Graph
      */
    @tailrec
    def findAugmentingPathRecursive(
        graph: WDiGraph[String],
        aList: Map[Belief, Set[NodeWeightPair[String]]],
        sourceNode: Belief = sourceNode,
        targetNode: Belief = targetNode
    ): WDiGraph[String] = {
      // Find path from a to r
      val augmentingPath: List[WDiEdge[Belief]] = bfs(graph, sourceNode, targetNode, aList)
      if (augmentingPath.isEmpty) graph
      else {
        // TODO: Implement some way to make this faster (A trait of network that maps a node 2-tuple to an edge?)
        // TODO: or perhaps a map from edge to edge that just maps to its reverse counterpart?
        // Get reverse path
        val reversePath: List[WDiEdge[Belief]] = {
          // Check for each edge if its endpoints are part of the nodes in the path
          val reversePathNodes: List[Belief] = Range(augmentingPath.size - 1, 0, -1).inclusive
            .map(i => augmentingPath(i).right)
            .toList ++ List(sourceNode)
          Range(0, augmentingPath.size, 1)
            .map((i: Int) =>
              WDiEdge(
                reversePathNodes(i),
                reversePathNodes(i + 1),
                aList(reversePathNodes(i))
                  .filter(_.node == reversePathNodes(i + 1))
                  .random
                  .get
                  .weight
              )
            )
            .toList
        }

        // Find the minimum capacity through this path
        val minCapacity: Double = augmentingPath.map(_.weight).min

        /** Recursively traverse list of edges to update the adjacency list
          *
          * @param aList
          *   adjacency list
          * @param edgeList
          *   list of updated edges
          * @return
          *   updated adjacency list
          */
        @tailrec
        def updateAdjacencyList(
            aList: Map[Belief, Set[NodeWeightPair[String]]],
            edgeList: List[WDiEdge[Belief]]
        ): Map[Belief, Set[NodeWeightPair[String]]] = {
          // Base case
          if (edgeList.isEmpty) aList
          else {
            val edge = edgeList.head

            // Take the old adjacency list but ignore the values that needs to be updated
            val newNeighbours: Set[NodeWeightPair[String]] = aList(edge.left)
              .filter((nodeWeightPair: NodeWeightPair[String]) =>
                nodeWeightPair.node != edge.right
              ) + // Add updated weight to the set
              NodeWeightPair[String](edge.right, edge.weight)
            // Update adjacencyList
            val newAList: Map[Belief, Set[NodeWeightPair[String]]] =
              aList + (edge.left -> newNeighbours)
            updateAdjacencyList(newAList, edgeList.tail)
          }
        }

        // Adjust all path capacities
        // Reduce weight of forward edges
        val newForwardEdges: List[WDiEdge[Belief]] = augmentingPath
          .map((e: WDiEdge[Belief]) => WDiEdge(e.left, e.right, e.weight - minCapacity))

        // Increase weight of backward edges
        val newReverseEdges: List[WDiEdge[Belief]] = reversePath
          .map((e: WDiEdge[Belief]) => WDiEdge(e.left, e.right, e.weight + minCapacity))

        val newEdges: List[WDiEdge[Belief]] = newForwardEdges ++ newReverseEdges

        // Update adjacencyList
        val newAList: Map[Belief, Set[NodeWeightPair[String]]] =
          updateAdjacencyList(aList, newEdges)

        val newGraph = WDiGraph(
          graph.vertices,
          graph.edges -- augmentingPath.toSet -- reversePath.toSet ++ newEdges.toSet
        )

        findAugmentingPathRecursive(newGraph, newAList)
      }
    }

    val finalGraph = findAugmentingPathRecursive(dirGraph, dirGraph.adjacencyList)
    finalGraph
  }

  /** Use Breadth-First Search to find the shortest path from startNode ("sourceNode") to targetNode
    * ("targetNode") O(|E| + |V|)
    *
    * @param graph
    *   Weighted Directed Graph
    * @param startNode
    *   Start Node of the path
    * @param targetNode
    *   End Node of the path
    * @param aList
    *   adjacency list
    * @return
    *   Path of edges
    */
  private def bfs(
      graph: WDiGraph[String],
      startNode: Belief,
      targetNode: Belief,
      aList: Map[Belief, Set[NodeWeightPair[String]]]
  ): List[WDiEdge[Belief]] = {
    assert(graph.vertices.contains(startNode))
    assert(graph.vertices.contains(targetNode))

    /** Search through adjacency list to construct edge with the appropriate weight
      *
      * @param left
      *   Left Node
      * @param right
      *   Right Node
      * @param aList
      *   Adjacency list
      * @return
      *   Weighted Directed Edge
      */
    def getEdgeFromNodes(
        left: Belief,
        right: Belief,
        aList: Map[Belief, Set[NodeWeightPair[String]]]
    ): WDiEdge[Belief] = {
      WDiEdge(left, right, aList(left).filter(_.node == right).random.get.weight)
    }

    if (startNode == targetNode) List().empty
    else {

      /** Recursive call of bfs
        *
        * @param graph
        *   Weighted Directed Graph
        * @param aList
        *   adjacency list
        * @param pathToNode
        *   Map of found Nodes the path that was taken to get there
        * @param startNode
        *   Start Node of the path
        * @param targetNode
        *   End Node of the path
        * @param queue
        *   Nodes that still need to be explored
        * @param explored
        *   already explored nodes
        * @return
        */
      @tailrec
      def bfsRecursive(
          graph: WDiGraph[String],
          aList: Map[Belief, Set[NodeWeightPair[String]]],
          pathToNode: Map[Belief, List[WDiEdge[Belief]]],
          startNode: Belief,
          targetNode: Belief,
          queue: List[Belief],
          explored: Set[Belief]
      ): List[WDiEdge[Belief]] = {

        val neighbours: Set[Belief] = aList(startNode)
          .filter(_.weight != 0)
          .map(_.node)
          .filterNot(queue.contains) // Remove nodes that are already in the queue
          .diff(explored)            // Remove nodes that have already been explored

        // Update path to found neighbours
        val newPathToNode: Map[Belief, List[WDiEdge[Belief]]] =
          pathToNode ++
            neighbours
              .map(n => n -> (pathToNode(startNode) ++ List(getEdgeFromNodes(startNode, n, aList))))
              .toMap

        // If the target has been found
        if (neighbours.contains(targetNode)) {
          newPathToNode(targetNode)
        } else {
          // We've explored the current node
          val newExplored: Set[Belief] = explored + startNode
          // Add neighbours to the end of the queue
          val newQueue: List[Belief] =
            queue ++ neighbours.toList

          // If the queue is empty at this point, there is no path from startNode to targetNode
          if (newQueue.isEmpty) {
            List.empty
          } else {
            // Keep searching
            bfsRecursive(
              graph,         // network
              aList,         // adjacencyList
              newPathToNode, // map from node to path
              newQueue.head, // startNode
              targetNode,    // targetNode
              newQueue.tail, // queue
              newExplored    // explored
            )
          }
        }
      }

      val neighbours: Set[Belief] = aList(startNode).filter(_.weight != 0).map(_.node)
      val pathToNode: Map[Belief, List[WDiEdge[Belief]]] = neighbours
        .map(n =>
          n -> List(WDiEdge(startNode, n, aList(startNode).filter(_.node == n).random.get.weight))
        )
        .toMap
      val explored: Set[Belief] = Set(startNode)
      val queue: List[Belief]   = neighbours.toList

      // No path can be found
      if (queue.isEmpty) List.empty
      // The target has been immediately found
      else if (queue.head == targetNode) pathToNode(targetNode)
      // Continue searching from next node
      else {
        bfsRecursive(
          graph,      // network
          aList,      // adjacencyList
          pathToNode, // map from node to path
          queue.head, // startNode
          targetNode, // targetNode
          queue.tail, // queue
          explored    // explored
        )
      }
    }
  }

  /** Get the truth-value assignment and coherence of this 2-connected component graph, where one
    * components is 'true' and the other is 'false'
    *
    * @param graph
    *   Weighted Undirected Graph with exactly 2 connected components
    * @return
    *   A truth-value assignment and its coherence value
    */
  protected def getPartition(
      graph: WUnDiGraph[String]
  ): (TruthValueAssignment, Double) = {

    /** Get all Nodes connected to the startNode
      *
      * @param graph
      *   Weighted Directed Graph
      * @param startNode
      *   Starting node from which to determine the connected component (usually Node("sourceNode"))
      * @return
      *   A set of nodes connected to the start Node
      */
    def getConnected(graph: WDiGraph[String], startNode: Belief): Set[Belief] = {

      /** For a given Node, find all its neighbours in the given Graph
        *
        * @param graph
        *   Weighted Directed Graph
        * @param node
        *   Node to find neighbours of
        * @param ignore
        *   Set of nodes to ignore (do not return these nodes as neighbours)
        * @return
        *   Set of neighbouring Nodes
        */
      def findNeighboursInGraph(
          graph: WDiGraph[String],
          node: Belief,
          ignore: Set[Belief] = Set.empty
      ): Set[Belief] = {
        assert(graph.vertices.contains(node))

        /** For a given Edge, if Self is incident to that edge AND the weight of the edges is larger
          * than 0, return the other Node
          *
          * @param edge
          *   Weighted Directed Edge
          * @param self
          *   Node
          * @param ignore
          *   Set of nodes to ignore (do not return these nodes as neighbours)
          * @return
          *   Th other Node of this edge if Self is incident to the edge and it has weight > 0
          */
        def getNeighbourIfIncident(
            edge: WDiEdge[Belief],
            self: Belief,
            ignore: Set[Belief] = Set.empty
        ): Set[Belief] = {
          if (edge.left == self && edge.weight > 0 && !ignore.contains(edge.right)) Set(edge.right)
          else Set.empty
        }
        // For all edges in the graph, check if node is incident to the edge and what its neighbour would be
        graph.edges.flatMap(getNeighbourIfIncident(_, node, ignore))
      }

      /** Recursive call of getConnected
        *
        * @param graph
        *   Weighted Directed Graph
        * @param node
        *   Node
        * @param connected
        *   Set of Nodes already found to be connected
        * @param queue
        *   List of Nodes still waiting to be explored
        * @return
        *   Set of Nodes connected to Node
        */
      @tailrec
      def getConnectedRecursive(
          graph: WDiGraph[String],
          node: Belief,
          connected: Set[Belief],
          queue: List[Belief]
      ): Set[Belief] = {
        val neighbours = findNeighboursInGraph(graph, node, connected ++ queue.toSet)
        val newQueue   = queue ++ neighbours
        if (queue.isEmpty) connected + node
        else {
          getConnectedRecursive(graph, newQueue.head, connected + node, newQueue.tail)
        }
      }

      val connected: Set[Belief] = Set(startNode) // Usually Node("sourceNode")
      val queue: List[Belief] =
        findNeighboursInGraph(graph, startNode).toList // Neighbours of Node("sourceNode")

      // if startNode has no Neighbours, return it by itself
      if (queue.isEmpty) connected
      // Otherwise, keep searching from its neighbours
      else {
        getConnectedRecursive(graph, queue.head, connected, queue.tail)
      }
    }

    // residualGraph is the graph after running the Edmonds-Karp algorithm on the graph
    val residualGraph: WDiGraph[String] = maxFlow(graph)

    // residualGraph should have only 2 connected components
    val trueComponent: Set[Belief] =
      getConnected(
        residualGraph,
        Node("sourceNode")
      ) // Nodes connected to "sourceNode" are set to True
    val falseComponent: Set[Belief] =
      graph.vertices -- trueComponent // Nodes connected to "targetNode" are set to False

    // Combine the found truth-value assignments
    val assignmentAsMap = trueComponent.map((_, true)).toMap ++ falseComponent.map((_, false)).toMap
    val assignment: TruthValueAssignment = TruthValueAssignment(assignmentAsMap.keySet, assignmentAsMap.toSet)

    // Calculate coherence over the max-flow subgraph
    val tempBeliefNet: BeliefNetwork = new BeliefNetwork(graph, Set.empty)
    val coherenceValue: Double       = tempBeliefNet.coh(assignment)

    // Return the full assignment plus the coherence value
    (assignment, coherenceValue)
  }

}
