package com.computationalcognitivescience.coherencecommunication.coherence

import mathlib.graph.GraphImplicits.{EdgeImpl2, WUnDiEdgeImpl}
import mathlib.graph._
import mathlib.set.SetTheory._

import scala.annotation.tailrec
import scala.util.Random

class BeliefNetwork(
                     val graph: WUnDiGraph[String],
                     val negativeConstraints: Set[WUnDiEdge[Node[String]]]
                   ) extends WUnDiGraph[String](graph.vertices, graph.edges) {

  val positiveConstraints: Set[WUnDiEdge[Node[String]]] = graph.edges \ negativeConstraints

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

  /** Check if in the given truth-value assignment a positive constraint is determined
   *
   * @param assignment
   * The truth-value assignment over vertices
   * @param edge
   * A constraint
   * @return
   * True if both endpoints of the edge have been assigned, false otherwise
   */
  protected def isDeterminedConstraint(assignment: Map[Node[String], Boolean])(
    edge: WUnDiEdge[Node[String]]
  ): Boolean = assignment.keySet.contains(edge.left) && assignment.keySet.contains(edge.right)

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

    satisfiedPositiveConstraints.toList
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

    satisfiedNegativeConstraints.toList
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

  /** Calculate the optimal truth-value assignment of this BeliefNetwork
   *
   * Based on Blokpoel, M. & van Rooij, I. (2021). Theoretical modeling for cognitive science and
   * psychology Chapter 5
   *
   * @return
   * A truth-value assignment over vertices that results in maximum coherence If multiple maximal
   * truth-value assignments exists, get a random maximal one.
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
   * Set of nodes incident to a negative constraint
   * @return
   * All possible truth value assignments over unassignedMinus
   */
  protected def ac1(
                   unassignedMinus: Set[Node[String]] // All nodes incident to a negative constraint
                 ): Set[Map[Node[String], Boolean]] =
    unassignedMinus.allMappings(Set(true, false))

  /** Given a graph and a truth-value assignment, remove all determined constraints from the graph
   *
   * Remove determined constraints rule If an constraint is determined (i.e. both of its endpoints
   * have a truth-value assignment) then remove it from the graph, if it also satisfied, keep track
   * of the coherence-value that would be acquired with this constraint.
   *
   * @param assignmentSet
   * A truth-value assignment over Nodes
   * @return
   * A tuple containing
   *   1. A Weighted Undirected Graph with determined constraints removed 2. The truth-value
   *      assignment over Nodes 3. The sum coherence value of satisfied constraints
   */
  protected def ac2(
                   assignmentSet: Set[Map[Node[String], Boolean]]
                 ): (WUnDiGraph[String], Set[(Map[Node[String], Boolean], Double)]) = {

    // Because the set of *determined* constraints (positive or negative) is the same for all truth-value assignments
    // We can take a any truth-value assignment to determine the determined constraints
    val randomAssignment: Map[Node[String], Boolean] = assignmentSet.random.get

    // For positive constraints we need to check if they've been determined already
    val dPosConstraints: Set[WUnDiEdge[Node[String]]] =
      positiveConstraints.filter(isDeterminedConstraint(randomAssignment))

    // Because of the application of AC1 we know all negative constraints are already determined
    val dNegConstraints: Set[WUnDiEdge[Node[String]]] = negativeConstraints

    // For a set of determined positive constraints, get the coherence value
    def cohDPlus(
                  edgeSet: Set[WUnDiEdge[Node[String]]],
                  assignment: Map[Node[String], Boolean]
                ): Double = {
      val satisfiedPositiveConstraints: Set[WUnDiEdge[Node[String]]] =
        edgeSet.filter(isSatisfiedPositiveConstraint(assignment))

      satisfiedPositiveConstraints.toList
        .map((edge: WUnDiEdge[Node[String]]) => edge.weight) // Get weights
        .sum // Sum weights
    }

    // For a set of determined negative constraints, get the coherence value

    /** Auxiliary function for calculating the coherence value over determined constraints
     *
     * @param edgeSet
     * The set of edges that are determined
     * @param assignment
     * Truth-value assignment over Nodes
     * @return
     * The sum coherence value over satisfied determined constraints
     */
    def cohDMin(
                 edgeSet: Set[WUnDiEdge[Node[String]]],
                 assignment: Map[Node[String], Boolean]
               ): Double = {
      val satisfiedNegativeConstraints: Set[WUnDiEdge[Node[String]]] =
        edgeSet.filter(isSatisfiedNegativeConstraint(assignment))

      satisfiedNegativeConstraints.toList
        .map((edge: WUnDiEdge[Node[String]]) => edge.weight) // Get weights
        .sum // Sum weights
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
    val graphPrime: WUnDiGraph[String] = WUnDiGraph(vertices, edgesPrime)

    (graphPrime, assignmentCoherence)
  }

  /** Merge accepted and rejected Nodes
   *
   * Merge Accepted and Rejected vertices rule Merge all determined rejected Nodes into a single
   * {"targetNode"} rejected Node, and all accepted Nodes into a single {"sourceNode"} accepted
   * Node. Edges going to the determined (removed) nodes are also transferred to the new nodes.
   *
   * @param graph
   * Weighted Undirected Graph
   * @param assignment
   * Truth-value assignment over Nodes
   * @return
   * Weighted Undirected Graph
   */
  protected def ac3(
                   graph: WUnDiGraph[String],
                   assignment: Map[Node[String], Boolean]
                 ): WUnDiGraph[String] = {
    val acceptedNode: Node[String] = Node("sourceNode")
    val rejectedNode: Node[String] = Node("targetNode")

    /** Divides edges into sets encoding incidence to an accepted Node, incidence to rejected Node,
     * or incidence to neither (both ends unassigned). ASSUMPTION: Input graph has all determined
     * edges removed (i.e. edges that have both endpoints be assigned)
     *
     * @param graph
     * Weighted Undirected Graph
     * @param assignment
     * Truth-value assignment over Nodes
     * @return
     * a 3-tuple of sets of edges
     *   1. edges incident to an accepted Node 2. edges incident to an rejected Node 3. edges
     *      incident to neither
     */
    def sortIncidentEdges(graph: WUnDiGraph[String], assignment: Map[Node[String], Boolean]): (
      Set[WUnDiEdge[Node[String]]],
        Set[WUnDiEdge[Node[String]]],
        Set[WUnDiEdge[Node[String]]]
      ) = {
      val edgeList: List[WUnDiEdge[Node[String]]] = graph.edges.toList
      val incidentToA: Set[WUnDiEdge[Node[String]]] = Set.empty
      val incidentToR: Set[WUnDiEdge[Node[String]]] = Set.empty
      val notIncident: Set[WUnDiEdge[Node[String]]] = Set.empty

      /** Recursive call of sortIncidentEdges
       *
       * @param edgeList
       * List of edges left to consider
       * @param incidentToA
       * Set of edges incident to an accepted Node (so far)
       * @param incidentToR
       * Set of edges incident to an rejected Node (so far)
       * @param notIncident
       * Set of edges incident to neither (so far)
       * @return
       * a 3-tuple of sets of edges
       *   1. edges incident to an accepted Node 2. edges incident to an rejected Node 3. edges
       *      incident to neither
       */
      @tailrec
      def sortIncidentEdgesRecursive(
                                      edgeList: List[WUnDiEdge[Node[String]]],
                                      incidentToA: Set[WUnDiEdge[Node[String]]],
                                      incidentToR: Set[WUnDiEdge[Node[String]]],
                                      notIncident: Set[WUnDiEdge[Node[String]]]
                                    ): (
        Set[WUnDiEdge[Node[String]]],
          Set[WUnDiEdge[Node[String]]],
          Set[WUnDiEdge[Node[String]]]
        ) = {
        // If there are no more edges to sort
        if (edgeList.isEmpty) (incidentToA, incidentToR, notIncident)
        else {
          // If the edgeList is non-empty
          val edge: WUnDiEdge[Node[String]] = edgeList.head
          // if the left Node is assigned
          if (assignment.contains(edge.left)) {
            if (assignment(edge.left)) // if the left Node is true
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
            if (assignment(edge.right)) // if the right Node is true
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
        val edge: WUnDiEdge[Node[String]] = edgeList.head
        if (assignment.contains(edge.left)) {
          if (assignment(edge.left))
            sortIncidentEdgesRecursive(edgeList.tail, incidentToA + edge, incidentToR, notIncident)
          else sortIncidentEdgesRecursive(edgeList.tail, incidentToA, incidentToR + edge, notIncident)
        } else if (assignment.contains(edge.right)) {
          if (assignment(edge.right))
            sortIncidentEdgesRecursive(edgeList.tail, incidentToA + edge, incidentToR, notIncident)
          else sortIncidentEdgesRecursive(edgeList.tail, incidentToA, incidentToR + edge, notIncident)
        } else sortIncidentEdgesRecursive(edgeList.tail, incidentToA, incidentToR, notIncident + edge)
      }
    }

    // Collect all constraints that are incident to an accepted node
    val (constraintsAPrime, constraintsRPrime, notIncidentConstraints): (
      Set[WUnDiEdge[Node[String]]],
        Set[WUnDiEdge[Node[String]]],
        Set[WUnDiEdge[Node[String]]]
      ) =
      sortIncidentEdges(graph, assignment)

    /** Replace the given edge with an edge connected to the special accepted Node {"sourceNode"}
     *
     * @param assignment
     * Truth-Value assignment over Nodes
     * @param edge
     * Weighted Undirected Edge
     * @return
     * Weighted Undirected Edge connected to the special accepted Node
     */
    def replaceConstraintAPrime(
                                 assignment: Map[Node[String], Boolean],
                                 edge: WUnDiEdge[Node[String]]
                               ): WUnDiEdge[Node[String]] = {
      if (assignment.contains(edge.left)) {
        WUnDiEdge(left = edge.right, right = acceptedNode, weight = edge.weight)
      } else {
        WUnDiEdge(left = edge.left, right = acceptedNode, weight = edge.weight)
      }
    }

    /** Replace the given edge with an edge connected to the special rejected Node {"targetNode"}
     *
     * @param assignment
     * Truth-Value assignment over Nodes
     * @param edge
     * Weighted Undirected Edge
     * @return
     * Weighted Undirected Edge connected to the special rejected Node
     */
    def replaceConstraintRPrime(
                                 assignment: Map[Node[String], Boolean],
                                 edge: WUnDiEdge[Node[String]]
                               ): WUnDiEdge[Node[String]] = {
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
     * List of edges
     * @param targetNode
     * Node that all edges connect to
     * @return
     * Set of edges
     */
    def combineDuplicateEdges(
                               edgeList: List[WUnDiEdge[Node[String]]],
                               targetNode: Node[String]
                             ): Set[WUnDiEdge[Node[String]]] = {

      /** Recursive call of combineDuplicateEdges
       *
       * @param edgeList
       * list of edges
       * @param weightMap
       * Map of Nodes to (found) edge weights
       * @param targetNode
       * Node that all edges connect to
       * @return
       * Set of edges
       */
      @tailrec
      def combineDuplicateEdgesRecursive(
                                          edgeList: List[WUnDiEdge[Node[String]]],
                                          weightMap: Map[Node[String], Double],
                                          targetNode: Node[String]
                                        ): Set[WUnDiEdge[Node[String]]] = {
        if (edgeList.isEmpty) {
          weightMap
            .map((nodeWeightPair: (Node[String], Double)) =>
              WUnDiEdge(nodeWeightPair._1, targetNode, nodeWeightPair._2)
            )
            .toSet
        } else {
          val edge: WUnDiEdge[Node[String]] = edgeList.head
          if (weightMap.contains(edge.left)) {
            val newMap: Map[Node[String], Double] =
              weightMap + (edge.left -> (weightMap(edge.left) + edge.weight))
            combineDuplicateEdgesRecursive(edgeList.tail, newMap, targetNode)
          } else {
            val newMap: Map[Node[String], Double] = weightMap + (edge.left -> edge.weight)
            combineDuplicateEdgesRecursive(edgeList.tail, newMap, targetNode)
          }
        }
      }

      val weightMap: Map[Node[String], Double] = Map.empty
      if (edgeList.isEmpty) Set.empty
      else {
        val edge: WUnDiEdge[Node[String]] = edgeList.head
        val newMap: Map[Node[String], Double] = weightMap + (edge.left -> edge.weight)
        combineDuplicateEdgesRecursive(edgeList.tail, newMap, targetNode)
      }
    }

    // Replace a constraint from A-Prime with a new constraint in A-Star (A-star is a subset of P' x {a})
    val newConstraintsA: Set[WUnDiEdge[Node[String]]] = combineDuplicateEdges(
      constraintsAPrime.toList.map(replaceConstraintAPrime(assignment, _)),
      acceptedNode
    )
    // Replace a constraint from R-Prime with a new constraint in R-Star (R-star is a subset of P' x {r})
    val newConstraintsR: Set[WUnDiEdge[Node[String]]] = combineDuplicateEdges(
      constraintsRPrime.toList.map(replaceConstraintRPrime(assignment, _)),
      rejectedNode
    )

    // The new graph only has the old unassigned nodes plus the "sourceNode" and "targetNode" nodes
    val newNodes: Set[Node[String]] =
      graph.vertices -- assignment.keySet ++ Set(acceptedNode, rejectedNode)

    // Replace all constraints that were incident to A' and R' with their replacing constraints connecting to 'a' and 'r'.
    val newConstraints: Set[WUnDiEdge[Node[String]]] =
      notIncidentConstraints ++ newConstraintsA ++ newConstraintsR

    WUnDiGraph(newNodes, newConstraints)
  }

  /** {C-}-FPT algorithm for coherence as presented by van Rooij (1998) {C-} representing the number
   * of negatively constrained edges
   *
   * @return
   * A truth-value assignment over vertices that results in maximum coherence If multiple maximal
   * truth-value assignments exists, get a random maximal one.
   */
  def cMinusCoherence(): Map[Node[String], Boolean] = {

    // Get all vertices incident to a negative constraint
    val unassignedMinus: Set[Node[String]] = negativeConstraints.flatMap(e => Set(e.left, e.right))

    // Apply AC1 exhaustively
    // Effectively: Get all possible truth-value assignments over the vertices incident to a negative constraint
    val assignmentMinusSet: Set[Map[Node[String], Boolean]] = ac1(unassignedMinus)

    // Apply AC2 where possible
    // For each truth-value assignment:
    // create a new graph wherein all edges that have a pre-determined truth-value assignment are removed
    val (graphPrime, assignmentCoherence)
    : (WUnDiGraph[String], Set[(Map[Node[String], Boolean], Double)]) = ac2(assignmentMinusSet)

    // Apply AC3 where possible
    // Remove all nodes that have a pre-assigned truth-value assignment and replace them with a single true node and a single false node
    // with each of the edges that went to a removed node being replace with an edge with the same weight going to the single true/false nodes.
    val maxFlowGraphs: Set[
      (
        WUnDiGraph[String], // Graph
          Map[Node[String], Boolean], // Truth-value assignment of determined Nodes
          Double // Coherence value of determined edges
        )
    ] = // Coherence value of determined constraints
    // Apply AC3 to the graph, pass the truth-value assignment and coherence value as is
    assignmentCoherence.map(
      (instance: (
        Map[Node[String], Boolean], // Truth-value assignment of determined Nodes
          Double
        )) => // Coherence value of determined constraints
        (ac3(graphPrime, instance._1), instance._1, instance._2)
    )

    /** Combine the results of getPartition (which applies maxFlow to maxFlowGraph) with
     * pre-determined assignments and coherence values
     *
     * @param maxFlowGraph
     * Weighted Undirected Graph (representing the belief network after AC3 has been applied)
     * @param predeterminedAssignment
     * Truth-value assignment of determined Nodes
     * @param predeterminedCoherence
     * Coherence value of determined edges
     * @return
     * Tuple of (Truth-value assignment, Coherence-value)
     */
    def combinePartitionWithPredetermined(
                                           maxFlowGraph: WUnDiGraph[String],
                                           predeterminedAssignment: Map[Node[String], Boolean],
                                           predeterminedCoherence: Double
                                         ): (Map[Node[String], Boolean], Double) = {
      val (assignment: Map[Node[String], Boolean], coherenceValue: Double) = getPartition(
        maxFlowGraph
      )
      (
        assignment ++ predeterminedAssignment - Node("sourceNode") - Node("targetNode"),
        coherenceValue + predeterminedCoherence
      )
    }

    val partitionCoherenceTuples: Set[(Map[Node[String], Boolean], Double)] =
      maxFlowGraphs.map(instance =>
        combinePartitionWithPredetermined(instance._1, instance._2, instance._3)
      )

    // Get the partition with the highest coherence value
    partitionCoherenceTuples.argMax((e: (Map[Node[String], Boolean], Double)) => e._2).map(_._1).random.get
  }

  /** Perform the Edmonds-Karp algorithm on the given graph
   *
   * @param graph
   * Weighted Undirected Graph with exactly 1 Node {"sourceNode"} (source node) and 1 Node
   * {"targetNode"} target Node
   * @return
   * Final residual Graph (Weighted Directed graph)
   */
  protected def maxFlow(graph: WUnDiGraph[String]): WDiGraph[String] = {
    val sourceNode: Node[String] = Node("sourceNode")
    val targetNode: Node[String] = Node("targetNode")

    /** Given an Weighted Undirected Edge, generate two Weighted Directed Edges
     *
     * @param edge
     * Weighted Undirected Edge
     * @return
     * A Set of two Weighted Directed Edges
     */
    def createDirectedEdges(
                             edge: WUnDiEdge[Node[String]]
                           ): Set[WDiEdge[Node[String]]] = {
      val firstEdge = WDiEdge(edge.left, edge.right, edge.weight)
      val secondEdge = WDiEdge(edge.right, edge.left, edge.weight)
      Set(firstEdge, secondEdge)
    }

    // Create directed graph
    val edges: Set[WDiEdge[Node[String]]] = graph.edges.flatMap(createDirectedEdges)
    val dirGraph: WDiGraph[String] = WDiGraph(graph.vertices, edges)

    /** Recursively finds the augmenting path through the given Weighted Directed Graph and updates
     * the graph by updating edges
     *
     * @param graph
     * Weighted Directed Graph
     * @param aList
     * Adjacency list
     * @param sourceNode
     * Start Node of the path
     * @param targetNode
     * End Node of the path
     * @return
     * Weighted Directed Graph
     */
    @tailrec
    def findAugmentingPathRecursive(
                                     graph: WDiGraph[String],
                                     aList: Map[Node[String], Set[NodeWeightPair[String]]],
                                     sourceNode: Node[String] = sourceNode,
                                     targetNode: Node[String] = targetNode
                                   ): WDiGraph[String] = {
      // Find path from a to r
      val augmentingPath: List[WDiEdge[Node[String]]] = bfs(graph, sourceNode, targetNode, aList)
      if (augmentingPath.isEmpty) graph
      else {
        // TODO: Implement some way to make this faster (A trait of network that maps a node 2-tuple to an edge?)
        // TODO: or perhaps a map from edge to edge that just maps to its reverse counterpart?
        // Get reverse path
        val reversePath: List[WDiEdge[Node[String]]] = {
          // Check for each edge if its endpoints are part of the nodes in the path
          val reversePathNodes: List[Node[String]] = Range(augmentingPath.size - 1, 0, -1).inclusive
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
         * adjacency list
         * @param edgeList
         * list of updated edges
         * @return
         * updated adjacency list
         */
        @tailrec
        def updateAdjacencyList(
                                 aList: Map[Node[String], Set[NodeWeightPair[String]]],
                                 edgeList: List[WDiEdge[Node[String]]]
                               ): Map[Node[String], Set[NodeWeightPair[String]]] = {
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
            val newAList: Map[Node[String], Set[NodeWeightPair[String]]] = aList + (edge.left -> newNeighbours)
            updateAdjacencyList(newAList, edgeList.tail)
          }
        }

        // Adjust all path capacities
        // Reduce weight of forward edges
        val newForwardEdges: List[WDiEdge[Node[String]]] = augmentingPath
          .map((e: WDiEdge[Node[String]]) => WDiEdge(e.left, e.right, e.weight - minCapacity))

        // Increase weight of backward edges
        val newReverseEdges: List[WDiEdge[Node[String]]] = reversePath
          .map((e: WDiEdge[Node[String]]) => WDiEdge(e.left, e.right, e.weight + minCapacity))

        val newEdges: List[WDiEdge[Node[String]]] = newForwardEdges ++ newReverseEdges

        // Update adjacencyList
        val newAList: Map[Node[String], Set[NodeWeightPair[String]]] = updateAdjacencyList(aList, newEdges)

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
   * Weighted Directed Graph
   * @param startNode
   * Start Node of the path
   * @param targetNode
   * End Node of the path
   * @param aList
   * adjacency list
   * @return
   * Path of edges
   */
  private def bfs(
                   graph: WDiGraph[String],
                   startNode: Node[String],
                   targetNode: Node[String],
                   aList: Map[Node[String], Set[NodeWeightPair[String]]]
                 ): List[WDiEdge[Node[String]]] = {
    assert(graph.vertices.contains(startNode))
    assert(graph.vertices.contains(targetNode))

    /** Search through adjacency list to construct edge with the appropriate weight
     *
     * @param left
     * Left Node
     * @param right
     * Right Node
     * @param aList
     * Adjacency list
     * @return
     * Weighted Directed Edge
     */
    def getEdgeFromNodes(
                          left: Node[String],
                          right: Node[String],
                          aList: Map[Node[String], Set[NodeWeightPair[String]]]
                        ): WDiEdge[Node[String]] = {
      WDiEdge(left, right, aList(left).filter(_.node == right).random.get.weight)
    }

    if (startNode == targetNode) List().empty
    else {

      /** Recursive call of bfs
       *
       * @param graph
       * Weighted Directed Graph
       * @param aList
       * adjacency list
       * @param pathToNode
       * Map of found Nodes the path that was taken to get there
       * @param startNode
       * Start Node of the path
       * @param targetNode
       * End Node of the path
       * @param queue
       * Nodes that still need to be explored
       * @param explored
       * already explored nodes
       * @return
       */
      @tailrec
      def bfsRecursive(
                        graph: WDiGraph[String],
                        aList: Map[Node[String], Set[NodeWeightPair[String]]],
                        pathToNode: Map[Node[String], List[WDiEdge[Node[String]]]],
                        startNode: Node[String],
                        targetNode: Node[String],
                        queue: List[Node[String]],
                        explored: Set[Node[String]]
                      ): List[WDiEdge[Node[String]]] = {

        val neighbours: Set[Node[String]] = aList(startNode)
          .filter(_.weight != 0)
          .map(_.node)
          .filterNot(queue.contains) // Remove nodes that are already in the queue
          .diff(explored) // Remove nodes that have already been explored

        // Update path to found neighbours
        val newPathToNode: Map[Node[String], List[WDiEdge[Node[String]]]] =
          pathToNode ++
            neighbours
              .map(n => n -> (pathToNode(startNode) ++ List(getEdgeFromNodes(startNode, n, aList))))
              .toMap

        // If the target has been found
        if (neighbours.contains(targetNode)) {
          newPathToNode(targetNode)
        } else {
          // We've explored the current node
          val newExplored: Set[Node[String]] = explored + startNode
          // Add neighbours to the end of the queue
          val newQueue: List[Node[String]] =
            queue ++ neighbours.toList

          // If the queue is empty at this point, there is no path from startNode to targetNode
          if (newQueue.isEmpty) {
            List.empty
          } else {
            // Keep searching
            bfsRecursive(
              graph, // network
              aList, // adjacencyList
              newPathToNode, // map from node to path
              newQueue.head, // startNode
              targetNode, // targetNode
              newQueue.tail, // queue
              newExplored // explored
            )
          }
        }
      }

      val neighbours: Set[Node[String]] = aList(startNode).filter(_.weight != 0).map(_.node)
      val pathToNode: Map[Node[String], List[WDiEdge[Node[String]]]] = neighbours
        .map(n =>
          n -> List(WDiEdge(startNode, n, aList(startNode).filter(_.node == n).random.get.weight))
        )
        .toMap
      val explored: Set[Node[String]] = Set(startNode)
      val queue: List[Node[String]] = neighbours.toList

      // No path can be found
      if (queue.isEmpty) List.empty
      // The target has been immediately found
      else if (queue.head == targetNode) pathToNode(targetNode)
      // Continue searching from next node
      else {
        bfsRecursive(
          graph, // network
          aList, // adjacencyList
          pathToNode, // map from node to path
          queue.head, // startNode
          targetNode, // targetNode
          queue.tail, // queue
          explored // explored
        )
      }
    }
  }

  /** Get the truth-value assignment and coherence of this 2-connected component graph, where one
   * components is 'true' and the other is 'false'
   *
   * @param graph
   * Weighted Undirected Graph with exactly 2 connected components
   * @return
   * A truth-value assignment and its coherence value
   */
  protected def getPartition(
                            graph: WUnDiGraph[String]
                          ): (Map[Node[String], Boolean], Double) = {

    /** Get all Nodes connected to the startNode
     *
     * @param graph
     * Weighted Directed Graph
     * @param startNode
     * Starting node from which to determine the connected component (usually Node("sourceNode"))
     * @return
     * A set of nodes connected to the start Node
     */
    def getConnected(graph: WDiGraph[String], startNode: Node[String]): Set[Node[String]] = {

      /** For a given Node, find all its neighbours in the given Graph
       *
       * @param graph
       * Weighted Directed Graph
       * @param node
       * Node to find neighbours of
       * @param ignore
       * Set of nodes to ignore (do not return these nodes as neighbours)
       * @return
       * Set of neighbouring Nodes
       */
      def findNeighboursInGraph(
                                 graph: WDiGraph[String],
                                 node: Node[String],
                                 ignore: Set[Node[String]] = Set.empty
                               ): Set[Node[String]] = {
        assert(graph.vertices.contains(node))

        /** For a given Edge, if Self is incident to that edge AND the weight of the edges is larger
         * than 0, return the other Node
         *
         * @param edge
         * Weighted Directed Edge
         * @param self
         * Node
         * @param ignore
         * Set of nodes to ignore (do not return these nodes as neighbours)
         * @return
         * Th other Node of this edge if Self is incident to the edge and it has weight > 0
         */
        def getNeighbourIfIncident(
                                    edge: WDiEdge[Node[String]],
                                    self: Node[String],
                                    ignore: Set[Node[String]] = Set.empty
                                  ): Set[Node[String]] = {
          if (edge.left == self && edge.weight > 0 && !ignore.contains(edge.right)) Set(edge.right)
          else Set.empty
        }
        // For all edges in the graph, check if node is incident to the edge and what its neighbour would be
        graph.edges.flatMap(getNeighbourIfIncident(_, node, ignore))
      }

      /** Recursive call of getConnected
       *
       * @param graph
       * Weighted Directed Graph
       * @param node
       * Node
       * @param connected
       * Set of Nodes already found to be connected
       * @param queue
       * List of Nodes still waiting to be explored
       * @return
       * Set of Nodes connected to Node
       */
      @tailrec
      def getConnectedRecursive(
                                 graph: WDiGraph[String],
                                 node: Node[String],
                                 connected: Set[Node[String]],
                                 queue: List[Node[String]]
                               ): Set[Node[String]] = {
        val neighbours = findNeighboursInGraph(graph, node, connected ++ queue.toSet)
        val newQueue = queue ++ neighbours
        if (queue.isEmpty) connected + node
        else {
          getConnectedRecursive(graph, newQueue.head, connected + node, newQueue.tail)
        }
      }

      val connected: Set[Node[String]] = Set(startNode) // Usually Node("sourceNode")
      val queue: List[Node[String]] =
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
    val trueComponent: Set[Node[String]] =
      getConnected(
        residualGraph,
        Node("sourceNode")
      ) // Nodes connected to "sourceNode" are set to True
    val falseComponent: Set[Node[String]] =
      graph.vertices -- trueComponent // Nodes connected to "targetNode" are set to False

    // Combine the found truth-value assignments
    val assignment: Map[Node[String], Boolean] =
      trueComponent.map((_, true)).toMap ++ falseComponent.map((_, false)).toMap

    // Calculate coherence over the max-flow subgraph
    val tempBeliefNet: BeliefNetwork = new BeliefNetwork(graph, Set.empty)
    val coherenceValue: Double = tempBeliefNet.coh(assignment)

    // Return the full assignment plus the coherence value
    (assignment, coherenceValue)
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
   * The number of edges in the network
   * @param density
   * The (expected) density of edges in the network
   * @param ratioPosNeg
   * The target ratio of positive edges to negative edges
   * @return
   * A randomly generated BeliefNetwork
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
    val nrNegativeConstraints: Int = (network.edges.size * (1 - ratioPosNeg)).intValue
    val negativeConstraints: Set[WUnDiEdge[Node[String]]] =
      Random.shuffle(network.edges.toList).take(nrNegativeConstraints).toSet

    new BeliefNetwork(network, negativeConstraints)
  }

  /** Generate a belief network from a fixed number of vertices, edges and negative constraints with
   * edges having random (uniformly drawn) weights between 0 and 1.
   *
   * @param size
   * The number of vertices in the network
   * @param nrEdges
   * The number of edges in the network
   * @param nrNegativeEdges
   * The number of negative constraints in the belief network where The number of negative
   * constraints must be smaller than or equal to the total number of edges
   * @return
   * A randomly generated BeliefNetwork
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
