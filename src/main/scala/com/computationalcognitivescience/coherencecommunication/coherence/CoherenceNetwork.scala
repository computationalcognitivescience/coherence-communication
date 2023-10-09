package com.computationalcognitivescience.coherencecommunication.coherence

import mathlib.set.SetTheory._
import mathlib.graph.{Node, WUnDiEdge, WUnDiGraph}

class CoherenceNetwork(vertices: Set[Node[String]],
                       positiveConstraints: Set[WUnDiEdge[Node[String]]],
                       negativeConstraints: Set[WUnDiEdge[Node[String]]]
                      ) extends WUnDiGraph[String](vertices, positiveConstraints \/ negativeConstraints) {
    // check if no overlap between pos and neg constraintsK
}
