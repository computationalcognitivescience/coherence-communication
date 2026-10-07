package computationalcognitivescience.coherencecommunication.coherence.graph

import mathlib.graph.{Graph, Node}
import mathlib.graph.properties.Edge

trait Cut[T, E <: Edge[Node[T]], G <: Graph[T, E]]

