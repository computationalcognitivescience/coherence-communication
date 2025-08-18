package com.computationalcognitivescience.coherencecommunication

/** Provides classes for different variants of Coherence, based on Thagard & Verbeurgt (1998) and
  * van Rooij (2008).
  *
  * ==Overview==
  * This package contains a base implementation of Coherence in the trait [[BaseBeliefNetwork]].
  * This trait is further implemented in three variants: [[BeliefNetwork]] implements a regular
  * coherence network, [[FoundationalBeliefNetwork]] implements foundational coherence (van Rooij,
  * 2008), and [[BiasedBeliefNetwork]] implements a variant of discriminating coherence (van Rooij,
  * 2008; Blokpoel & van Rooij, 2021-2025).
  *
  * Beliefs are represented by nodes with a string value:
  *
  * {{{
  *   val b = N("a")
  * }}}
  *
  * The networks are represented by weighted undirected graphs with string values:
  *
  * {{{
  *   val graph = mathlib.WUnDiGraph(
  *      vertices = Set(N("a"), N("b")),
  *      edges = Set(N("a") ~ N("b"))
  *   )
  * }}}
  *
  * A truth-value assignment over beliefs is represented in the datastructure
  * [[TruthValueAssignment]], which provides additional utility functions such as similarity, merge,
  * etc.
  *
  * ==Bibliography==
  *
  * Blokpoel, M. & van Rooij, I. (2021-2025). Theoretical modeling for cognitive science and
  * psychology. Retrieved 2025-08-15 from
  * [[https://computationalcognitivescience.github.io/lovelace/]].
  *
  * van Rooij, I. (2008). The Tractable Cognition Thesis. _Cognitive Science: A Multidisciplinary
  * Journal, 32_(6), 939–984. [[https://doi.org/10.1080/03640210801897856]]
  *
  * Thagard, P., & Verbeurgt, K. (1998). Coherence as Constraint Satisfaction. _Cognitive Science,
  * 22_(1), 1–24. [[https://doi.org/10.1207/s15516709cog2201_1]]
  */
package object coherence {}
