package com.computationalcognitivescience.coherencecommunication

import mathlib.set.SetTheory._
import coherence.Belief.Belief
import coherence.{FoundationalBeliefNetwork, TruthValueAssignment}
import mathlib.graph.{WUnDiEdge, WUnDiGraph}

trait Interlocutor {
  val graph: WUnDiGraph[String]
  val negativeConstraints: Set[WUnDiEdge[Belief]]
  val ownBeliefs: TruthValueAssignment
  val sharedBeliefs: TruthValueAssignment = TruthValueAssignment.emtpy
  val previousState: Option[Interlocutor] = None
  val maxUtteranceLength: Option[Int]     = None

  protected val foundationalBeliefNetwork: FoundationalBeliefNetwork = FoundationalBeliefNetwork(
    graph = graph,
    negativeConstraints = negativeConstraints,
    priorBeliefs = ownBeliefs.beliefs \/ sharedBeliefs.beliefs,
    priorBeliefsAssignment = ownBeliefs ++ sharedBeliefs
  )
  lazy val allBeliefs: TruthValueAssignment      = beliefInference()
  lazy val inferredBeliefs: TruthValueAssignment = allBeliefs \ ownBeliefs \ sharedBeliefs
  lazy val coherence: Double                = foundationalBeliefNetwork.coh(allBeliefs)

  /** Computes the belief revision for `ownBeliefs` as defined in <span style="font-variant-caps:
    * normal;">Belief Revision</span>.
    *
    * A truth-value assignment over all beliefs, $T$ that is maximally coherent and conforms to the
    * prior as defined by the set:
    * $$\mathbf{T_{max}}=\textsc{F-Coherence}(G,C^+,C^-,T_{shared}\oplus T_{own}) $$ and is
    * maximally structurally similar to previously inferred truth values $T=\argmax_{T\in
    * \mathbf{T_{max}}}T\sim T_{prev}$.
    *
    * @return
    */
  private def beliefInference(): TruthValueAssignment = {
    val tMax: Set[TruthValueAssignment] = foundationalBeliefNetwork.coherenceSolutions()

    if (previousState.isDefined) {
      val tPrev: TruthValueAssignment = previousState.get.allBeliefs
      tMax.argMax(tva => tva ~ tPrev).random.get
    } else {
      tMax.random.get
    }
  }

  /** Returns a new interlocutor (either [[Initiator]] or [[Responder]]) with the new utterance
    * added to the shared beliefs.
    * @param utterance The beliefs to add to shared beliefs.
    * @return
    */
  protected def addSharedBeliefs(utterance: TruthValueAssignment): Interlocutor

  //
//  protected def compare(
//      a: Node[String],
//      condidateInference: Map[Node[String], Boolean],
//      previousBeliefs: Map[Node[String], Boolean]
//  ): Int =
//    if (condidateInference(a) == previousBeliefs(a)) 1 else 0
//
//  def inferBeliefs(): Map[Node[String], Boolean] = {
////    println("[Interlocutor.inferredBeliefs]")
//    val allPossibleMaximumCoherenceInferences = beliefNetwork.coherenceSolutions()
//    val currentInferredBeliefSet =
//      (beliefNetwork.vertices \ priorBeliefs.keySet) \ communicatedBeliefs.keySet
//
//    if (previousState.isEmpty)
//      allPossibleMaximumCoherenceInferences.random.get
//    else {
//      val overlapPreviousCurrentInferredBeliefs =
//        previousState.get.inferredBeliefs.keySet /\ currentInferredBeliefSet
//
//      // Infer the beliefs that are structurally most similar to the previous inferred beliefs
//      allPossibleMaximumCoherenceInferences
//        .argMax(inference => {
//          sum(
//            overlapPreviousCurrentInferredBeliefs,
//            compare(_, inference, previousState.get.allBeliefTruthValueAssignments)
//          )
//        })
//        .random
//        .get
//    }
//  }
//
//
//  def allBeliefTruthValueAssignments: Map[Node[String], Boolean] =
//    priorBeliefs ++ communicatedBeliefs ++ inferredBeliefs
//
//  val utteranceLengthLimit: Int = {
//    if (maxUtteranceLength.isDefined)
//      maxUtteranceLength.get
//    else beliefNetwork.vertices.size + 1
//  }
//
//  def addCommunicatedBeliefs(utterance: Map[Node[String], Boolean]): Interlocutor

//  /** Calculate structural similarity between the truth-value assignments of two interlocutors for
//    * the given subset.
//    *
//    * @param subset
//    *   Optional (sub)set of beliefs to calculate similarity over. If subset contains a node not in
//    *   any of the belief networks, an error will occur. If left to None (default), it calculates
//    *   the similarity of the entire network.
//    * @return
//    *   The number of beliefs that have the same truth-value assignment.
//    */
//  def structuralSimilarity(
//      that: Interlocutor,
//      subset: Set[Node[String]] = this.beliefNetwork.vertices
//  ): Int = {
////    println("[Interlocutor.structuralSimilarity]")
//    subset.toList
//      .map(belief =>
//        if (
//          this.allBeliefTruthValueAssignments(belief) == that
//            .allBeliefTruthValueAssignments(belief)
//        ) 1
//        else 0
//      )
//      .sum
//  }

//  def toDOTString(
//      id: String = "G",
//      colorMaps: Option[Map[Node[String], List[String]]],
//      maxColors: Option[Int],
//      msg: String = "",
//      xOffset: Int = 0,
//      yOffset: Int = 0
//  ): String = {
//    val orderedVertices = beliefNetwork.vertices.toList.sortBy(_.label)
//    val coordinates: Map[Node[String], (Double, Double)] = orderedVertices
//      .map(vertex => {
//        val index: Int = orderedVertices.indexOf(vertex)
//        val y: Double  = yOffset + 4 * sin(2 * math.Pi / orderedVertices.size * index)
//        val x: Double  = xOffset + 4 * cos(2 * math.Pi / orderedVertices.size * index)
//        vertex -> (x, y)
//      })
//      .toMap
//
//    "graph " + id + " {" +
//      "\nnode[penwidth=2]" +
//      beliefNetwork.vertices
//        .map(vertex => {
//          val position = "pos=\"" + coordinates(vertex)._1 + "," + coordinates(vertex)._2 + "!\""
//          val style = "color=black, style=filled,fillcolor=" +
//            (if (allBeliefTruthValueAssignments(vertex)) "darkolivegreen1" else "coral1")
//          val colors = colorMaps.getOrElse(Map.empty).getOrElse(vertex, List("none"))
//          val colorTable =
//            colors
//              .map(color => s"<td style=\"rounded\" bgcolor=\"$color\">&nbsp;</td>")
//              .mkString("<tr>", "", "") +
//              (for (i <- 0 until (maxColors.getOrElse(1) - colors.size))
//                yield "<td>&nbsp;</td>").mkString("", "", "</tr>")
//          val label = s"<<table border=\"0\" cellborder=\"0\"><tr><td colspan=\"${maxColors
//              .getOrElse(1)}\">${vertex.label}</td></tr>$colorTable</table>>"
////        val label = vertex.label
//          val mainNode =
//            "" + (vertex.label + id) + "[" + position + ",label=" + label + "," + style + "]\n"
//          mainNode
//        })
//        .mkString("\n", "\n", "\n") +
//      "Coh" + id + "[shape=plaintext, pos=\"" + xOffset + "," + (yOffset + 4.5) + "!\", label=\"Coh=" + math
//        .round(10 * beliefNetwork.coh(allBeliefTruthValueAssignments)) / 10.0 + "\"]\n" +
//      "Msg" + id + "[shape=plaintext, pos=\"" + xOffset + "," + (yOffset + 5) + "!\", label=\"" + msg + "\"]" +
//      beliefNetwork.edges
//        .map(edge => {
//          val style = if (edge in beliefNetwork.negativeConstraints) "dashed" else "solid"
//          (edge.left.label + id) + " -- " + (edge.right.label + id) + " [label=<<table border=\"0\" cellborder=\"0\"><tr><td bgcolor=\"white\">" + scala.math
//            .round(edge.weight * 100) / 100.0 + "</td></tr></table>>, penwidth=" + scala.math
//            .round(0.5 + edge.weight * 3) + ", style=\"" + style + "\"]"
//        })
//        .mkString("\n", "\n", "\n") +
//      "}"
//  }
}
