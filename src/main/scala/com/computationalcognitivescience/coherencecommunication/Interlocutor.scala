package com.computationalcognitivescience.coherencecommunication

import mathlib.set.SetTheory._
import mathlib.graph._
import coherence.{BeliefNetwork, FoundationalBeliefNetwork}

import scala.math.{cos, sin}

abstract class Interlocutor(
    val beliefNetwork: FoundationalBeliefNetwork,
    val priorBeliefs: Map[Node[String], Boolean],
    val previousState: Option[Interlocutor] = None,
    val communicatedBeliefs: Map[Node[String], Boolean] = Map.empty,
    maxUtteranceLength: Option[Int] = None
) {

  protected def compare(
      a: Node[String],
      condidateInference: Map[Node[String], Boolean],
      previousBeliefs: Map[Node[String], Boolean]
  ): Int =
    if (condidateInference(a) == previousBeliefs(a)) 1 else 0

  def inferBeliefs(): Map[Node[String], Boolean] = {
//    println("[Interlocutor.inferredBeliefs]")
    val allPossibleMaximumCoherenceInferences = beliefNetwork.coherenceSolutions()
    val currentInferredBeliefSet =
      (beliefNetwork.vertices \ priorBeliefs.keySet) \ communicatedBeliefs.keySet

    if (previousState.isEmpty)
      allPossibleMaximumCoherenceInferences.random.get
    else {
      val overlapPreviousCurrentInferredBeliefs =
        previousState.get.inferredBeliefs.keySet /\ currentInferredBeliefSet

      // Infer the beliefs that are structurally most similar to the previous inferred beliefs
      allPossibleMaximumCoherenceInferences
        .argMax(inference => {
          sum(
            overlapPreviousCurrentInferredBeliefs,
            compare(_, inference, previousState.get.allBeliefTruthValueAssignments)
          )
        })
        .random
        .get
    }
  }

  val inferredBeliefs: Map[Node[String], Boolean] = inferBeliefs()

  def allBeliefTruthValueAssignments: Map[Node[String], Boolean] =
    priorBeliefs ++ communicatedBeliefs ++ inferredBeliefs

  val utteranceLengthLimit: Int = {
    if (maxUtteranceLength.isDefined)
      maxUtteranceLength.get
    else beliefNetwork.vertices.size + 1
  }

  def addCommunicatedBeliefs(utterance: Map[Node[String], Boolean]): Interlocutor

  /** Calculate structural similarity between the truth-value assignments of two interlocutors for
    * the given subset.
    *
    * @param subset
    *   Optional (sub)set of beliefs to calculate similarity over. If subset contains a node not in
    *   any of the belief networks, an error will occur. If left to None (default), it calculates
    *   the similarity of the entire network.
    * @return
    *   The number of beliefs that have the same truth-value assignment.
    */
  def structuralSimilarity(
      that: Interlocutor,
      subset: Set[Node[String]] = this.beliefNetwork.vertices
  ): Int = {
//    println("[Interlocutor.structuralSimilarity]")
    subset.toList
      .map(belief =>
        if (
          this.allBeliefTruthValueAssignments(belief) == that
            .allBeliefTruthValueAssignments(belief)
        ) 1
        else 0
      )
      .sum
  }

  def toDOTString(
      id: String = "G",
      colorMaps: Option[Map[Node[String], List[String]]],
      maxColors: Option[Int],
      msg: String = "",
      xOffset: Int = 0,
      yOffset: Int = 0
  ): String = {
    val orderedVertices = beliefNetwork.vertices.toList.sortBy(_.label)
    val coordinates: Map[Node[String], (Double, Double)] = orderedVertices
      .map(vertex => {
        val index: Int = orderedVertices.indexOf(vertex)
        val y: Double  = yOffset + 4 * sin(2 * math.Pi / orderedVertices.size * index)
        val x: Double  = xOffset + 4 * cos(2 * math.Pi / orderedVertices.size * index)
        vertex -> (x, y)
      })
      .toMap

    "graph " + id + " {" +
      "\nnode[penwidth=2]" +
      beliefNetwork.vertices
        .map(vertex => {
          val position = "pos=\"" + coordinates(vertex)._1 + "," + coordinates(vertex)._2 + "!\""
          val style = "color=black, style=filled,fillcolor=" +
            (if (allBeliefTruthValueAssignments(vertex)) "darkolivegreen1" else "coral1")
          val colors = colorMaps.getOrElse(Map.empty).getOrElse(vertex, List("none"))
          val colorTable =
            colors
              .map(color => s"<td style=\"rounded\" bgcolor=\"$color\">&nbsp;</td>")
              .mkString("<tr>", "", "") +
              (for (i <- 0 until (maxColors.getOrElse(1) - colors.size))
                yield "<td>&nbsp;</td>").mkString("", "", "</tr>")
          val label = s"<<table border=\"0\" cellborder=\"0\"><tr><td colspan=\"${maxColors
              .getOrElse(1)}\">${vertex.label}</td></tr>$colorTable</table>>"
//        val label = vertex.label
          val mainNode =
            "" + (vertex.label + id) + "[" + position + ",label=" + label + "," + style + "]\n"
          mainNode
        })
        .mkString("\n", "\n", "\n") +
      "Coh" + id + "[shape=plaintext, pos=\"" + xOffset + "," + (yOffset + 4.5) + "!\", label=\"Coh=" + math
        .round(10 * beliefNetwork.coh(allBeliefTruthValueAssignments)) / 10.0 + "\"]\n" +
      "Msg" + id + "[shape=plaintext, pos=\"" + xOffset + "," + (yOffset + 5) + "!\", label=\"" + msg + "\"]" +
      beliefNetwork.edges
        .map(edge => {
          val style = if (edge in beliefNetwork.negativeConstraints) "dashed" else "solid"
          (edge.left.label + id) + " -- " + (edge.right.label + id) + " [label=<<table border=\"0\" cellborder=\"0\"><tr><td bgcolor=\"white\">" + scala.math
            .round(edge.weight * 100) / 100.0 + "</td></tr></table>>, penwidth=" + scala.math
            .round(0.5 + edge.weight * 3) + ", style=\"" + style + "\"]"
        })
        .mkString("\n", "\n", "\n") +
      "}"
  }
}
