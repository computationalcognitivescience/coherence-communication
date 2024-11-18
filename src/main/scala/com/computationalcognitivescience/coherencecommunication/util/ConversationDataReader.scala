package com.computationalcognitivescience.coherencecommunication.util

import com.computationalcognitivescience.coherencecommunication.coherence.FoundationalBeliefNetwork
import com.computationalcognitivescience.coherencecommunication.{
  ConversationData,
  Initiator,
  Parameters,
  PicklableConversationData
}
import mathlib.graph.{Node, WUnDiEdge, WUnDiGraph}
import mathlib.set.SetTheory._
import os.Path

case class ConversationDataReader(file: Path) {

  def convertPickledData(
      pd: (Parameters, Seq[PicklableConversationData])
  ): (Parameters, Seq[ConversationData]) = {
    ???
  }


  def readAll(): Map[Parameters, Seq[ConversationData]] = {
    val jsonString = os.read(file)
    val unpickledData: Seq[(Parameters, Seq[PicklableConversationData])] =
      upickle.default.read[Seq[(Parameters, Seq[PicklableConversationData])]](jsonString)

    unpickledData.map(convertPickledData).toMap
  }
}

object ConversationDataReader {
  def main(args: Array[String]): Unit = {
    val cdr = ConversationDataReader(os.pwd / "output" / "out1730894954.json")

    val data = cdr.readAll()

    println(data.last)
  }
}
