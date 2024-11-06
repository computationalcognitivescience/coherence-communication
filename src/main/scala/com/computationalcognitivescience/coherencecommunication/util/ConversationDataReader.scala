package com.computationalcognitivescience.coherencecommunication.util

import com.computationalcognitivescience.coherencecommunication.{
  ConversationData,
  Parameters,
  PicklableConversationData
}
import os.Path

case class ConversationDataReader(file: Path) {
  def readAll(): Seq[(Parameters, Seq[PicklableConversationData])] = {
    val jsonString = os.read(file)
    upickle.default.read[Seq[(Parameters, Seq[PicklableConversationData])]](jsonString)
  }
}

object ConversationDataReader {
  def main(args: Array[String]): Unit = {
    val cdr = ConversationDataReader(os.pwd / "output" / "out1730894954.json")

    val data = cdr.readAll()

    println(data.last)
  }
}
