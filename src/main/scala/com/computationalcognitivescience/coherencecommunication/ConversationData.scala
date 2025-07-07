package com.computationalcognitivescience.coherencecommunication

object ConversationData {
  type ConversationData = Seq[TurnData]

  def apply(conversation: Seq[TurnData]): ConversationData = conversation
}
