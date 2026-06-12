package computationalcognitivescience.coherencecommunication.datastructures

object ConversationData {
  type ConversationData = Seq[TurnData]

  def apply(conversation: Seq[TurnData]): ConversationData = conversation
}
