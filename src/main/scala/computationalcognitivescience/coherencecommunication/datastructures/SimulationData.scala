package computationalcognitivescience.coherencecommunication.datastructures

import ConversationData.ConversationData

case class SimulationData(
    parameters: Parameters,
    conversations: Seq[ConversationData]
)

object SimulationData
