package com.computationalcognitivescience.coherencecommunication

import com.computationalcognitivescience.coherencecommunication.ConversationData.ConversationData

case class SimulationData(
    parameters: Parameters,
    conversations: Seq[ConversationData]
)

object SimulationData
