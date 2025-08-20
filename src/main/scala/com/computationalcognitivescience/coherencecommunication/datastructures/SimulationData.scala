package com.computationalcognitivescience.coherencecommunication.datastructures

import com.computationalcognitivescience.coherencecommunication.datastructures.ConversationData.ConversationData

case class SimulationData(
    parameters: Parameters,
    conversations: Seq[ConversationData]
)

object SimulationData
