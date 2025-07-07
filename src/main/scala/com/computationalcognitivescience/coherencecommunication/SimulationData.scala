package com.computationalcognitivescience.coherencecommunication

case class SimulationData(
    parameters: Parameters,
    conversations: Seq[Seq[TurnData]]
)

object SimulationData