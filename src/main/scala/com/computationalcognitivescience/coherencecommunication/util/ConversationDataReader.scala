package com.computationalcognitivescience.coherencecommunication.util

import com.computationalcognitivescience.coherencecommunication.coherence.FoundationalBeliefNetwork
import com.computationalcognitivescience.coherencecommunication.{ConversationData, Initiator, Parameters, PicklableConversationData, Responder}
import mathlib.graph.{Node, WUnDiGraph}
import mathlib.set.SetTheory._
import os.Path

case class ConversationDataReader(file: Path) {

  private def unpickleTruthValueAssignment(
    pickledTruthValueAssignment: Map[String, Boolean]
  ): Map[Node[String], Boolean] =
    pickledTruthValueAssignment.map(b => Node(b._1) -> b._2)

  def convertPickledData(
      parameters: Parameters,
      pcds: Seq[PicklableConversationData]
  ): (Parameters, Seq[ConversationData]) = {
    val cds = pcds.map(pcd => {
      ConversationData(
        initiatorState = Initiator(
          beliefNetwork = new FoundationalBeliefNetwork(
            graph = new WUnDiGraph[String](
              pcd.beliefs.map(Node(_)),
              pcd.positiveConstraints.map(_.toWUnDiEdge) \/
              pcd.negativeConstraints.map(_.toWUnDiEdge)
            ),
            negativeConstraints   = pcd.negativeConstraints.map(_.toWUnDiEdge),
            priorBeliefs = pcd.initiatorPrior.keySet.map(Node(_)) \/ pcd.communicatedBeliefs.keySet.map(Node(_)) \/ pcd.initiatorIntent.keySet.map(Node(_)),
            priorBeliefsAssignment = unpickleTruthValueAssignment(pcd.initiatorPrior) ++ unpickleTruthValueAssignment(pcd.communicatedBeliefs) ++ unpickleTruthValueAssignment(pcd.initiatorIntent)
          ),
          priorBeliefs = unpickleTruthValueAssignment(pcd.initiatorPrior),
          communicativeIntent = unpickleTruthValueAssignment(pcd.initiatorIntent),
          previousState = None,
          sharedBeliefs = unpickleTruthValueAssignment(pcd.communicatedBeliefs),
          presetInferredBeliefs = Some(unpickleTruthValueAssignment(pcd.initiatorInferred)),
          maxUtteranceLength = Some(parameters.maxUtteranceLength)
      ),
      responderState = Responder(
        beliefNetwork = new FoundationalBeliefNetwork(
          graph = new WUnDiGraph[String](
            pcd.beliefs.map(Node(_)),
            pcd.positiveConstraints.map(_.toWUnDiEdge) \/
              pcd.negativeConstraints.map(_.toWUnDiEdge)
          ),
          negativeConstraints = pcd.negativeConstraints.map(_.toWUnDiEdge),
          priorBeliefs = pcd.responderPrior.keySet.map(Node(_)) \/ pcd.communicatedBeliefs.keySet.map(Node(_)),
          priorBeliefsAssignment = unpickleTruthValueAssignment(pcd.responderPrior) ++ unpickleTruthValueAssignment(pcd.communicatedBeliefs)
        ),
        priorBeliefs = pcd.responderPrior.map(b => Node(b._1) -> b._2),
        previousState = None,
        sharedBeliefs = pcd.communicatedBeliefs.map(b => Node(b._1) -> b._2),
        presetInferredBeliefs = Some(unpickleTruthValueAssignment(pcd.responderInferred)),
        maxUtteranceLength = Some(parameters.maxUtteranceLength)
      ),
      round = pcd.round,
      utterance = Some(unpickleTruthValueAssignment(pcd.utterance.getOrElse(Map.empty))),
      sharedBeliefs = unpickleTruthValueAssignment(pcd.communicatedBeliefs),
      restrictedOffer = Some(unpickleTruthValueAssignment(pcd.repair.getOrElse(Map.empty))),
      utteranceLengthsInitiator = pcd.utteranceLengthsInitiator,
      repairLengthsResponder = pcd.repairLengthsResponder
    )
  })
    parameters -> cds
  }

  def readAll(): Map[Parameters, Seq[ConversationData]] = {
    val jsonString = os.read(file)
    val unpickledData: Seq[(Parameters, Seq[PicklableConversationData])] =
      upickle.default.read[Seq[(Parameters, Seq[PicklableConversationData])]](jsonString)

    unpickledData.map((convertPickledData _).tupled).toMap
  }
}

object ConversationDataReader {
  def main(args: Array[String]): Unit = {
    val cdr = ConversationDataReader(os.pwd / "output" / "out1732536588.json")

    val data = cdr.readAll()

    println(data.last)
  }
}
