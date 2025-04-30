package com.computationalcognitivescience.coherencecommunication.util

import com.computationalcognitivescience.coherencecommunication.{ConversationData, Parameters}
import purecsv.safe._

case class AnalysisOne(
    networkSize: Int,
    networkConstraints: Int,
    networkPCRatio: Double,
    initiatorIntentSize: Int,
    initiatiorPriorSize: Int,
    responderPriorSize: Int,
    nRounds: Int,
    nRequests: Int,
    asymmetryAllBeliefsFirst: Double,
    asymmetryIntentionBeliefsFirst: Double,
    asymmetryAllBeliefsLast: Double,
    asymmetryIntentionBeliefsLast: Double,
    priorOverlap: Double,
    priorAsymmtery: Double
)

object AnalysisOne {
  def analyzeAll(data: Map[Parameters, Seq[ConversationData]]): Iterable[AnalysisOne] = {
    data
      .filter(row => {
        val conversationDataSorted = row._2.sortBy(_.round)
        val conversationDataLast = conversationDataSorted.last
        !conversationDataLast.initiatorState.communicativeIntent.forall(b => conversationDataLast.communicatedBeliefs.contains(b._1))
      })
      .map(row => {
      val conversationDataSorted = row._2.sortBy(_.round)
      val conversationDataFirst = conversationDataSorted.head
      val conversationDataLast = conversationDataSorted.last
      AnalysisOne(
        networkSize = conversationDataFirst.initiatorState.beliefNetwork.size,
        networkConstraints = conversationDataFirst.initiatorState.beliefNetwork.edges.size,
        networkPCRatio = conversationDataFirst.initiatorState.beliefNetwork.positiveConstraints.size.doubleValue /
          conversationDataFirst.initiatorState.beliefNetwork.negativeConstraints.size.doubleValue,
        initiatorIntentSize = conversationDataFirst.initiatorState.communicativeIntent.keySet.size,
        initiatiorPriorSize = conversationDataFirst.initiatorState.priorBeliefs.keySet.size,
        responderPriorSize = conversationDataFirst.responderState.priorBeliefs.keySet.size,
        nRounds = conversationDataLast.round,
        nRequests = conversationDataSorted.count(_.repair.isDefined),
        asymmetryAllBeliefsFirst = conversationDataFirst.asymmetryAllBeliefs,
        asymmetryIntentionBeliefsFirst = conversationDataFirst.asymmetryIntentionBeliefs,
        asymmetryAllBeliefsLast = conversationDataLast.asymmetryAllBeliefs,
        asymmetryIntentionBeliefsLast = conversationDataLast.asymmetryIntentionBeliefs,
        priorOverlap = conversationDataFirst.priorOverlap,
        priorAsymmtery = conversationDataFirst.priorAsymmetry
      )
    })
  }

  val csvHeaders: String =
    "networkSize,networkConstraints,networkPCRatio,initiatorIntentSize,initiatiorPriorSize,responderPriorSize,nRounds,nRequests,asymmetryAllBeliefsFirst,asymmetryIntentionBeliefsFirst,asymmetryAllBeliefsLast,asymmetryIntentionBeliefsLast,priorOverlap,priorAsymmtery"
  def main(args: Array[String]): Unit = {
    val filename = "out1734105925"
    val cdr = ConversationDataReader(os.pwd / "output" / s"$filename.json")

    val data = cdr.readAll()
    val analyzedData = analyzeAll(data).toSeq
    
    os.write(os.pwd / "output" / s"$filename-analysisOne.csv", csvHeaders + "\n" + analyzedData.toCSV(), createFolders = true)
  }
}
