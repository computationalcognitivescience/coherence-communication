package com.computationalcognitivescience.coherencecommunication.util

import com.computationalcognitivescience.coherencecommunication.JSON.loadJson
import com.computationalcognitivescience.coherencecommunication.{
  Parameters,
  SimulationData,
  TurnData
}
import os.Path
import purecsv.safe._
import purecsv.unsafe.RecordSplitter.defaultFieldSeparatorStr

object CSV {

  def simulationDataToCVSV(
      data: SimulationData,
      dataFolderPath: Path,
      dataFilename: String
  ): Unit = {
    val outputFile = dataFolderPath / dataFilename.split("\\.").dropRight(1).mkString.concat(".csv")
    if (!os.exists(outputFile)) {
      println(s"Creating new CSV file ${outputFile.toString()}...")
      os.write(
        outputFile,
        FlatData.parameters.mkString(defaultFieldSeparatorStr).concat("\n"),
        createFolders = true
      )
    }

    val flatData: Seq[FlatData] =
      FlatData.perParameterCombination(data.parameters, data.conversations)

    flatData
      .toCSVLines()
      .foreach(line =>
        os.write.append(
          dataFolderPath / dataFilename.split("\\.").dropRight(1).mkString.concat(".csv"),
          line.concat("\n"),
          createFolders = true
        )
      )
  }

  def jsonToCSV(dataFolderPath: Path, dataFilename: String): Unit = {
    println("Loading JSON data...")
    val data: List[SimulationData] = loadJson(dataFolderPath / dataFilename)
    println("Done.")
    println("Writing CSV...")
    data.foreach(simulationDataToCVSV(_, dataFolderPath, dataFilename))
    println("Done.")
  }

  case class FlatData(
      id: Long,
      networkSize: Int,
      networkConstraints: Int,
      networkPCRatio: Double,
      initiatorIntentSize: Int,
      initiatorOwnBeliefsSize: Int,
      responderOwnBeliefsSize: Int,
      nRounds: Int,
      nrOffers: Int,
      asymmetryAllBeliefsFirst: Double,
      asymmetryIntentionBeliefsFirst: Double,
      asymmetryAllBeliefsLast: Double,
      asymmetryIntentionBeliefsLast: Double,
      ownBeliefsOverlap: Double,
      ownBeliefsAsymmetry: Double
  )

  private case object FlatData {
    def parameters: Seq[String] = List(
      "parameterId",
      "networkSize",
      "networkConstraints",
      "networkPCRatio",
      "initiatorIntentSize",
      "initiatorOwnBeliefsSize",
      "responderOwnBeliefsSize",
      "nRounds",
      "nrOffers",
      "asymmetryAllBeliefsFirst",
      "asymmetryIntentionBeliefsFirst",
      "asymmetryAllBeliefsLast",
      "asymmetryIntentionBeliefsLast",
      "ownBeliefsOverlap",
      "ownBeliefsAsymmetry"
    )

    def perParameterCombination(
        parameters: Parameters,
        conversations: Seq[Seq[TurnData]]
    ): Seq[FlatData] = {
      conversations.indices
        .map(conversationIndex => {
          val conversation = conversations(conversationIndex)
          if (conversation.nonEmpty) {
            val firstTurn = conversation.last
            val lastTurn  = conversation.head
            Some(
              FlatData(
                id = parameters.id * conversations.size + conversationIndex,
                networkSize = parameters.beliefNetworkSize,
                networkConstraints = firstTurn.networkConstraints,
                networkPCRatio = firstTurn.networkPCRatio,
                initiatorIntentSize = firstTurn.initiatorIntentSize,
                initiatorOwnBeliefsSize = firstTurn.initiatorState.ownBeliefs.size,
                responderOwnBeliefsSize = firstTurn.responderState.ownBeliefs.size,
                nRounds = conversation.size,
                nrOffers = conversation.count(turn =>
                  turn.restrictedOffer.isDefined && turn.restrictedOffer.get.nonEmpty
                ),
                asymmetryAllBeliefsFirst = firstTurn.allBeliefsAsymmetry,
                asymmetryIntentionBeliefsFirst = firstTurn.intentionBeliefAsymmetry,
                asymmetryAllBeliefsLast = lastTurn.allBeliefsAsymmetry,
                asymmetryIntentionBeliefsLast = lastTurn.intentionBeliefAsymmetry,
                ownBeliefsOverlap = firstTurn.ownBeliefsOverlap,
                ownBeliefsAsymmetry = firstTurn.ownBeliefAsymmetry
              )
            )
          } else None
        })
        .filter(_.isDefined)
        .map(_.get)
    }

    def flattenToCSV(data: List[SimulationData]): List[FlatData] =
      data
        .flatMap(simData =>
          FlatData.perParameterCombination(simData.parameters, simData.conversations)
        )
  }
}
