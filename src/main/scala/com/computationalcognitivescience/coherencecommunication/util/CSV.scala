package com.computationalcognitivescience.coherencecommunication.util

import com.computationalcognitivescience.coherencecommunication.JSON.loadJson
import com.computationalcognitivescience.coherencecommunication.{
  ConversationData,
  Parameters,
  SimulationData
}
import os.Path
import purecsv.safe._
import purecsv.unsafe.RecordSplitter.defaultFieldSeparatorStr

object CSV {
  def main(args: Array[String]): Unit = {

    val dataFolderPath: Path = os.pwd / "output" / "1751635313"
    val dataFilename: String   = "complete.json"

    println("Loading JSON data...")
    val data: List[SimulationData] = loadJson(dataFolderPath / dataFilename)
    println("Done.")
    println("Flattening data...")
    val flatData: Seq[FlatData] = FlatData.flattenToCSV(data)
    println("Done.")
    println("Writing to CSV...")

    os.write(
      dataFolderPath / dataFilename.split("\\.").dropRight(1).mkString.concat(".csv") ,
      FlatData.parameters.mkString(defaultFieldSeparatorStr).concat("\n"),
      createFolders = true
    )
    flatData
      .toCSVLines()
      .foreach(line =>
        os.write.append(
          dataFolderPath / dataFilename.split("\\.").dropRight(1).mkString.concat(".csv") ,
          line.concat("\n"),
          createFolders = true
        )
      )

    println("Done.")
    println("Finished, exiting.")
  }

  case class FlatData(
      parameterId: Long,
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

    private def perParameterCombination(
        parameters: Parameters,
        conversations: Iterable[ConversationData]
    ): Option[FlatData] = {
      if (conversations.nonEmpty) {
        val firstTurn = conversations.last
        val lastTurn  = conversations.head
        Some(
          FlatData(
            parameterId = parameters.id,
            networkSize = parameters.beliefNetworkSize,
            networkConstraints = firstTurn.networkConstraints,
            networkPCRatio = firstTurn.networkPCRatio,
            initiatorIntentSize = firstTurn.initiatorIntentSize,
            initiatorOwnBeliefsSize = firstTurn.initiatorState.ownBeliefs.size,
            responderOwnBeliefsSize = firstTurn.responderState.ownBeliefs.size,
            nRounds = conversations.size,
            nrOffers = conversations.count(turn =>
              turn.restrictedOffer.isDefined && turn.restrictedOffer.get.nonEmpty
            ),
            asymmetryAllBeliefsFirst = firstTurn.asymmetryAllBeliefs,
            asymmetryIntentionBeliefsFirst = firstTurn.asymmetryIntentionBeliefs,
            asymmetryAllBeliefsLast = lastTurn.asymmetryAllBeliefs,
            asymmetryIntentionBeliefsLast = lastTurn.asymmetryIntentionBeliefs,
            ownBeliefsOverlap = firstTurn.ownBeliefsOverlap,
            ownBeliefsAsymmetry = firstTurn.ownBeliefAsymmetry
          )
        )
      } else None
    }

    def flattenToCSV(data: List[SimulationData]): List[FlatData] =
      data
        .map(simData => FlatData.perParameterCombination(simData.parameters, simData.conversations))
        .filter(_.isDefined)
        .map(_.get)
  }
}

//  val csvHeaders: String =
//    "networkSize,networkConstraints,networkPCRatio,initiatorIntentSize,initiatiorPriorSize,responderPriorSize,nRounds,nRequests,asymmetryAllBeliefsFirst,asymmetryIntentionBeliefsFirst,asymmetryAllBeliefsLast,asymmetryIntentionBeliefsLast,priorOverlap,priorAsymmtery"
//  def main(args: Array[String]): Unit = {
//    val filename = "out1734105925"
//    val cdr      = ConversationDataReader(os.pwd / "output" / s"$filename.json")
//
//    val data         = cdr.readAll()
//    val analyzedData = flattenToCSV(data).toSeq
//
//    os.write(
//      os.pwd / "output" / s"$filename-analysisOne.csv",
//      csvHeaders + "\n" + analyzedData.toCSV(),
//      createFolders = true
//    )
//  }
