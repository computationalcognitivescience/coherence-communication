package com.computationalcognitivescience.coherencecommunication.util

import com.computationalcognitivescience.coherencecommunication.datastructures.SimulationData
import io.circe.generic.auto._
import io.circe.parser._
import io.circe.syntax.EncoderOps
import os.Path

object JSON {

  def loadJson(filePath: Path): List[SimulationData] = {
    val jsonDecoding = decode[List[SimulationData]](os.read.lines(filePath).mkString).toOption
    if (jsonDecoding.isEmpty) List.empty[SimulationData]
    else jsonDecoding.get
  }

  def mergeDatafileParts(dataFolderPath: Path): List[SimulationData] = {
    val fileList   = os.list(dataFolderPath).filter(_.toString().contains("part"))
    val outputFile = dataFolderPath / s"complete.json"
    println(s"Merging datafile parts into ${outputFile.toString()}...")
    val data: List[SimulationData] = fileList
      .map(dataFilePath => {
        decode[SimulationData](os.read.lines(dataFilePath).mkString).toOption
      })
      .filter(_.isDefined)
      .map(_.get)
      .toList
    os.write(outputFile, data.asJson.toString(), createFolders = true)
    println("Done.")
    data
  }
}
