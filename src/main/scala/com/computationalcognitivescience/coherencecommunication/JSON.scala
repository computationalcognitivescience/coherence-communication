package com.computationalcognitivescience.coherencecommunication

import io.circe.generic.auto._
import io.circe.parser._
import os.Path

object JSON {

  def loadJson(filePath: Path): List[SimulationData] = {
    val jsonDecoding =  decode[List[SimulationData]](os.read.lines(filePath).mkString).toOption
    if (jsonDecoding.isEmpty) List.empty[SimulationData]
    else jsonDecoding.get
  }

  def main(args: Array[String]): Unit = {
    Simulation.mergeDatafileParts(os.pwd / "output" / "1751822806")
  }

  }
