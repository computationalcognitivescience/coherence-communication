package com.computationalcognitivescience.coherencecommunication

import io.circe.Encoder
import io.circe.generic.auto._
import io.circe.parser._
import io.circe.syntax._
import os.Path

object Analyses {

  private def loadJson(filePath: Path): List[SimulationData] = {
    val jsonDecoding = decode[List[SimulationData]](os.read.lines(filePath).mkString).toOption
    if(jsonDecoding.isEmpty) List.empty[SimulationData]
    else jsonDecoding.get
  }



  def main(args: Array[String]): Unit = {
    println("Loading...")
    val data: List[SimulationData] = loadJson(os.pwd / "output" / "out1751625394.json")
    println(data.head)
  }

  }
