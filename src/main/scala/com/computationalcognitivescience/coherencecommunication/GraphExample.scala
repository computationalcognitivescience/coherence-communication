package com.computationalcognitivescience.coherencecommunication

import mathlib.graph._
import mathlib.graph.GraphImplicits._
object GraphExample {

  def main(args: Array[String]): Unit = {
    val network = WUnDiGraph.random(10, 0.5)
    println("Vertices")
    network.vertices.foreach(println)
    println("Edges")
    network.edges.foreach(println)


  }

}
