package com.computationalcognitivescience.coherencecommunication

import mathlib.set.SetTheory._
import mathlib.graph._
import coherence._
import mathlib.graph.GraphImplicits.{EdgeImpl2, WDiEdgeImpl, WUnDiEdgeImpl}

import scala.util.Random

object Main {
  def main(args: Array[String]): Unit = {

    val assignment = Map(
      Node("A") -> true,
      Node("B") -> false,
      Node("C") -> true,
    )
    println(assignment)

    val newAssignment = assignment ++ Map(
      Node("B") -> true,
      Node("C") -> false,
    )
    println(newAssignment)
//    val V: Set[Node[String]] = ('A' to 'C').map((x: Char) => Node(x.toString)).toSet
//    println(V.size)
//    val requests: Set[Map[Node[String], Boolean]] = powerset(V).flatMap(_ allMappings Set(true, false))
//    println(requests.size)
//    requests.foreach(println(_))
  }
}
