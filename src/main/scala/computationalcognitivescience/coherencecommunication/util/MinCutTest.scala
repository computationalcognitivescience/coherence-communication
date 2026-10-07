package computationalcognitivescience.coherencecommunication.util

import computationalcognitivescience.coherencecommunication.coherence.graph.WUnDiCut.ImplWUnDiGraph
import mathlib.graph.GraphImplicits._
import mathlib.graph.WUnDiGraph

object MinCutTest {
  def main(args: Array[String]): Unit = {
    val test1 = WUnDiGraph(
      Set(
        N("1") ~ N("2") % 2,
        N("1") ~ N("5") % 3,
        N("2") ~ N("3") % 3,
        N("2") ~ N("5") % 2,
        N("2") ~ N("6") % 2,
        N("3") ~ N("4") % 4,
        N("3") ~ N("7") % 2,
        N("4") ~ N("7") % 2,
        N("4") ~ N("8") % 2,
        N("5") ~ N("6") % 3,
        N("6") ~ N("7") % 1,
        N("7") ~ N("8") % 3
      )
    )

    val res1 = test1.minCut()
    res1.foreach(cut => println(s"${cut.weight}  => ${cut.edges}"))

    /*
    1 -- 2
    |    |
    3 -- 4
    */
    println("Test 2")
    val test2 = WUnDiGraph(
      Set(
        N("1") ~ N("2") % 3,
        N("1") ~ N("3") % 3,
        N("2") ~ N("4") % 3,
        N("3") ~ N("4") % 3
      )
    )
    val res2 = test2.minCut()
    res2.foreach(cut => println(s"${cut.weight}  => ${cut.edges}"))
  }
}
