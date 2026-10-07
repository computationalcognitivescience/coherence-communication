package computationalcognitivescience.coherencecommunication.coherence.graph

import computationalcognitivescience.coherencecommunication.coherence.graph.WUnDiCut.ImplWUnDiGraph
import mathlib.graph.GraphImplicits._
import mathlib.graph.{Node, WUnDiGraph}

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

//    println(test1.minCutPhase(test1.initializeMergeGraph, Node(Set(Node("2")))))

    val res = test1.minCut()
    res.foreach(cut => {
      println(cut.cut)
      println(cut.weight)
    })

    println(res.head == res.last)

    //    println(
    //      toDOTStringCorrect(
    //        test1.mergeCut(
    //          test1.initializeMergeGraph,
    //          Phase(
    //            Node(Set("1")),
    //            Node(Set("5")),
    //            5
    //          )
    //        )
    //      )
    //    )

    def toDOTStringCorrect[T](g: WUnDiGraph[T]): String = {
      val vertexIds = g.vertices.toSeq.zipWithIndex

      "graph G {\n" +
        vertexIds.map(nid => s"\tN${nid._2} [label=\"${nid._1.label}\"]").mkString("", "\n", "\n") +
        g.edges
          .map(edge => {
            "\tN" + vertexIds.find(_._1 == edge.left).get._2 +
              " -- N" + vertexIds.find(_._1 == edge.right).get._2 +
              " [label=" + edge.weight + "]"
          })
          .mkString("\n") +
        "\n}"
    }
  }
}
