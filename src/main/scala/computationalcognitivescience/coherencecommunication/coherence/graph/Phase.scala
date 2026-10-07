package computationalcognitivescience.coherencecommunication.coherence.graph

import mathlib.graph.Node

case class Phase[T](s: Node[Set[Node[T]]], t: Node[Set[Node[T]]], w: Double) {
  override def canEqual(obj: Any): Boolean =
    obj.isInstanceOf[Phase[_]]

  override def equals(obj: Any): Boolean = {

    obj match {
      case obj: Phase[_] => obj.s == s && obj.t == t || obj.s == t && obj.t == s
      case _             => false
    }
  }

  override def hashCode: Int = {
    val prime  = 31
    var result = 1
    result = prime * result + s.hashCode() + t.hashCode()
    result
  }
}