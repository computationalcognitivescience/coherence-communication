package com.computationalcognitivescience.coherencecommunication.coherence
import mathlib.graph.Node

//TODO: Ask Mark about "Never override case class equals"
case class WeightedBelief(
                           label: String,
                           weight: Double
                         ) {

//  if ((weight < 0) || (weight > 1)){
//    throw new IllegalArgumentException("The weight of a WeightedBelief must be in [0,1]")
//  }

  def canEqual(a: Any): Boolean = a.isInstanceOf[WeightedBelief]

  override def equals(a: Any): Boolean = {
    a match {
      case a: WeightedBelief => {
        this.label == a.label
      }
      case _ => false
    }
  }
}
