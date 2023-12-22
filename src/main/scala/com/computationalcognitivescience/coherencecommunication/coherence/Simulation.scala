package com.computationalcognitivescience.coherencecommunication.coherence

import mathlib.graph.GraphImplicits.{EdgeImpl2, WUnDiEdgeImpl}
import mathlib.graph.{Node, WUnDiEdge, WUnDiGraph}
import mathlib.set.SetTheory._

class Simulation {

  val pc: Set[WUnDiEdge[Node[String]]] = Set(
    "a" ~ "b" % 1,
    "a" ~ "d" % 1
  )
  val nc: Set[WUnDiEdge[Node[String]]] = Set(
    "b" ~ "d" % 1,
    "a" ~ "c" % 1
  )

  val net: WUnDiGraph[String] = WUnDiGraph(
    pc \/ nc
  )

  val w_prior = 1
  val w_intention = 1000
  val w_communicated = 1

  val initiator = new Initiator(net, pc, nc, w_prior, w_intention, w_communicated)
  val responder = new Responder(net, pc, nc, w_prior, w_communicated)

  var conversation = true
  var T_utterance : Map[Node[String], Boolean] = null
  var T_repair : Map[Node[String], Boolean] = null
  var i: Int = 0

  while(conversation){
    println(i)
    i = i+1

    T_utterance = initiator.utteranceProduction()

    val initiateRepair: Boolean = responder.utteranceProcessing(T_utterance)

    if(initiateRepair){
      println("repair initiated")
      T_repair = responder.repairProduction()
      val repairResponse: Map[Node[String], Boolean] = initiator.repairProcessing(T_repair)
      responder.updateNetwork(repairResponse)
    }

    val endConversation : Boolean = initiator.endConversation(T_repair)
    if (endConversation){
      conversation = false
    }
  }







}
