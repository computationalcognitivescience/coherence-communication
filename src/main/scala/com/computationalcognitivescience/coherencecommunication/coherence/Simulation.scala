package com.computationalcognitivescience.coherencecommunication.coherence

import mathlib.graph.{Node, WUnDiEdge, WUnDiGraph}
import mathlib.set.SetTheory._

import scala.util.Random
import scala.util.Random.shuffle


class Simulation {

  val w_constraints = 1
  val w_prior = 1
  val w_intention = 1000
  val w_communicated = 1

  val N_vertices = 8 // Random.between(3,6) //TODO: give correct range
  val N_pc : Int = Random.between(1,4) //TODO: give correct range
  val N_nc : Int = Random.between(1,4) //TODO: give correct range
  val N_priorInitiator : Int = Random.between(1,3) //TODO: give correct range
  val N_intentionInitiator : Int = Random.between(1,3) //TODO: give correct range
  val N_priorResponder : Int = N_priorInitiator //TODO: give correct range

  val vertices: Set[Node[String]] = (0 until N_vertices).toSet.map("N" + _).map(Node(_))
  val pc : Set[WUnDiEdge[Node[String]]] = randomPositiveConstraints(N_pc, w_constraints, vertices)
  val nc : Set[WUnDiEdge[Node[String]]] = randomNegativeConstraints(N_nc, w_constraints, vertices, pc)
  val net: WUnDiGraph[String]= createNetwork(pc, nc)



  val priorBeliefsInitiator : Map[Node[String], Boolean] = randomPriorBeliefInitiator(vertices, N_priorInitiator)
  val intentionBeliefsInitiator : Map[Node[String], Boolean] = randomIntentionBeliefInitiator(vertices, N_intentionInitiator, priorBeliefsInitiator)
  val priorBeliefsResponder : Map[Node[String], Boolean] = randomPriorBeliefResponder(vertices, N_priorResponder)


  val initiator = new Initiator(net, pc, nc, priorBeliefsInitiator, intentionBeliefsInitiator, w_prior, w_intention, w_communicated)
  val responder = new Responder(net, pc, nc, priorBeliefsResponder, w_prior, w_communicated)

  var conversation = true
  var T_utterance : Map[Node[String], Boolean] = null
  var T_repair : Map[Node[String], Boolean] = null
  var i: Int = 0

  while(conversation){
    println(f"iteration $i")
    i = i+1

    T_utterance = initiator.utteranceProduction()

    val initiateRepair: Boolean = responder.utteranceProcessing(T_utterance)

    if(initiateRepair){
      T_repair = responder.repairProduction()
      val repairResponse: Map[Node[String], Boolean] = initiator.repairProcessing(T_repair)
      responder.updateNetwork(repairResponse)
    }

    val endConversation : Boolean = initiator.endConversation(T_repair)
    if (endConversation){
      conversation = false
    }
  }


  def randomPositiveConstraints(N_pc : Int, w_constraints : Int, vertices : Set[Node[String]]): Set[WUnDiEdge[Node[String]]] = {
    val possibleEdges = (vertices x vertices).filter(n => n._1.label < n._2.label)
    val selectedEdges = shuffle(possibleEdges).take(N_pc)
    val pc : Set[WUnDiEdge[Node[String]]] = selectedEdges.map(n => WUnDiEdge(n._1, n._2, w_constraints))
    pc
  }

  def randomNegativeConstraints(N_nc : Int, w_constraints : Int, vertices : Set[Node[String]], pc : Set[WUnDiEdge[Node[String]]]): Set[WUnDiEdge[Node[String]]] = {
    val possibleEdges = (vertices x vertices).filter(n => n._1.label < n._2.label)
    val filteredOutPC = possibleEdges.filter(n => !pc.contains(WUnDiEdge(n._1, n._2, w_constraints)))
    val selectedEdges = shuffle(filteredOutPC).take(N_nc)
    val nc : Set[WUnDiEdge[Node[String]]] = selectedEdges.map(n => WUnDiEdge(n._1, n._2, w_constraints))

    nc
  }

  def createNetwork(pc : Set[WUnDiEdge[Node[String]]], nc : Set[WUnDiEdge[Node[String]]]): WUnDiGraph[String] = {
    val net: WUnDiGraph[String] = WUnDiGraph(
      pc \/ nc
    )
    net
  }

  def randomPriorBeliefInitiator(vertices : Set[Node[String]], n_prior : Int): Map[Node[String], Boolean] = {
    val priorBeliefs : Set[Node[String]] = shuffle(vertices).take(n_prior)
    val rd : Random = new Random()
    val prior = priorBeliefs.map(n => n -> rd.nextBoolean()).toMap

    prior
  }

  def randomIntentionBeliefInitiator(vertices: Set[Node[String]], n_intention: Int, priorBeliefsInitiator: Map[Node[String], Boolean]): Map[Node[String], Boolean] = {
    val intentionBeliefs: Set[Node[String]] = shuffle(vertices.filter(n => !priorBeliefsInitiator.keySet.contains(n))).take(n_intention)
    val rd: Random = new Random()
    val intention: Map[Node[String], Boolean] = intentionBeliefs.map(n => n -> rd.nextBoolean()).toMap

    intention
  }


  def randomPriorBeliefResponder(vertices : Set[Node[String]], n_prior : Int): Map[Node[String], Boolean] = {
    val priorBeliefs: Set[Node[String]] = shuffle(vertices).take(n_prior)
    val rd: Random = new Random()
    val prior: Map[Node[String], Boolean] = priorBeliefs.map(n => n -> rd.nextBoolean()).toMap

    prior

  }



}
