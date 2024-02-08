package com.computationalcognitivescience.coherencecommunication.coherence

import com.github.tototoshi.csv._
import mathlib.graph.{Node, UnDiGraph, WUnDiEdge, WUnDiGraph}
import mathlib.set.SetTheory._

import java.io.File
import java.time.ZoneId
import scala.collection.parallel.CollectionConverters._
import scala.util.Random
import scala.util.Random.shuffle
import scala.util.control.Breaks.{break, breakable}

case object Simulation {
  case class Parameter(
                        constraintWeight: Double,
                        priorWeight: Double,
                        communicatedWeightInitiator: Double,
                        communicatedWeightResponder: Double,
                        numberOfPriorBeliefs: Int
                      )

  case class Results(
      n_vertices: Int,
      n_edges: Int,
      n_pc: Int,
      n_nc: Int,
      n_priorBeliefsResponder: Int,
      w_constraints: Double,
      w_prior: Double,
      w_communicatedInitiator: Double,
      w_communicatedResponder: Double,
      w_intention: Double,
      preferredPriorBeliefsInitiator : Map[Node[String], Boolean],
      n_priorBeliefsInitiator: Int,
      preferredIntentionBeliefsInitiator : Map[Node[String], Boolean],
      n_intentionBeliefsInitiator: Int,
      preferredPriorBeliefsResponder : Map[Node[String], Boolean],
      initialPriorBeliefsInitiator: Map[Node[String], Boolean],
      initialIntentionBeliefsInitiator: Map[Node[String], Boolean],
      initialPriorBeliefsResponder: Map[Node[String], Boolean],
      initialInitiatorTVA: Map[Node[String], Boolean],
      initialCoherenceInitiator: Double,
      initialResponderTVA: Map[Node[String], Boolean],
      initialCoherenceResponder: Double,
      initialSharedBeliefs: Map[Node[String], Boolean],
      n_initialSharedBeliefs: Int,
      initialSharedIntentionBeliefs: Map[Node[String], Boolean],
      n_initialSharedIntentionBeliefs: Int,
      initialPriorBeliefsComparison: Map[Node[String], (Boolean, Boolean)],
      n_initialPriorBeliefsComparison: Int,

      allUtterances: List[Map[Node[String], Boolean]],
      allRepairRequests: List[Map[Node[String], Boolean]],
      allRepairAnswers: List[Map[Node[String], Boolean]],
      allEndConversationChecks: List[(Boolean, Boolean)],

      finalPriorBeliefsInitiator: Map[Node[String], Boolean],
      finalPriorBeliefsResponder: Map[Node[String], Boolean],
      finalInitiatorTVA: Map[Node[String], Boolean],
      finalCoherenceInitiator: Double,
      finalIntentionBeliefsInitiator: Map[Node[String], Boolean],
      finalResponderTVA: Map[Node[String], Boolean],
      finalCoherenceResponder: Double,
      finalSharedBeliefs: Map[Node[String], Boolean],
      n_finalSharedBeliefs: Int,
      finalSharedIntentionBeliefs: Map[Node[String], Boolean],
      n_finalSharedIntentionBeliefs: Int,
      finalPriorBeliefsComparison: Map[Node[String], (Boolean, Boolean)],
      n_finalPriorBeliefsComparison: Int,
      ) {
    /*
    parameters
    - all
    belief alignment analysis
    - structural similarity of TVA between agents
      - all beliefs pre and post
      - intention beliefs pre and post
      - non-intention beliefs pre and post
     - structural similarity of TVA with-in agent
      - all beliefs pre and post
      - prior beliefs pre and post
    - prior beliefs
      - (responder prior /\ initiator prior) structural similarity (op truth-value)
      - |(responder prior /\ initiator prior)| / min(|r_prior|, |i_prior|) (op aantal, may or may not be the same tv)
      - (responder prior /\ initiator intention) structural similarity
      - |(responder prior /\ initiator intention)| / min(|r_prior|, |i_intention|)
    conversation analysis
    - number of turns
    - number of repair
    - utterance length per turn (max |n_vertices| cols)
    
    voor later:
    - tracking the dialogue, e.g., which communicated beliefs get accepted when... 
    */
    
    def toSeq(): Seq[Any] = {
      val allBeliefsInitial = structuralSimilarity(initialInitiatorTVA, initialResponderTVA)
      val allBeliefsFinal = structuralSimilarity(finalInitiatorTVA, finalResponderTVA)
      val intentionBeliefsPreferredInitial = structuralSimilarity(preferredIntentionBeliefsInitiator, initialResponderTVA)
      val intentionBeliefsInitial = structuralSimilarity(initialIntentionBeliefsInitiator, initialResponderTVA)
      val intentionBeliefsPreferredFinal = structuralSimilarity(preferredIntentionBeliefsInitiator, finalResponderTVA)
      val intentionBeliefsFinal = structuralSimilarity(finalIntentionBeliefsInitiator, finalResponderTVA)

      val allBeliefsInitiator_initialVSfinal = structuralSimilarity(initialInitiatorTVA, finalInitiatorTVA)
      val allBeliefsResponder_initialVSfinal = structuralSimilarity(initialResponderTVA, finalResponderTVA)
      val priorBeliefsInitiator_preferredVSinitial = structuralSimilarity(preferredPriorBeliefsInitiator, initialPriorBeliefsInitiator)
      val priorBeliefsInitiator_initialVSfinal = structuralSimilarity(initialPriorBeliefsInitiator, finalPriorBeliefsInitiator)
      val priorBeliefsResponder_preferredVSinitial = structuralSimilarity(preferredPriorBeliefsResponder, initialPriorBeliefsResponder)
      val priorBeliefsResponder_initialVSfinal = structuralSimilarity(initialPriorBeliefsResponder, finalPriorBeliefsResponder)
      val intentionBeliefsInitiator_preferredVSinitial = structuralSimilarity(preferredIntentionBeliefsInitiator, initialIntentionBeliefsInitiator)
      val intentionBeliefsInitiator_initialVSfinal = structuralSimilarity(initialIntentionBeliefsInitiator, finalIntentionBeliefsInitiator)

      val overlappingPreferredPriors = (preferredPriorBeliefsInitiator.keySet /\ preferredPriorBeliefsResponder.keySet).size
      val overlappingPriors = (initialPriorBeliefsInitiator.keySet /\ initialPriorBeliefsResponder.keySet).size
      val overlappingPreferredPriors_StructuralSimilarity = structuralSimilarity(preferredPriorBeliefsInitiator, preferredPriorBeliefsResponder)
      val overlappingPriors_StructuralSimilarity = structuralSimilarity(initialPriorBeliefsInitiator, initialPriorBeliefsResponder)
      val overlappingPreferredIntention = (preferredIntentionBeliefsInitiator.keySet /\ initialResponderTVA.keySet).size
      val overlappingIntention = (initialIntentionBeliefsInitiator.keySet /\ initialResponderTVA.keySet).size

      Seq(
        n_vertices,
        n_edges,
        n_pc, 
        n_nc,
        n_priorBeliefsResponder ,
        w_constraints,
        w_prior,
        w_communicatedInitiator,
        w_communicatedResponder,
        w_intention,
        allBeliefsInitial,
        allBeliefsFinal,
        intentionBeliefsPreferredInitial,
        intentionBeliefsInitial,
        intentionBeliefsPreferredFinal,
        intentionBeliefsFinal,
        allBeliefsInitial - intentionBeliefsInitial,
        allBeliefsFinal - intentionBeliefsFinal,
        allBeliefsInitiator_initialVSfinal,
        allBeliefsResponder_initialVSfinal,
        priorBeliefsInitiator_preferredVSinitial,
        priorBeliefsInitiator_initialVSfinal,
        priorBeliefsResponder_preferredVSinitial,
        priorBeliefsResponder_initialVSfinal,
        intentionBeliefsInitiator_preferredVSinitial,
        intentionBeliefsInitiator_initialVSfinal,
        overlappingPreferredPriors,
        overlappingPriors,
        overlappingPreferredPriors_StructuralSimilarity,
        overlappingPriors_StructuralSimilarity,
        overlappingPreferredIntention,
        overlappingIntention,
        allUtterances.size,
        allUtterances.tail,
        allRepairRequests.size,
        allUtterances.map(n => n.size),
      )
    }
   
    def structuralSimilarity(
        tva1: Map[Node[String], Boolean],
        tva2: Map[Node[String], Boolean]
    ): Int = {
      val overlappingBeliefs = tva1.keySet /\ tva2.keySet
      overlappingBeliefs.toList
        .map(belief => tva1(belief) == tva2(belief))
        .count(_ == true)
    }
    
    def overlappingBeliefs(
       tva1: Map[Node[String], Boolean],
       tva2: Map[Node[String], Boolean]
     ): Int = (tva1.keySet /\ tva2.keySet).size
  }


  private val N_vertices = 10
  private val N_priorInitiator : Int = 3
  private val N_intentionInitiator : Int = 3
  private val N_priorResponderList: List[Int] = List(3, 6)
  private val numberOfAgentPairsPerCondition: Int = 100

  val w_intention : Double = 100.0
  val w_constraintsList : List[Double] = List(0.7, 1.3)
  val w_priorList : List[Double] = List(0.7, 1.3)
  val w_communicatedInitiatorList : List[Double] = List(0.7, 1.3)
  val w_communicatedResponderList : List[Double] = List(0.7, 1.3)

  def run(): Unit = {
    val allParameterCombinations = for(w_constraints <- w_constraintsList;
        w_prior <- w_priorList;
        w_communicatedInitiator <- w_communicatedInitiatorList;
        w_communicatedResponder <- w_communicatedResponderList;
        n_priorResponder <- N_priorResponderList) yield {
      Parameter(
        w_constraints,
        w_prior,
        w_communicatedInitiator,
        w_communicatedResponder,
        n_priorResponder
      )
    }

    val columnNames: List[String] = List(
      "n_vertices",
      "n_edges",
      "n_pc",
      "n_nc",
      "n_priorBeliefsResponder",
      "w_constraints",
      "w_prior",
      "w_communicatedInitiator",
      "w_communicatedResponder",
      "w_intention",
      "allBeliefsInitial",
      "allBeliefsFinal",
      "intentionBeliefsPreferredInitial",
      "intentionBeliefsInitial",
      "intentionBeliefsPreferredFinal",
      "intentionBeliefsFinal",
      "nonIntentionBeliefsInitial",
      "nonIntentionBeliefsFinal",
      "allBeliefsInitiator_initialVSfinal",
      "allBeliefsResponder_initialVSfinal",
      "priorBeliefsInitiator_preferredVSinitial",
      "priorBeliefsInitiator_initialVSfinal",
      "priorBeliefsResponder_preferredVSinitial",
      "priorBeliefsResponder_initialVSfinal",
      "intentionBeliefsInitiator_preferredVSinitial",
      "intentionBeliefsInitiator_initialVSfinal",
      "overlappingPreferredPriors",
      "overlappingPriors",
      "overlappingPreferredPriors_StructuralSimilarity",
      "overlappingPriors_StructuralSimilarity",
      "overlappingPreferredIntention",
      "overlappingIntention",
      "n_utterances",
      "last_utterance",
      "n_repairRequests",
      "utteranceLengthPerTurn"
//      "initialPriorBeliefsInitiator",
//      "n_initialPriorBeliefsInitiator",
//      "intentionBeliefsInitiator",
//      "n_intentionBeliefsInitiator",
//      "initialPriorBeliefsResponder",
//      "initialInitiatorTVA",
//      "initialCoherenceInitiator",
//      "initialResponderTVA",
//      "initialCoherenceResponder",
//      "initialSharedBeliefs",
//      "n_initialSharedBeliefs",
//      "initialSharedIntentionBeliefs",
//      "n_initialSharedIntentionBeliefs",
//      "initialPriorBeliefsComparison",
//      "n_initialPriorBeliefsComparison",
//      "allUtterances",
//      "allRepairRequests",
//      "allRepairAnswers",
//      "allEndConversationChecks",
//      "finalPriorBeliefsInitiator",
//      "finalPriorBeliefsResponder",
//      "finalInitiatorTVA",
//      "finalCoherenceInitiator",
//      "finalResponderTVA",
//      "finalCoherenceResponder",
//      "finalSharedBeliefs",
//      "n_finalSharedBeliefs",
//      "finalSharedIntentionBeliefs",
//      "n_finalSharedIntentionBeliefs",
//      "finalPriorBeliefsComparison",
//      "n_finalPriorBeliefsComparison"
    )
    val currentDir: String = System.getProperty("user.dir")
    val time = java.time.LocalDateTime.now.atZone(ZoneId.systemDefault()).toEpochSecond
    val filePath: String = currentDir + "\\_Results\\Simulation_results_" + time + ".csv"

    val f = new File(filePath)
    val writer = CSVWriter.open(f)
    writer.writeRow(columnNames)
    writer.close()

    for(par <- allParameterCombinations) {
      val agentPairs: Seq[(Initiator, Responder)] =
        for(_ <- 0 until numberOfAgentPairsPerCondition) yield createAgentPair(par)
      val results = agentPairs.par.map(pair => {
        doConversation(pair._1, pair._2)
      }).toList
      // write results to file

//      columnNames
      val currentDir : String = System.getProperty("user.dir")
      val filePath : String = currentDir+"\\_Results\\Simulation_results_"+time+".csv"

      val f = new File(filePath)
      val writer = CSVWriter.open(f, append=true)
      results.foreach(row => writer.writeRow(row.toSeq()))
      writer.close()

      results.foreach(println)
    }
  }


  def doConversation(initiator: Initiator, responder: Responder): Results = {
    var conversation = true
    var T_utterance: Map[Node[String], Boolean] = null
    var T_repair: Map[Node[String], Boolean] = null

    val preferredPriorBeliefsInitiator = initiator.BeliefNetwork.multiBeliefBiases(0).valueAssignment
    val n_priorBeliefsInitiator = preferredPriorBeliefsInitiator.size
    val preferredIntentionBeliefsInitiator = initiator.BeliefNetwork.multiBeliefBiases(1).valueAssignment
    val n_intentionBeliefsInitiator = preferredIntentionBeliefsInitiator.size
    val preferredPriorBeliefsResponder = responder.BeliefNetwork.multiBeliefBiases(0).valueAssignment
    val n_priorBeliefsResponder = preferredPriorBeliefsResponder.size

    val initialInitiatorTVA = initiator.getTVA()
    val initialResponderTVA = responder.getTVA()

    val initialPriorBeliefsInitiator = initialInitiatorTVA.filter(n => preferredPriorBeliefsInitiator.contains(n._1))
    val initialIntentionBeliefsInitiator = initialInitiatorTVA.filter(n => preferredIntentionBeliefsInitiator.contains(n._1))
    val initialPriorBeliefsResponder = initialResponderTVA.filter(n => preferredPriorBeliefsResponder.contains(n._1))
    val initialCoherenceInitiator = initiator.BeliefNetwork.coh(initialInitiatorTVA)
    val initialCoherenceResponder = initiator.BeliefNetwork.coh(initialResponderTVA)
    val initialSharedBeliefs = initialInitiatorTVA.filter(n => initialInitiatorTVA.get(n._1) == initialResponderTVA.get(n._1))
    val n_initialSharedBeliefs = initialInitiatorTVA.size
    val initialSharedIntentionBeliefs = initialIntentionBeliefsInitiator.filter(n => initialInitiatorTVA.get(n._1) == initialResponderTVA.get(n._1))
    val n_initialSharedIntentionBeliefs = initialSharedIntentionBeliefs.size
    val initiatorPriorBeliefsComparison = initialPriorBeliefsInitiator.keySet.map(n => n->(initialInitiatorTVA.get(n)==true, initialResponderTVA.get(n)==true))
    val responderPriorBeliefsComparison = initialPriorBeliefsResponder.keySet.map(n => n->(initialInitiatorTVA.get(n)==true, initialResponderTVA.get(n)==true))
    val initialPriorBeliefsComparison = (initiatorPriorBeliefsComparison ++ responderPriorBeliefsComparison).toMap
    val n_initialPriorBeliefsComparison = initialPriorBeliefsComparison.size

    var allUtterances = List[Map[Node[String], Boolean]]()
    var allRepairRequests = List[Map[Node[String], Boolean]]()
    var allRepairAnswers = List[Map[Node[String], Boolean]]()
    var allEndConversationChecks = List[(Boolean, Boolean)]()


      breakable {
      while (conversation) {
        // reset T_repair to null for the endConversation check
        var T_repair: Map[Node[String], Boolean] = null


        T_utterance = initiator.utteranceProduction()

        if (T_utterance == null) {
          val logNode : Node[String] = Node[String]("AllNodesCommunicated")
          val log : Map[Node[String], Boolean] = Map(logNode -> true)
          allUtterances = allUtterances :+ log
          break()
        }
        allUtterances = allUtterances :+ T_utterance

        val initiateRepair: Boolean = responder.utteranceProcessing(T_utterance)

        if (initiateRepair) {
          T_repair = responder.repairProduction()
          allRepairRequests = allRepairRequests :+ T_repair

          val repairResponse: Map[Node[String], Boolean] = initiator.repairProcessing(T_repair)
          allRepairAnswers = allRepairAnswers :+ repairResponse
          responder.updateNetwork(repairResponse)
        }

        val endConversation = initiator.endConversation(T_repair)
        allEndConversationChecks = allEndConversationChecks :+ (endConversation._2, endConversation._3)


        if (endConversation._1) {
          conversation = false
        }
      }
    }

    val finalInitiatorPriorBeliefsComparison = initialPriorBeliefsInitiator.keySet.map(n => n -> (initialInitiatorTVA.get(n) == true, initialResponderTVA.get(n) == true))
    val finalResponderPriorBeliefsComparison = initialPriorBeliefsResponder.keySet.map(n => n -> (initialInitiatorTVA.get(n) == true, initialResponderTVA.get(n) == true))
    val finalPriorBeliefsComparison = (finalInitiatorPriorBeliefsComparison ++ finalResponderPriorBeliefsComparison).toMap
    val finalInitiatorTVA = initiator.getTVA()
    val finalResponderTVA = responder.getTVA()
    val finalIntentionBeliefsInitiator = finalInitiatorTVA.filter(n => preferredIntentionBeliefsInitiator.contains(n._1))

    Results(
      N_vertices,
      initiator.BeliefNetwork.edges.size,
      initiator.BeliefNetwork.positiveConstraints.size,
      initiator.BeliefNetwork.negativeConstraints.size,
      n_priorBeliefsResponder,
      initiator.BeliefNetwork.positiveConstraints.head.weight,
      initiator.get_w_prior(),
      initiator.get_w_communicated(),
      responder.get_w_communicated(),
      initiator.get_w_intention(),
      preferredPriorBeliefsInitiator,
      n_priorBeliefsInitiator,
      preferredIntentionBeliefsInitiator,
      n_intentionBeliefsInitiator,
      preferredPriorBeliefsResponder,
      initialPriorBeliefsInitiator,
      initialIntentionBeliefsInitiator,
      initialPriorBeliefsResponder,
      initialInitiatorTVA,
      initialCoherenceInitiator,
      initialResponderTVA,
      initialCoherenceResponder,
      initialSharedBeliefs,
      n_initialSharedBeliefs,
      initialSharedIntentionBeliefs,
      n_initialSharedIntentionBeliefs,
      initialPriorBeliefsComparison,
      n_initialPriorBeliefsComparison,

      allUtterances,
      allRepairRequests,
      allRepairAnswers,
      allEndConversationChecks,

      finalInitiatorTVA.filter(n => preferredPriorBeliefsInitiator.contains(n._1)),
      finalResponderTVA.filter(n => preferredPriorBeliefsResponder.contains(n._1)),
      finalInitiatorTVA,
      initiator.BeliefNetwork.coh(finalInitiatorTVA),
      finalIntentionBeliefsInitiator,
      finalResponderTVA,
      responder.BeliefNetwork.coh(finalResponderTVA),
      finalInitiatorTVA.filter(n => finalInitiatorTVA.get(n._1) == finalResponderTVA.get(n._1)),
      finalInitiatorTVA.count(n => finalInitiatorTVA.get(n._1) == finalResponderTVA.get(n._1)),
      finalIntentionBeliefsInitiator.filter(n => finalInitiatorTVA.get(n._1) == finalResponderTVA.get(n._1)),
      finalIntentionBeliefsInitiator.count(n => finalInitiatorTVA.get(n._1) == finalResponderTVA.get(n._1)),
      finalPriorBeliefsComparison,
      finalPriorBeliefsComparison.size
    )
  }

  def createAgentPair(parameter: Parameter): (Initiator, Responder) = {
    val net: WUnDiGraph[String] = preferentialAttachment(N_vertices, 2, parameter.constraintWeight)
    val N_edges = net.edges.size
    val N_nc = N_edges / 2
    val N_pc = N_edges - N_nc

    val pc = shuffle(net.edges.toList).take(N_pc).toSet
    val not_pc_constraints = net.edges.filter(n => !pc.contains(WUnDiEdge(n.left, n.right, parameter.constraintWeight)))
    val nc = shuffle(not_pc_constraints.toList).take(N_nc).toSet
    val vertices = net.vertices

    val intentionBeliefsInitiator: Map[Node[String], Boolean] = randomIntentionBeliefInitiator(vertices, N_intentionInitiator)
    val priorBeliefsInitiator: Map[Node[String], Boolean] = randomPriorBeliefInitiator(vertices, N_priorInitiator, intentionBeliefsInitiator)
    val priorBeliefsResponder: Map[Node[String], Boolean] = randomPriorBeliefResponder(vertices, parameter.numberOfPriorBeliefs)

    val initiator = new Initiator(net, pc, nc, priorBeliefsInitiator, intentionBeliefsInitiator, parameter.priorWeight, w_intention, parameter.communicatedWeightInitiator)
    val responder = new Responder(net, pc, nc, priorBeliefsResponder, parameter.priorWeight, parameter.communicatedWeightResponder)
    (initiator, responder)
  }

  private def randomPositiveConstraints(N_pc : Int, w_constraints : Int, vertices : Set[Node[String]]): Set[WUnDiEdge[Node[String]]] = {
    val possibleEdges = (vertices x vertices).filter(n => n._1.label < n._2.label)
    val selectedEdges = shuffle(possibleEdges.toList).take(N_pc).toSet
    val pc : Set[WUnDiEdge[Node[String]]] = selectedEdges.map(n => WUnDiEdge(n._1, n._2, w_constraints))
    pc
  }

  private def randomNegativeConstraints(N_nc : Int, w_constraints : Int, vertices : Set[Node[String]], pc : Set[WUnDiEdge[Node[String]]]): Set[WUnDiEdge[Node[String]]] = {
    val possibleEdges = (vertices x vertices).filter(n => n._1.label < n._2.label)
    val filteredOutPC = possibleEdges.filter(n => !pc.contains(WUnDiEdge(n._1, n._2, w_constraints)))
    val selectedEdges = shuffle(filteredOutPC.toList).take(N_nc).toSet
    val nc : Set[WUnDiEdge[Node[String]]] = selectedEdges.map(n => WUnDiEdge(n._1, n._2, w_constraints))

    nc
  }

  private def createNetwork(pc : Set[WUnDiEdge[Node[String]]], nc : Set[WUnDiEdge[Node[String]]]): WUnDiGraph[String] = {
    val net: WUnDiGraph[String] = WUnDiGraph(pc \/ nc)
    net
  }

  // Code adapted from preferentialAttachment function from WUnDiGraph.scala, as made by Mark Blokpoel
  def preferentialAttachment(size: Int, m: Int, w_constraints: Double): WUnDiGraph[String] = {
    val unweightedGraph = UnDiGraph.preferentialAttachment(size, m)
    val weightedEdges = unweightedGraph.edges.map(e => WUnDiEdge(e.left, e.right, w_constraints))
    WUnDiGraph(unweightedGraph.vertices, weightedEdges)
  }

  private def randomIntentionBeliefInitiator(vertices : Set[Node[String]], n_intention : Int): Map[Node[String], Boolean] = {
    val intentionBeliefs : Set[Node[String]] = shuffle(vertices.toList).take(n_intention).toSet
    val rd : Random = new Random()
    val intention = intentionBeliefs.map(n => n -> rd.nextBoolean()).toMap

    intention
  }

  private def randomPriorBeliefInitiator(vertices: Set[Node[String]], n_prior : Int, intentionBeliefsInitiator: Map[Node[String], Boolean]): Map[Node[String], Boolean] = {
    val priorBeliefs: Set[Node[String]] = shuffle(vertices.diff(intentionBeliefsInitiator.keySet).toList).take(n_prior).toSet
    val rd: Random = new Random()
    val prior: Map[Node[String], Boolean] = priorBeliefs.map(n => n -> rd.nextBoolean()).toMap

    prior
  }


  private def randomPriorBeliefResponder(vertices : Set[Node[String]], n_prior : Int): Map[Node[String], Boolean] = {
    val priorBeliefs: Set[Node[String]] = shuffle(vertices).toList.take(n_prior).toSet
    val rd: Random = new Random()
    val prior: Map[Node[String], Boolean] = priorBeliefs.map(n => n -> rd.nextBoolean()).toMap

    prior

  }


}
