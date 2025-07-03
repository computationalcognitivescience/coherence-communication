//package com.computationalcognitivescience.coherencecommunication.util
//
//import com.computationalcognitivescience.coherencecommunication.{Parameters, PicklableConversationData}
//
//object Converter {
//
//  implicit class ImplPickledData(data: Seq[(Parameters, Seq[PicklableConversationData])]) {
//
//
////    def toAnimatedGraphviz(id: Int): String = {
////      val conversationDataOption = data.find(_._1.id == id)
////      if (conversationDataOption.isEmpty) {
////        s"""
////        <!DOCTYPE html>
////        <meta charset="utf-8">
////        <body>Id $id not found.</body>
////        </html>
////        """
////      } else {
////        val conversationData = conversationDataOption.get
////        """
////          |<!DOCTYPE html>
////          |<meta charset="utf-8">
////          |<body>
////          |<script src="//d3js.org/d3.v7.min.js"></script>
////          |<script src="https://unpkg.com/@hpcc-js/wasm@2.20.0/dist/graphviz.umd.js"></script>
////          |<script src="https://unpkg.com/d3-graphviz@5.6.0/build/d3-graphviz.js"></script>
////          |<div id="graph" style="text-align: center;"></div>
////          |<div>
////          |    <button onclick="prev()">Previous</button>
////          |    <button onclick="next()">Next</button>
////          |</div>
////          |<script>
////          |
////          |var dotIndex = 0;
////          |var graphviz = d3.select("#graph").graphviz()
////          |    .engine("fdp")
////          |    .transition(function () {
////          |        return d3.transition("main")
////          |            //.ease(d3.easeLinear)
////          |            //.duration(1500)
////          |            .delay(0);
////          |    })
////          |    .logEvents(true)
////          |    .on("initEnd", render);
////          |
////          |function render() {
////          |    var dotLines = dots[dotIndex];
////          |    var dot = dotLines.join('');
////          |    graphviz
////          |        .renderDot(dot);
////          |}
////          |
////          |function next() {
////          |    dotIndex = (dotIndex + 1) % dots.length;
////          |    render();
////          |}
////          |
////          |function prev() {
////          |    dotIndex = (dotIndex - 1) % dots.length;
////          |    render();
////          |}
////          |
////          |var dots = [
////          |""" +
////          conversationData._2.foreach(round => {
////            val r = round.round
////            val initiator = round.initiatorState
////            val responder = round.responderState
////            val utterance = "Initiator said: " + round.utterance.getOrElse("")
////            val request = "Responder asks: " + round.repair.getOrElse("")
////            "[\n'graph G {',\n'label=" + r + "',\n" +
////              ("sub" + initiator.toDOTString(utterance)).split("\n").mkString("'", "',\n'", "',\n") +
////              ("sub" + responder.toDOTString(request)).split("\n").mkString("'", "',\n'", "',\n") +
////              "'}',\n],\n"
////          }) +
////          "];\n</script>"
////      }
////    }
//
//  }
//}