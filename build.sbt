ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.12"

val circeVersion = "0.14.13"



lazy val injectMathJax = taskKey[Unit]("Injects MathJax Javascript into Scaladoc template.js")




lazy val root = (project in file("."))
  .settings(
    name := "coherence-communication",
    libraryDependencies += "com.markblokpoel" %% "mathlib" % "0.9.2-b2",
    libraryDependencies += "com.lihaoyi" %% "os-lib" % "0.9.1",
    libraryDependencies += "com.lihaoyi" %% "upickle" % "4.0.2",
    libraryDependencies += "io.kontainers" %% "purecsv" % "1.3.10",
    libraryDependencies += "org.scala-lang.modules" %% "scala-parallel-collections" % "1.0.4",
    libraryDependencies ++= Seq(
      "io.circe" %% "circe-core",
      "io.circe" %% "circe-generic",
      "io.circe" %% "circe-parser"
    ).map(_ % circeVersion),
    injectMathJax := {
      val docPath = (Compile / doc).value
      val templateJsOutput = docPath / "lib" / "template.js"
      streams.value.log.info(s"Adding MathJax initialization to $templateJsOutput")
      // change this path, obviously
      IO.append(templateJsOutput, IO.readBytes(file("doc/static/js/mathjax.js")))
    },
    injectMathJax := (injectMathJax triggeredBy (Compile / doc)).value
  )
