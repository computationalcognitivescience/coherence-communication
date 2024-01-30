ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.12"

lazy val root = (project in file("."))
  .settings(
    name := "coherence-communication",
    libraryDependencies ++= Seq(
      "com.markblokpoel" %% "mathlib" % "0.9.2-b1",
      "org.scala-lang.modules" %% "scala-parallel-collections" % "1.0.4",
      "com.github.tototoshi" %% "scala-csv" % "1.3.10"
    )
  )
