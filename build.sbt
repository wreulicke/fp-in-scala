import sbt.Keys.libraryDependencies

name := "fp-in-scala"

version := "0.1"

scalaVersion := "2.12.4"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.4" % "test"
libraryDependencies += "org.slf4j" % "slf4j-api" % "1.7.25"
libraryDependencies += "ch.qos.logback" % "logback-classic" % "1.2.3"
libraryDependencies += "org.scalameta" %% "scalameta" % "3.7.4"
libraryDependencies += "io.monix" %% "monix" % "2.3.3"


