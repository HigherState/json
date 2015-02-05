name := "json"

organization := "org.higherState"

version := "0.1.0"

scalaVersion := "2.11.5"

libraryDependencies ++= Seq(
  "org.scala-lang" % "scala-compiler" % "2.11.5",
  "org.scalaz" %% "scalaz-core" % "7.1.0",
  "org.scalatest" % "scalatest_2.11" % "2.2.0" % "test",
  "joda-time" % "joda-time" % "2.1",
  "org.joda" % "joda-convert" % "1.2"
)