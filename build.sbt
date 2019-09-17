name := "data-exercise"
organization := "net.alexjeffery"

scalaVersion := "2.13.0"

libraryDependencies += "org.scalatest" % "scalatest_2.13" % "3.0.8" % Test
libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.14.0" % Test

scalacOptions := Seq("-unchecked", "-deprecation")
