import sbt._
import sbt.Keys._

object HuffmanBuild extends Build {

  lazy val huffman = Project(
    id = "huffman",
    base = file("."),
    settings = Project.defaultSettings ++ Seq(
      name := "Huffman",
      organization := "io.das",
      version := "0.1-SNAPSHOT",
      scalaVersion := "2.10.0",
      libraryDependencies += "org.scalatest" % "scalatest_2.10.0" % "2.0.M5" % "test",
      scalacOptions ++= Seq("-unchecked", "-deprecation")
    )
  )
}
