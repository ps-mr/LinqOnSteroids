scalaVersion in ThisBuild := "2.10.0-M7"

name := "scratch"

version := "0.0"

scalacOptions in ThisBuild ++= Seq("-unchecked", "-deprecation", "-explaintypes",
  "-language:experimental.macros", "-Dscala.timings=true")

libraryDependencies in ThisBuild <+= scalaVersion apply ("org.scala-lang" % "scala-reflect" % _)

initialCommands in console := """
    import scala.reflect.runtime.{universe => u}
    //import u.{Apply,Select,Ident}
    import u._
    val a = 1
    val b = 2
    import myLib._
    import Macros._
    val c = asExp(a)
"""
