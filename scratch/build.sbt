scalaVersion := "2.10.0-M6"

name := "scratch"

version := "0.0"

scalacOptions ++= Seq("-unchecked", "-deprecation", "-explaintypes",
  "-language:experimental.macros")

libraryDependencies <+= scalaVersion apply ("org.scala-lang" % "scala-reflect" % _)

initialCommands in console := """
    import scala.reflect.runtime.{universe => u}
    //import u.{Apply,Select,Ident}
    import u._
    val a = 1
    val b = 2
"""
