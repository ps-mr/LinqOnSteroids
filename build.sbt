import java.io.FileWriter
import scalariform.formatter.preferences._
import AssemblyKeys._
//import de.johoop.findbugs4sbt.FindBugs._
//import com.typesafe.startscript.StartScriptPlugin

name := "LinqOnSteroids"

version := "0.3-SNAPSHOT"

//scalaVersion in ThisBuild := "2.9.2"
scalaVersion in ThisBuild := "2.10.0-M5"

parallelExecution in Test := false

libraryDependencies <+= scalaVersion (ver => "org.scala-lang" % "scala-compiler" % ver)

libraryDependencies <+= scalaVersion (ver => "org.scala-lang" % "scala-reflect" % ver)

libraryDependencies += "junit" % "junit" % "4.8.2" % "test->default"

//We can't restrict this to "test" as long as FindBugsAnalysis is in main, including its "testing" part.
//libraryDependencies += "org.scalatest" %% "scalatest" % "1.7.1"// % "test"
libraryDependencies += "org.scalatest" % "scalatest_2.10.0-M5" % "1.9-2.10.0-M5-B2"

resolvers in ThisBuild += "Local Maven Repository" at "file://"+Path.userHome.absolutePath+"/.m2/repository"

libraryDependencies in ThisBuild += "de.tud.cs.st" %% "bat-core" % "1.0.0-SNAPSHOT"

//libraryDependencies += "org.scalaz" %% "scalaz-core" % "6.0.4"

scalacOptions ++= Seq("-unchecked", "-deprecation")

scalacOptions ++= Seq("-feature", "-language:implicitConversions",
  "-language:higherKinds", "-language:existentials")

//scalacOptions += "-explaintypes"

//scalacOptions += "-optimise"

//scalacOptions += "-Yinline"

//addCompilerPlugin("org.scala-tools.sxr" %% "sxr" % "0.2.8-SNAPSHOT")

//scalacOptions <+= scalaSource in Compile map { "-P:sxr:base-directory:" + _.getAbsolutePath }

//JUnit integration. It is disabled because our tests are both JUnit tests and
//ScalaTest ones, so it would run again the same tests.
//This integration would be useful if we ever used pure JUnit
//tests.
//libraryDependencies += "com.novocode" % "junit-interface" % "0.5" % "test->default"

// This enables test integration with Jenkins, following instructions from https://github.com/ijuma/junit_xml_listener#readme
testListeners <<= target.map(t => Seq(new eu.henkelmann.sbt.JUnitXmlTestsListener(t.getAbsolutePath)))

seq(defaultScalariformSettings: _*)

ScalariformKeys.preferences := FormattingPreferences().
  setPreference(IndentWithTabs, false).
  setPreference(DoubleIndentClassDeclaration, true).
  setPreference(AlignSingleLineCaseStatements, true).
  setPreference(MultilineScaladocCommentsStartOnFirstLine, true).
  setPreference(PlaceScaladocAsterisksBeneathSecondAsterisk, true).
  setPreference(PreserveSpaceBeforeArguments, true).
  setPreference(AlignParameters, true)

//Add support for FindBugs
//seq(findbugsSettings : _*)

// define the statements initially evaluated when entering 'console', 'console-quick', or 'console-project'
initialCommands := """
  import System.{nanoTime => now}
  def time[T](f: => T): T = {
    val start = now
    try { f } finally { println("Elapsed: " + (now - start)/(1000.0 * 1000.0) + " ms") }
  }
"""

// set the initial commands when entering 'console' or 'console-quick', but not 'console-project'
initialCommands in console := """
    import ivm._
    import expressiontree._
    import Lifting._
    import optimization._
    import squopt.imports._
    import performancetests.Benchmarking
    val bench = new Benchmarking { override val debugBench = false }
"""

initialCommands in (Test, console) := """
    import ivm._
    import expressiontree._
    import Lifting._
    import optimization._
    import tests._
    import performancetests._
    import opaltests._
//
    import de.tud.cs.st.bat
    import bat.resolved._
    import analyses._
//
    import reader.Java6Framework
"""

sourceGenerators in Compile <+= (sourceManaged in Compile, baseDirectory) map { (dir, baseDir) =>
  val sbtScalaVersion = "2.9.2"
  val gen = new Generator(sbtScalaVersion)
  dir.mkdirs()
  if (!(dir.exists() && dir.isDirectory())) {
    scala.Console.err.printf("Failure creating output directory %s\n", dir)
  }
  val verFile = dir / "version.scala"
  val gitVersion = "git describe --always --dirty --abbrev=40".!!
  val writer = new FileWriter(verFile)
  try {
    writer write ("""package ivm
object GitVersion {
  val version = "%s"
}
""" format gitVersion.trim)
  } finally {
    writer.close()
  }
  (for {
    base <- Generator.templates
    file = dir / (base + ".scala")
  } yield {
    if (!file.exists() || (baseDir / "src" / "main" / "resources" / (base + ".ssp") newerThan file)) {
      printf("Generating %s\n", file)
      gen.render(dir.absolutePath, base)
    }
    file
  }) :+ verFile
}

////Generate start scripts
////seq(StartScriptPlugin.startScriptForClassesSettings: _*)
//seq(StartScriptPlugin.startScriptForJarSettings: _*)
//
//mainClass in Test := Some("performancetests.opaltests.FindBugsAnalyses")
//
//mainClass in Compile := Some("performancetests.opaltests.FindBugsAnalyses")

//generate fat JAR
assemblySettings

mainClass in assembly := Some("performancetests.opaltests.FindBugsAnalyses")

test in assembly := {}
