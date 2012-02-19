import scalariform.formatter.preferences._
import de.johoop.findbugs4sbt.FindBugs._
//import com.typesafe.startscript.StartScriptPlugin

//import com.mojolly.scalate.ScalatePlugin._

name := "LinqOnSteroids"

version := "0.1"

scalaVersion := "2.9.1"

parallelExecution in Test := false

libraryDependencies += "junit" % "junit" % "4.8.2" % "test->default"

libraryDependencies += "org.scalatest" % "scalatest_2.9.0" % "1.6.1"

//scalacOptions ++= Seq("-unchecked", "-deprecation", "-explaintypes")

scalacOptions ++= Seq("-unchecked", "-deprecation", "-explaintypes", "-optimise")

//scalacOptions ++= Seq("-unchecked", "-deprecation", "-explaintypes", "-optimise", "-Yinline")

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
seq(findbugsSettings : _*)

// define the statements initially evaluated when entering 'console', 'console-quick', or 'console-project'
initialCommands := """
  import System.{currentTimeMillis => now}
  def time[T](f: => T): T = {
    val start = now
    try { f } finally { println("Elapsed: " + (now - start)/1000.0 + " s") }
  }
"""

// set the initial commands when entering 'console' or 'console-quick', but not 'console-project'
initialCommands in console := """
    import ivm._
    import expressiontree._
    import optimization._
"""

sourceGenerators in Compile <+= (sourceManaged in Compile, baseDirectory, scalaVersion) map { (dir, baseDir, scalaVer) =>
  val gen = new Generator(scalaVer)
  for {
    base <- Generator.templates
    file = dir / (base + ".scala")
  } yield {
    if (!file.exists() || (baseDir / "src" / "main" / "resources" / (base + ".ssp") newerThan file))
      gen.generate(dir.absolutePath)
    //XXX relies on "stage" having been run
    //Process(baseDir / "target" / "start", dir.getAbsolutePath()).run.exitCode()
    file
  }
}
