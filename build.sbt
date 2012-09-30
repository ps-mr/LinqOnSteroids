import java.io.FileWriter
import scalariform.formatter.preferences._
import AssemblyKeys._
//import de.johoop.findbugs4sbt.FindBugs._
import com.typesafe.startscript.StartScriptPlugin

name := "LinqOnSteroids"

version := "0.3-SNAPSHOT"

//scalaVersion in ThisBuild := "2.9.2"

//scalaVersion in ThisBuild := "2.10.0-M7"

resolvers in ThisBuild += Resolver.sonatypeRepo("snapshots")

scalaVersion in ThisBuild := "2.10.0-SNAPSHOT"

////Alternative resolver setup:
////DefaultOptions.addResolvers
////resolvers in ThisBuild += ScalaToolsSnapshots
////resolvers in ThisBuild += "sonatype snapshots" at "https://oss.sonatype.org/content/repositories/snapshots/"

parallelExecution in Test := false

libraryDependencies <+= scalaVersion (ver => "org.scala-lang" % "scala-compiler" % ver)

libraryDependencies <+= scalaVersion (ver => "org.scala-lang" % "scala-reflect" % ver)

libraryDependencies <+= scalaVersion (ver => "org.scala-lang" % "scala-actors" % ver)

libraryDependencies += "junit" % "junit" % "4.8.2" % "test->default"

//We can't restrict this to "test" as long as FindBugsAnalysis is in main, including its "testing" part.
//libraryDependencies += "org.scalatest" %% "scalatest" % "1.7.1"// % "test"
//libraryDependencies <+= scalaVersion (ver => "org.scalatest" % ("scalatest_" + ver) % ("1.9-%s-B2" format ver))
//libraryDependencies += "org.scalatest" % "scalatest" % ("1.9-%s-B1" format "2.10.0-M7") cross CrossVersion.full
//libraryDependencies <+= scalaVersion (ver => "org.scalatest" % "scalatest" % ("1.9-%s-B1" format ver) cross CrossVersion.full)
libraryDependencies += ((ver: String) => "org.scalatest" % ("scalatest_" + ver) % ("1.9-%s-B1" format ver))("2.10.0-M7")

libraryDependencies += "com.google.guava" % "guava" % "13.0"

//Not yet available for 2.10, so use a local build.
//libraryDependencies += "com.github.scopt" %% "scopt" % "2.1.0"

libraryDependencies += "com.google.code.findbugs" % "jsr305" % "1.3.9"

//resolvers in ThisBuild += "Local Maven Repository" at "file://"+Path.userHome.absolutePath+"/.m2/repository"

//libraryDependencies in ThisBuild += "de.tud.cs.st" %% "bat-core" % "1.0.0-SNAPSHOT"

//libraryDependencies in ThisBuild += "de.tud.cs.st" % "bat-core" % "1.0.0-SNAPSHOT" cross CrossVersion.full

//libraryDependencies += "org.scalaz" %% "scalaz-core" % "6.0.4"

scalacOptions ++= Seq("-unchecked", "-deprecation")

//scalacOptions ++= Seq("-Xprint:typer")

scalacOptions ++= Seq("-feature", "-language:implicitConversions",
  "-language:higherKinds", "-language:existentials")

scalacOptions ++= Seq("-Ywarn-adapted-args", "-Ywarn-inaccessible", 
  "-Ywarn-nullary-override", "-Ywarn-nullary-unit", "-Ywarn-numeric-widen") //All -Ywarn except -Ywarn-dead-code, which gives tons
//of false positives, and -Ywarn-value-discard, which doesn't sound relevant.

//scalacOptions += "-explaintypes"

scalacOptions += "-optimise"

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
    import bat.resolved.analyses._
//
    import reader.Java6Framework
"""

//scalaVersion in GlobalScope <<= appConfiguration(_.provider.scalaProvider.version)

sourceGenerators in Compile <+= (sourceManaged in Compile, baseDirectory, appConfiguration) map { (dir, baseDir, appConfig ) =>
  val gen = new Generator(scalaLibraryPath = appConfig.provider.scalaProvider.libraryJar)
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

//Generate start scripts
//seq(StartScriptPlugin.startScriptForClassesSettings: _*)
seq(StartScriptPlugin.startScriptForJarSettings: _*)

mainClass in Compile := Some("performancetests.opaltests.FindBugsAnalyses")

//generate fat JAR
assemblySettings

mainClass in assembly := Some("performancetests.opaltests.FindBugsAnalyses")

test in assembly := {}
