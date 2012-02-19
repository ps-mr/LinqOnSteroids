import scalariform.formatter.preferences._
import de.johoop.findbugs4sbt.FindBugs._
import com.mojolly.scalate.ScalatePlugin._
import com.typesafe.startscript.StartScriptPlugin

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

//Scalate settings
libraryDependencies += "org.fusesource.scalate" % "scalate-core" % "1.5.3"

seq(scalateSettings: _*)

scalateTemplateDirectory in Compile <<= (baseDirectory) { _ / "src/main/resources" }

//Generate start scripts
seq(StartScriptPlugin.startScriptForClassesSettings: _*)

mainClass := Some("ivm.generation.Generator")

//Add "sourceGenerators in Compile <+= "
