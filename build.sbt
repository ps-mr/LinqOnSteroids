name := "LinqOnSteroids"

version := "0.3-SNAPSHOT"

scalaVersion in ThisBuild := "2.10.2"

libraryDependencies <+= scalaVersion (ver => "org.scala-lang" % "scala-compiler" % ver)

libraryDependencies <+= scalaVersion (ver => "org.scala-lang" % "scala-reflect" % ver)

libraryDependencies <+= scalaVersion (ver => "org.scala-lang" % "scala-actors" % ver)

scalacOptions in ThisBuild ++= Seq("-unchecked", "-deprecation")

//scalacOptions ++= Seq("-Xprint:typer")

scalacOptions in ThisBuild ++= Seq("-feature", "-language:implicitConversions",
  "-language:higherKinds", "-language:existentials")

scalacOptions in ThisBuild ++= Seq("-Ywarn-adapted-args", "-Ywarn-inaccessible", 
  "-Ywarn-nullary-override", "-Ywarn-nullary-unit", "-Ywarn-numeric-widen") //All -Ywarn except -Ywarn-dead-code, which gives tons
//of false positives, and -Ywarn-value-discard, which doesn't sound relevant.
