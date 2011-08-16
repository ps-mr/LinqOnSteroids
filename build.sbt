name := "LinqOnSteroids"

version := "0.1"

scalaVersion := "2.9.0-1"

parallelExecution in Test := false

libraryDependencies += "junit" % "junit" % "4.8.2" % "test->default"

libraryDependencies += "org.scalatest" % "scalatest_2.9.0" % "1.6.1"

libraryDependencies += "com.novocode" % "junit-interface" % "0.5" % "test->default"

//addCompilerPlugin("org.scala-tools.sxr" %% "sxr" % "0.2.8-SNAPSHOT")

//scalacOptions <+= scalaSource in Compile map { "-P:sxr:base-directory:" + _.getAbsolutePath }
