resolvers += Classpaths.typesafeResolver

addSbtPlugin("com.typesafe.sbtscalariform" % "sbtscalariform" % "0.4.0")

//resolvers in ThisBuild += "Local Maven Repository" at "file://"+Path.userHome.absolutePath+"/.m2/repository"

//libraryDependencies in ThisBuild += "de.tud.cs.st" %% "bat-core" % "1.0.0-SNAPSHOT"

//addSbtPlugin("de.johoop" % "findbugs4sbt" % "1.1.6")

libraryDependencies += "org.slf4j" % "slf4j-simple" % "1.6.1"


addSbtPlugin("com.typesafe.startscript" % "xsbt-start-script-plugin" % "0.5.3")

//resolvers += "FuseSource Public Repository" at "http://repo.fusesource.com/nexus/content/repositories/public"
//This is enough to build our own generator:
libraryDependencies += "org.fusesource.scalate" % "scalate-core_2.9" % "1.6.1"
//(Note the brokenness of using _2.9).
//so we don't need this:
//addSbtPlugin("com.mojolly.scalate" % "xsbt-scalate-generator" % "0.4.2")

libraryDependencies += "org.scala-lang" % "scalap" % "2.9.2"

//Support for sbt-idea.
addSbtPlugin("com.github.mpeltonen" % "sbt-idea" % "1.1.0")

//resolvers += "Sonatype snapshots" at "http://oss.sonatype.org/content/repositories/snapshots/"

//addSbtPlugin("com.github.mpeltonen" % "sbt-idea" % "1.2.0-SNAPSHOT")

//Support for sbt-assembly
addSbtPlugin("com.eed3si9n" % "sbt-assembly" % "0.8.3")

//addSbtPlugin("com.typesafe.sbteclipse" % "sbteclipse-plugin" % "2.1.0")
