resolvers += Classpaths.typesafeResolver

addSbtPlugin("com.typesafe.sbtscalariform" % "sbtscalariform" % "0.4.0")

//resolvers in ThisBuild += "Local Maven Repository" at "file://"+Path.userHome.absolutePath+"/.m2/repository"

//libraryDependencies in ThisBuild += "de.tud.cs.st" %% "bat-core" % "1.0.0-SNAPSHOT"

//addSbtPlugin("de.johoop" % "findbugs4sbt" % "1.1.6")

libraryDependencies += "org.slf4j" % "slf4j-simple" % "1.6.1"


//addSbtPlugin("com.typesafe.startscript" % "xsbt-start-script-plugin" % "0.5.2")

//This is enough to build our own generator:
libraryDependencies += "org.fusesource.scalate" % "scalate-core" % "1.5.3"
//so we don't need this:

//libraryDependencies <+= sbtVersion(v => "com.mojolly.scalate" %% "xsbt-scalate-generator" % (v + "-0.1.5"))
//BTW, that's a hack.
//They should fix their plugin for one to use:
//addSbtPlugin("com.mojolly.scalate" %% "xsbt-scalate-generator" % "0.1.5")

//Indeed, this line works!
//addSbtPlugin("com.mojolly.scalate" %% "xsbt-scalate-generator" % "0.1.1")

libraryDependencies += "org.scala-lang" % "scalap" % "2.9.2"

//Support for sbt-idea.
//resolvers += "sbt-idea-repo" at "http://mpeltonen.github.com/maven/"

//addSbtPlugin("com.github.mpeltonen" % "sbt-idea" % "1.0.0")
resolvers += "Sonatype snapshots" at "http://oss.sonatype.org/content/repositories/snapshots/"

addSbtPlugin("com.github.mpeltonen" % "sbt-idea" % "1.1.0-SNAPSHOT")

addSbtPlugin("com.eed3si9n" % "sbt-assembly" % "0.8.3")
