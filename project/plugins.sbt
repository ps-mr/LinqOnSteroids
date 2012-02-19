resolvers += Classpaths.typesafeResolver

addSbtPlugin("com.typesafe.sbtscalariform" % "sbtscalariform" % "0.3.0")

addSbtPlugin("de.johoop" % "findbugs4sbt" % "1.1.3")

libraryDependencies += "org.slf4j" % "slf4j-simple" % "1.6.1"


//Currently not needed:
//addSbtPlugin("com.typesafe.startscript" % "xsbt-start-script-plugin" % "0.5.0")

//This is enough to build our own generator:
libraryDependencies += "org.fusesource.scalate" % "scalate-core" % "1.5.3"
//so we don't need this:

//libraryDependencies <+= sbtVersion(v => "com.mojolly.scalate" %% "xsbt-scalate-generator" % (v + "-0.1.5"))
//BTW, that's a hack.
//They should fix their plugin for one to use:
//addSbtPlugin("com.mojolly.scalate" %% "xsbt-scalate-generator" % "0.1.5")

//Indeed, this line works!
//addSbtPlugin("com.mojolly.scalate" %% "xsbt-scalate-generator" % "0.1.1")
