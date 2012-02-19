resolvers += Classpaths.typesafeResolver

addSbtPlugin("com.typesafe.sbtscalariform" % "sbtscalariform" % "0.3.0")

addSbtPlugin("de.johoop" % "findbugs4sbt" % "1.1.3")

//addSbtPlugin("com.typesafe.startscript" % "xsbt-start-script-plugin" % "0.5.0")

libraryDependencies += "org.slf4j" % "slf4j-simple" % "1.6.1"

//libraryDependencies <+= sbtVersion(v => "com.mojolly.scalate" %% "xsbt-scalate-generator" % (v + "-0.1.5"))

libraryDependencies += "org.fusesource.scalate" % "scalate-core" % "1.5.3"

//They should fix their plugin for one to use:
//addSbtPlugin("com.mojolly.scalate" %% "xsbt-scalate-generator" % "0.1.5")

//Indeed, this line works!
//addSbtPlugin("com.mojolly.scalate" %% "xsbt-scalate-generator" % "0.1.1")
