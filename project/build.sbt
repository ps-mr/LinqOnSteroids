resolvers in ThisBuild += "Local Maven Repository" at "file://"+Path.userHome.absolutePath+"/.m2/repository"

libraryDependencies in ThisBuild += "de.tud.cs.st" % "bat-core" % "1.0.0-SNAPSHOT"
