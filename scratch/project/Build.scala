import sbt._
import Keys._

object Scract extends Build {
    lazy val root = Project(id = "macros-client",
        base = file(".")) dependsOn(macros)

    lazy val macros = Project(id = "macros",
        base = file("macros"))
}

// vim: set ts=4 sw=4 et:
