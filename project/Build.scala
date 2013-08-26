import sbt._
import Keys._

object LoSBuild extends Build {
    lazy val root = Project(id = "los-root",
        base = file("."))
}

// vim: set ts=4 sw=4 et:
