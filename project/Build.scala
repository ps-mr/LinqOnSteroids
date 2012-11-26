import sbt._
import Keys._

object LoSBuild extends Build {
    lazy val root = Project(id = "los-root",
        base = file("."), settings = Defaults.defaultSettings ++ CompileQuickPlugin.compileQuickSettings) dependsOn(sampleapp)

    lazy val sampleapp = Project(id = "los-sampleapp",
        base = file("sampleapp"))
}

// vim: set ts=4 sw=4 et:
