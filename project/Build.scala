import sbt._
import Keys._

object LoSBuild extends Build {
    lazy val root = Project(id = "los-root",
        base = file("."), settings = Defaults.defaultSettings ++ CompileQuickPlugin.compileQuickSettings)
}

// vim: set ts=4 sw=4 et:
