import sbt._
import Keys._

object LoSBuild extends Build {
    lazy val sampleapp = Project(id = "los-sampleapp",
        base = file("sampleapp"))

    lazy val root = Project(id = "los-root",
        base = file("."), settings = Defaults.defaultSettings ++ CompileQuickPlugin.compileQuickSettings) dependsOn(sampleapp)

    lazy val eval = Project(id = "los-eval",
        base = file("eval")) dependsOn(root % "compile->compile;test->test")

    lazy val macros = Project(id = "los-macros",
        base = file("macros")) dependsOn(root)

    lazy val rfp = Project(id = "los-rfp",
        base = file("rfp")) dependsOn(root)

    lazy val global = Project(id = "los-global",
        base = file("global")) aggregate(sampleapp, root, eval, macros, rfp)
}

// vim: set ts=4 sw=4 et:
