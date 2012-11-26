import sbt._
import Keys._
import Compiler._

object CompileQuickPlugin extends Plugin {
  lazy val compileQuick = InputKey[Unit]("compile-quick", "forces compilation of *only* the given file(s) with a the compiler options in scalac-options(for compile-quick)")

  lazy val compileQuickSettings: Seq[Project.Setting[_]] = compileQuickSettings(Compile) ++ compileQuickSettings(Test)

  def compileQuickSettings(conf: Configuration): Seq[Project.Setting[_]] = Seq(    
    scalacOptions in compileQuick <<= scalacOptions,
    // TODO Parser to tab-complete source files
    compileQuick <<= inputTask { (argTask: TaskKey[Seq[String]]) =>
      (argTask, scalacOptions in compileQuick, Keys.compilers, compileInputs in Compile, dependencyClasspath in conf, 
        classDirectory in conf, streams) map { 
          (args: Seq[String], options: Seq[String], c: Compilers, i: Inputs, cp, output, s) =>
            val sources = args.map(file)
            // TODO (?) use a different output directory to discard the compiled files
            c.scalac(sources, noChanges, cp.map(_.data), output, options, noopCallback, 1000, i.incSetup.cache, s.log)
        }
    }
  )
  // This should mean that there are no changes to external dependencies - is that correct?
  val noChanges = new xsbti.compile.DependencyChanges {
    def isEmpty = true
    def modifiedBinaries = Array()
    def modifiedClasses = Array()
  }

  import xsbti._
  object noopCallback extends xsbti.AnalysisCallback {
    def beginSource(source: File) {}

    def generatedClass(source: File, module: File, name: String) {}

    def api(sourceFile: File, source: xsbti.api.SourceAPI) {}

    def sourceDependency(dependsOn: File, source: File) {}

    def binaryDependency(binary: File, name: String, source: File) {}

    def endSource(sourcePath: File) {}

    def problem(what: String, pos: Position, msg: String, severity: Severity, reported: Boolean) {}
  }
}
