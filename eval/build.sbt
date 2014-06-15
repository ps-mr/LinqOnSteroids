import AssemblyKeys._
import com.typesafe.startscript.StartScriptPlugin

seq(StartScriptPlugin.startScriptForJarSettings: _*)

assemblySettings

mainClass in assembly := Some("performancetests.opaltests.FindBugsAnalyses")

test in assembly := {}

mergeStrategy in assembly <<= (mergeStrategy in assembly) { old =>
  {
    case "rootdoc.txt"     => MergeStrategy.discard
    case x => old(x)
  }
}
