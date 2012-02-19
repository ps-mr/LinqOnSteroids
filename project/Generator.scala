import java.io.{File, PrintStream, FileOutputStream}
import org.fusesource.scalate._
import sbt._

/**
 * User: pgiarrusso
 * Date: 19/2/2012
 */

object Generator {
  val templates = Seq("tupleSupport")
}

class Generator(scalaVersion: String) {
  import Generator._

  val engine = new TemplateEngine()
  // XXX: workaround to big bug, as discussed here:
  // http://groups.google.com/group/scalate/browse_frm/thread/b17acb9a345badbc/3a9cbc742edf6cda?#3a9cbc742edf6cda
  //In practice, it is quite robust wrt. non-invasive changes to SBT.
  engine.combinedClassPath = true
  engine.classpath = (new File(System.getProperty("user.home")) / ".ivy2" / "cache" / "org.scala-lang" / "scala-library" / "jars" / ("scala-library-" + scalaVersion + ".jar")).absolutePath

  //ScalaInstance(scalaVersion)
  //ScalaInstance()
  /*
  {
    val state: State = State.
    val extracted: Extracted = Project.extract(state)
    //val scope: Scope
    val scala-version: Option[String] = Keys.scalaVersion in extracted.currentRef get extracted.structure.data
  }
  //ScalaInstance()
  */

  //This generates (once and for all) _source_ files.
  def renderOnce[A](name: String, inFileName: String = "", outFileName: String = "", args: Map[String, A] = Map[String, A]()) {
    val file = "src" + File.separator + (if (outFileName != "") outFileName else name) + ".scala"
    if (!(new java.io.File(file)).exists())
      renderHelper(name, inFileName, file, args)
  }

  def render[A](outpath: String, name: String, inFileName: String = "", outFileName: String = "", args: Map[String, A] = Map[String, A]()) {
    val file = outpath + File.separator + (if (outFileName != "") outFileName else name) + ".scala"
    renderHelper(name, inFileName, file, args)
  }

  def renderHelper[A](name: String, inFileName: String = "", outFileName: String, args: Map[String, A]) {
    val file = (if (inFileName != "") inFileName else name)
    //val output = engine.layout("templates/" + (file) + ".ssp", args)
    val output = engine.layout("src/main/resources/" + file + ".ssp", args)
    new File(outFileName).getParentFile.mkdirs()
    val outStream = new PrintStream(new FileOutputStream(outFileName))
    outStream.print("/* this file has been auto-generated */\n\n")
    outStream.print(output)
    outStream.close()
  }

  def generate(outPath: String) {
    try {
      //render("tupleSupport", "src/main/scala/0_generated/")
      for (t <- templates)
        render(outPath, t)
      //new scalate.$_scalate_$tupleSupport_ssp()
    } finally {
      //System.exit(0) //Ensure that the process actually exits - scalate has a tendency to not do so.
    }
  }

  def main(args: Array[String]) {
    generate(args(0))
  }
}
