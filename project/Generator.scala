import java.io.{File, PrintStream, FileOutputStream}
import org.fusesource.scalate._
import sbt._

/**
 * User: pgiarrusso
 * Date: 19/2/2012
 */

object Generator {
  val engine = new TemplateEngine()
  engine.combinedClassPath = true
  engine.classpath = (new File(System.getProperty("user.home")) / ".ivy2" / "cache" / "org.scala-lang" / "scala-library" / "jars" / "scala-library-2.9.1.jar").absolutePath

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

  val templates = Seq("tupleSupport")
  def main(args: Array[String]) {
    try {
      //render("tupleSupport", "src/main/scala/0_generated/")
      for (t <- templates)
        render(args(0), t)
      //new scalate.$_scalate_$tupleSupport_ssp()
    } finally {
      //System.exit(0) //Ensure that the process actually exits - scalate has a tendency to not do so.
    }
  }
}
