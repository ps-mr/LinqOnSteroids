import java.io.{File, PrintStream, FileOutputStream}
import org.fusesource.scalate._
import util.ClassPathBuilder
import sbt._

/**
 * Heavily modified from:
 * http://code.google.com/p/scala-integrated-query/source/browse/trunk/generator-src/generator.scala
 * released under a GPLv2 license.
 * User: pgiarrusso
 * Date: 19/2/2012
 */

object Generator {
  val templates = Seq("tupleSupport","BATGenerator")
}

class Generator(scalaVersion: String) {
  import Generator._

  val engine = new TemplateEngine()
  // XXX: workaround to big bug, as discussed here:
  // http://groups.google.com/group/scalate/browse_frm/thread/b17acb9a345badbc/3a9cbc742edf6cda?#3a9cbc742edf6cda
  //In practice, it is quite robust wrt. non-invasive changes to SBT.

  engine.combinedClassPath = true
  val cpBuilder = new ClassPathBuilder
  cpBuilder.addEntry((new File(System.getProperty("user.home")) / ".ivy2" / "cache" / "org.scala-lang" / "scala-library" / "jars" / ("scala-library-" + scalaVersion + ".jar")).absolutePath)
  cpBuilder.addEntry((new File(System.getProperty("user.home")) /".ivy2" / "cache" / "org.scala-lang" / "scalap" / "jars" / "scalap-2.9.1.jar").absolutePath)
  cpBuilder.addLibDir("lib")
  engine.classpath = cpBuilder.classPath

  //This generates (once and for all) _source_ files.
  def renderOnce[A](name: String, inFileName: String = "", outFileName: String = "", args: Map[String, A] = Map[String, A]()) {
    val file = "src" + File.separator + (if (outFileName != "") outFileName else name) + ".scala"
    if (!(new java.io.File(file)).exists())
      renderHelper(name, inFileName, file, args)
  }

  def render[A](outpath: String, name: String, inFileName: String = "", outFileName: String = "", args: Map[String, A] = Map[String, A]()) {
    val file = outpath + File.separator + (if (outFileName != "") outFileName else name) + ".scala"
    println(engine.classpath)
    renderHelper(name, inFileName, file, args)
  }

  def renderHelper[A](name: String, inFileName: String = "", outFileName: String, args: Map[String, A]) {
    val file = (if (inFileName != "") inFileName else name)
    val output = engine.layout("src/main/resources/" + file + ".ssp", args)
    new File(outFileName).getParentFile.mkdirs()
    val outStream = new PrintStream(new FileOutputStream(outFileName))
    outStream.print("/* this file has been auto-generated */\n\n")
    outStream.print(output)
    outStream.close()
  }

  def generate(outPath: String) {
    for (t <- templates)
      render(outPath, t)
    //new scalate.$_scalate_$tupleSupport_ssp()
  }
}
