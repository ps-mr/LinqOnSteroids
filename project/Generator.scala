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
  val templates = Seq[String]()
}

class Generator(scalaLibraryPath: File) {
  import Generator._

  val engine = new TemplateEngine()

  // XXX: workaround to big bug, as discussed here:
  // https://groups.google.com/d/msg/scalate/sXrLmjRbrbw/36M_U5I7p7UJ
  engine.combinedClassPath = true
  val cpBuilder = new ClassPathBuilder
  cpBuilder.addEntry(scalaLibraryPath.absolutePath)
  //To add a directory use:
  //  cpBuilder.addLibDir("lib")
  //However, "lib" is already available as part of the classpath inherited from
  //the JVM running the generator.
  engine.classpath = cpBuilder.classPath

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
    val output = engine.layout("src/main/resources/" + file + ".ssp", args)
    new File(outFileName).getParentFile.mkdirs()
    val outStream = new PrintStream(new FileOutputStream(outFileName))
    outStream.print("/* this file has been auto-generated */\n\n")
    outStream.print(output)
    outStream.close()
  }
}
