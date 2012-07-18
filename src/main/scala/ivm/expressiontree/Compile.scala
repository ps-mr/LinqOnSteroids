package ivm.expressiontree

import collection.mutable

/**
 * User: pgiarrusso
 * Date: 17/7/2012
 */

trait Compiled[T] {
  def result: T
}

object ScalaCompile {
  /*
   * Derived from
   * virtualization-lms-core/src/internal/ScalaCompile.scala, branch delite-develop,
   * commit 8f4cabbb2605f246496c084b765d1df95f903f57.
   */
  import java.io.{Console => _, _}

  import scala.tools.nsc._
  import scala.tools.nsc.util._
  import scala.tools.nsc.reporters._
  import scala.tools.nsc.io._

  import scala.tools.nsc.interpreter.AbstractFileClassLoader
  var compiler: Global = _
  var reporter: ConsoleReporter = _

  def setupCompiler() = {
    /*
      output = new ByteArrayOutputStream()
      val writer = new PrintWriter(new OutputStreamWriter(output))
    */
    val settings = new Settings()

    settings.classpath.value = this.getClass.getClassLoader match {
      case ctx: java.net.URLClassLoader => ctx.getURLs.map(_.getPath).mkString(":")
      case _ => System.getProperty("java.class.path")
    }
    settings.bootclasspath.value = Predef.getClass.getClassLoader match {
      case ctx: java.net.URLClassLoader => ctx.getURLs.map(_.getPath).mkString(":")
      case _ => System.getProperty("sun.boot.class.path")
    }
    settings.encoding.value = "UTF-8"
    settings.outdir.value = "."
    settings.extdirs.value = ""
    //settings.verbose.value = true
    // -usejavacp needed on windows?

    reporter = new ConsoleReporter(settings, null, new PrintWriter(Console.err))//writer
    compiler = new Global(settings, reporter)
  }

  def invokeCompiler[T: ClassManifest](sourceStr: String, className: String) = {
    if (this.compiler eq null)
      setupCompiler()

    val compiler = this.compiler
    val run = new compiler.Run

    val fileSystem = new VirtualDirectory("<vfs>", None)
    compiler.settings.outputDirs.setSingleOutput(fileSystem)
    //      compiler.genJVM.outputDir = fileSystem

    //Apparently, this compiler setup does not load the files from the executing application. Prepending the "correct"
    //source code works fine enough for now!
    val prefix = "import ivm.expressiontree.Compiled\n"
    run.compileSources(List(new util.BatchSourceFile("<stdin>", prefix + sourceStr)))
    reporter.printSummary()

    if (!reporter.hasErrors)
      println("compilation: ok")
    else
      println("compilation: had errors")

    reporter.reset()
    //output.reset

    val parent = this.getClass.getClassLoader
    val loader = new AbstractFileClassLoader(fileSystem, parent)

    val cls: Class[_] = loader.loadClass(className)
    cls
  }
}

object Compile {
  import CrossStagePersistence.{map, varId}

  def manifestToString[T](m: ClassManifest[T]): String = {
    val str = m.toString
    if (str endsWith ".type") //Workaround bug: names for singleton types are not fully qualified!
      //Remove final '$' from class name.
      m.erasure.getName.replaceFirst("""\$$""", "") + ".type"
    else
      str
  }
  val classId = new Util.GlobalIDGenerator

  //Cache compilation results.
  private val codeCache = new ScalaThreadLocal(mutable.Map[String, Class[_]]())
  def cachedInvokeCompiler[T: ClassManifest](prefix: String, restSourceStr: String, className: String) =
    codeCache.get().getOrElseUpdate(restSourceStr, ScalaCompile.invokeCompiler(prefix + restSourceStr, className))

  //*Reset methods are just (or mostly?) for testing {{{
  def precompileReset() {
    map.get().clear()
    varId.localReset()
  }

  def reset() {
    precompileReset()
    codeCache.get().clear()
    classId.reset()
  }

  def completeReset() {
    codeCache.get().clear()
    reset()
  }
  //}}}

  /*private[expressiontree]*/ def emitSourceInternal[T: ClassManifest](e: Exp[T]): (String, String, Seq[(ClassManifest[_], Any)], String) = {
    precompileReset()
    val name = "Outclass" + classId()
    val typ = classManifest[T]
    val body = e.toCode
    val declValues = (map.get().toSeq map {
      case (value, CSPVar(memberName, memberType)) =>
        ("val %s: %s" format (memberName, manifestToString(memberType)), (memberType, value))
    })
    val (decls, staticData) = declValues.unzip[String, (ClassManifest[_], Any)]
    val declsStr = decls mkString ", "
    val prefix = "class %s" format name
    val rest =
      """(%s) extends Compiled[%s] {
        |  override def result = %s
        |}""".stripMargin format (declsStr, manifestToString(typ), body)
    (prefix, rest, staticData, name)
  }

  //Mostly for testing
  def emitSource[T: ClassManifest](e: Exp[T]): String = {
    val res = emitSourceInternal(e)
    res._1 + res._2
  }


  def toValue[T: ClassManifest](exp: Exp[T]): T = {
    val (prefix, restSourceStr, staticData, className) = emitSourceInternal(exp)

    val cls = cachedInvokeCompiler(prefix, restSourceStr, className)

    val cons = cls.getConstructor(staticData.map(_._1.erasure):_*)
    val obj: T = cons.newInstance(staticData.map(_._2.asInstanceOf[AnyRef]):_*).asInstanceOf[Compiled[T]].result
    obj
  }
}
