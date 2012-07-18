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
  import java.io._

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

    reporter = new ConsoleReporter(settings, null, new PrintWriter(System.out))//writer
    compiler = new Global(settings, reporter)
  }

  def invokeCompiler[T: ClassManifest](exp: Exp[T]) = {
    if (this.compiler eq null)
      setupCompiler()

    //val className = "staged$" + compileCount

    //val staticData = codegen.emitSource(f, className, new PrintWriter(source))
    //val sourceStr = source.toString
    val (sourceStr, staticData, className) = Compile.emitSourceInternal(exp)

    val compiler = this.compiler
    val run = new compiler.Run

    val fileSystem = new VirtualDirectory("<vfs>", None)
    compiler.settings.outputDirs.setSingleOutput(fileSystem)
    //      compiler.genJVM.outputDir = fileSystem

    run.compileSources(List(new util.BatchSourceFile("<stdin>", sourceStr)))
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
    (cls, staticData)
  }
}

object Compile {
  import CrossStagePersistence.{map, varId}

  private val codeCache = new ScalaThreadLocal(mutable.Map[String, Class[_]]())

  val classId = new Util.GlobalIDGenerator
  //private val varId = new ScalaThreadLocal(0)

  //Just (or mostly?) for testing.
  def reset() {
    codeCache.get().clear()
    map.get().clear()
    classId.reset()
  }
  def precompileReset() {
    map.get().clear()
    varId.localReset()
  }

  def emitSourceInternal[T: ClassManifest](e: Exp[T]): (String, Seq[(ClassManifest[_], Any)], String) = {
    precompileReset()
    val name = "Outclass" + classId()
    val typ = classManifest[T]
    val body = e.toCode
    val declValues = (map.get().toSeq map {
      case (value, CSPVar(memberName, memberType)) =>
        ("val %s: %s" format (memberName, memberType), (memberType, value))
    })
    val (decls, staticData) = declValues.unzip[String, (ClassManifest[_], Any)]
    val declsStr = decls mkString ", "
    val res =
      """class %s(%s) extends Compiled[%s] {
        |  override def result = %s
        |}""".stripMargin format (name, declsStr, typ, body)
    //Compile res and cache the result.
    (res, staticData, name)
  }

  def emitSource[T: ClassManifest](e: Exp[T]): String =
    emitSourceInternal(e)._1

  def toValue[T: ClassManifest](exp: Exp[T]): T = {
    val (cls, staticData) = ScalaCompile.invokeCompiler(exp)
    val cons = cls.getConstructor(staticData.map(_._1.erasure):_*)

    val obj: T = cons.newInstance(staticData.map(_._2.asInstanceOf[AnyRef]):_*).asInstanceOf[T]
    obj
  }
}
