package ivm.expressiontree

import collection.mutable
import collection.mutable.ArrayBuffer

/**
 * User: pgiarrusso
 * Date: 17/7/2012
 */

trait Compiled[T] {
  def result: T
}

case class CSPVar(name: String, typ: ClassManifest[_])

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

  def compileInternal[A: ClassManifest](exp: Exp[A]) = {
    if (this.compiler eq null)
      setupCompiler()

    //val className = "staged$" + compileCount

    //val staticData = codegen.emitSource(f, className, new PrintWriter(source))
    //val sourceStr = source.toString
    val (sourceStr, staticData, className) = Compile.compileInternal(exp)

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

  def compile[A: ClassManifest](exp: Exp[A]): A = {
    val (cls, staticData) = compileInternal(exp)
    val cons = cls.getConstructor(staticData.map(_._1.erasure):_*)

    val obj: A = cons.newInstance(staticData.map(_._2.asInstanceOf[AnyRef]):_*).asInstanceOf[A]
    obj
  }
}

object Compile {
  private val codeCache = new ScalaThreadLocal(mutable.Map[String, Class[_]]())
  private val map = new ScalaThreadLocal(ArrayBuffer[(Any, CSPVar)]())
  def addVar[T: ClassManifest](node: Const[T]) = {
    val name = "x" + varId()
    map.get() += (node.x -> CSPVar(name, classManifest[T]))
    name
  }
  val classId = new Util.GlobalIDGenerator
  //private val varId = new ScalaThreadLocal(0)
  val varId = new Util.ThreadLocalIDGenerator

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

  def compileInternal[T: ClassManifest](e: Exp[T]): (String, Seq[(ClassManifest[_], Any)], String) = {
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

  def compile[T: ClassManifest](e: Exp[T]): String = {
    val (code, _, _) = compileInternal(e)
    code
  }

  private def invokeCompiler(code: String): Class[_] = null

  def toValue[T: ClassManifest](e: Exp[T]): String = {
    val (code, staticData, _) = compileInternal(e)
    val clazz = invokeCompiler(code)

    code
  }
}
