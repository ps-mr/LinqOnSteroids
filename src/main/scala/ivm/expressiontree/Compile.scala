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

case class CSPVar(name: String, typ: String)

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

  var compileCount = 0

  /*
  def compile[A,B](f: Exp[A] => Exp[B])(implicit mA: Manifest[A], mB: Manifest[B]): A=>B = {
    if (this.compiler eq null)
      setupCompiler()

    val className = "staged$" + compileCount
    compileCount += 1

    val source = new StringWriter()
    val staticData = codegen.emitSource(f, className, new PrintWriter(source))

    val compiler = this.compiler
    val run = new compiler.Run

    val fileSystem = new VirtualDirectory("<vfs>", None)
    compiler.settings.outputDirs.setSingleOutput(fileSystem)
    //      compiler.genJVM.outputDir = fileSystem

    run.compileSources(List(new util.BatchSourceFile("<stdin>", source.toString)))
    reporter.printSummary()

    if (!reporter.hasErrors)
      println("compilation: ok")
    else
      println("compilation: had errors")

    reporter.reset
    //output.reset

    val parent = this.getClass.getClassLoader
    val loader = new AbstractFileClassLoader(fileSystem, this.getClass.getClassLoader)

    val cls: Class[_] = loader.loadClass(className)
    val cons = cls.getConstructor(staticData.map(_._1.tp.erasure):_*)

    val obj: A=>B = cons.newInstance(staticData.map(_._2.asInstanceOf[AnyRef]):_*).asInstanceOf[A=>B]
    obj
  }
  */
}

object Compile {
  private val codeCache = new ScalaThreadLocal(mutable.Map[String, Class[_]]())
  private val map = new ScalaThreadLocal(ArrayBuffer[(Any, CSPVar)]())
  def addVar[T: ClassManifest](node: Const[T]) = {
    val name = "x" + varId()
    map.get() += (node.x -> CSPVar(name, classManifest[T].toString))
    name
  }
  val classId = new Util.GlobalIDGenerator
  //private val varId = new ScalaThreadLocal(0)
  val varId = new Util.ThreadLocalIDGenerator

  //Just (or mostly?) for testing.
  def reset() {
    codeCache.get().clear()
    map.get().clear()
    varId.localReset()
    classId.reset()
  }

  def compile[T: ClassManifest](e: Exp[T]) = {
    val name = "Outclass" + classId()
    val typ = classManifest[T]
    map.get().clear()
    val body = e.toCode
    val declValues = (map.get().toSeq map {
      case (value, CSPVar(memberName, memberType)) =>
        ("val %s: %s" format (memberName, memberType), value)
    })
    val (decls, values) = declValues.unzip
    val declsStr = decls mkString ", "
    val res =
      """class %s(%s) extends Compiled[%s] {
        |  override def result = %s
        |}""".stripMargin format (name, declsStr, typ, body)
    //Compile res and cache the result.
    res
  }
}
