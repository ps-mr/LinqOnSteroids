package ivm.expressiontree

import collection.mutable
import java.util.regex.Pattern
import performancetests.Benchmarking

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
  import scala.tools.nsc.reporters._
  import scala.tools.nsc.io._
  import scala.reflect.internal.util

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

  def invokeCompiler[T: TypeTag](sourceStr: String, className: String): Option[Class[_]] = {
    if (!Benchmarking.debugBench)
      Console.err println sourceStr
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
    val hasErrors = reporter.hasErrors

    reporter.reset()
    //output.reset

    if (!hasErrors) {
      println("compilation: ok")

      val parent = this.getClass.getClassLoader
      val loader = new AbstractFileClassLoader(fileSystem, parent)

      val cls: Class[_] = loader.loadClass(className)
      Some(cls)
    } else {
      println("compilation: had errors")
      None
    }
  }
}

object Compile {
  import CrossStagePersistence.{map, varId}

  def manifestToString[T](m: TypeTag[T]): String = {
    val str = m.tpe.toString
    if (str contains "_$")
      str replaceAll ("""_\$\d+""", "_")
    else
      str
    /*if (str endsWith ".type") //Workaround bug: names for singleton types are not fully qualified!
      //Remove final '$' from class name.
      m.runtimeClass.getName.replaceFirst("""\$$""", "") + ".type"
    else if (str startsWith "ivm.collections.TypeMapping[<?>, <?>, ")
      //Manifests don't support type constructors, but TypeTags do! Yeah!
      str.replaceFirst(Pattern.quote("<?>, <?>"), "Traversable, ({type l[+X]=Tuple2[Any, X]})#l")
    else
      str.replaceAll(Pattern.quote("<?>"), "_")*/
  }
  val classId = new Util.GlobalIDGenerator

  //Cache compilation results. Note: the cache does not identify alpha-equivalent expressions, but it does identify
  // expressions with the same constants!
  // Moreover, this cache should use soft references for keys. Googling SoftHashMap gives this answer:
  // http://stackoverflow.com/questions/264582/is-there-a-softhashmap-in-java
  private val codeCache = new ScalaThreadLocal(mutable.Map[String, Option[Class[_]]]())
  def cachedInvokeCompiler[T: TypeTag](prefix: String, restSourceStr: String, className: String) =
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

  /*private[expressiontree]*/ def emitSourceInternal[T: TypeTag](e: Exp[T]): (String, String, Seq[(ClassTag[_], Any)], String) = {
    precompileReset()
    val name = "Outclass" + classId()
    val typ = typeTag[T]
    val body = e.toCode
    val declValues = (map.get().toSeq map {
      case (value, CSPVar(memberName, memberCtag, memberType)) =>
        ("val %s: %s" format (memberName, manifestToString(memberType)), (memberCtag, value))
    })
    val (decls, staticData) = declValues.unzip[String, (ClassTag[_], Any)]
    val declsStr = decls mkString ", "
    val prefix = "class %s" format name
    val rest =
      """(%s) extends Compiled[%s] {
        |  override def result = %s
        |}""".stripMargin format (declsStr, manifestToString(typ), body)
    (prefix, rest, staticData, name)
  }

  //Mostly for testing
  def emitSource[T: TypeTag](e: Exp[T]): String = {
    val res = emitSourceInternal(e)
    res._1 + res._2
  }


  def toValue[T: TypeTag](exp: Exp[T]): T = {
    val (prefix, restSourceStr, staticData, className) = emitSourceInternal(exp)

    cachedInvokeCompiler(prefix, restSourceStr, className) match {
      case Some(cls) =>
        val cons = cls.getConstructor(staticData.map(_._1.runtimeClass):_*)
        val obj: T = cons.newInstance(staticData.map(_._2.asInstanceOf[AnyRef]):_*).asInstanceOf[Compiled[T]].result
        obj
      case None =>
        throw new CompilationFailedException("")
    }
  }
}

case class CompilationFailedException(message: String) extends Exception(message)
