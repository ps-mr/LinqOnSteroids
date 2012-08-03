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
    if (Benchmarking.debugBench)
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
  import CrossStagePersistence.{cspMap, varId}

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

  //Note: these two maps should be concurrent maps, not ThreadLocals!
  private val codeCache = new ScalaThreadLocal(mutable.Map[String, Option[Class[_]]]())
  private val expCodeCache = new ScalaThreadLocal(mutable.Map[Exp[_], Option[Class[_]]]())

  //The caching here is to remove. Also, we must remove Const.toCode :-).
  def cachedInvokeCompiler[T: TypeTag](prefix: String, restSourceCode: String, className: String) =
    codeCache.get().getOrElseUpdate(restSourceCode, ScalaCompile.invokeCompiler(prefix + restSourceCode, className))

  //*Reset methods are just (or mostly?) for testing {{{
  def precompileReset() {
    cspMap.get().clear()
    varId.localReset()
  }

  def reset() {
    precompileReset()
    codeCache.get().clear()
    classId.reset()
  }

  def completeReset() {
    codeCache.get().clear()
    expCodeCache.get.clear()
    reset()
  }
  //}}}
  def removeConsts[T](e: Exp[T]) = {
    precompileReset()
    e transform {
      case c: Const[_] =>
        CrossStagePersistence.persist(c.x)(c.cTag, c.tTag)
      case exp => exp
    }
  }

  def toValue[T: TypeTag](e: Exp[T]): T = {
    val transfExp = removeConsts(e)
    transfExp.persistValues()
    val cspValues = cspMap.get().toList.toSeq //toList forces immutability.

    val staticData: Seq[(ClassTag[_], Any)] = cspValues map {
      case (value, CSPVar(memberName, memberCtag, memberType)) =>
        (memberCtag, value)
    }
    val maybeCls = expCodeCache.get().getOrElseUpdate(transfExp, {
      val className = "Outclass" + classId()
      val typ = typeTag[T]
      val body = transfExp.toCode
      val decls = (cspValues map {
        case (value, CSPVar(memberName, memberCtag, memberType)) =>
          "val %s: %s" format (memberName, manifestToString(memberType))
      })
      val declsStr = decls mkString ", "
      val prefix = "class %s" format className
      val restSourceCode =
        """(%s) extends Compiled[%s] {
          |  override def result = %s
          |}""".stripMargin format (declsStr, manifestToString(typ), body)
      cachedInvokeCompiler(prefix, restSourceCode, className)
      //Or simply
      //ScalaCompile.invokeCompiler(prefix + restSourceCode, className)
    })
    buildInstance(maybeCls, staticData)
  }

  /*private[expressiontree]*/ def emitSourceInternal[T: TypeTag](e: Exp[T]): (String, String, Seq[(ClassTag[_], Any)], String) = {
    precompileReset()
    val className = "Outclass" + classId()
    val typ = typeTag[T]
    val body = e.toCode
    val declValues = (cspMap.get().toSeq map {
      case (value, CSPVar(memberName, memberCtag, memberType)) =>
        ("val %s: %s" format (memberName, manifestToString(memberType)), (memberCtag, value))
    })
    val (decls, staticData) = declValues.unzip[String, (ClassTag[_], Any)]
    val declsStr = decls mkString ", "
    val prefix = "class %s" format className
    val restSourceCode =
      """(%s) extends Compiled[%s] {
        |  override def result = %s
        |}""".stripMargin format (declsStr, manifestToString(typ), body)
    (prefix, restSourceCode, staticData, className)
  }

  //Mostly for testing
  def emitSource[T: TypeTag](e: Exp[T]): String = {
    val res = emitSourceInternal(e)
    res._1 + res._2
  }

  def buildInstance[T](c: Option[Class[_]], staticData: Seq[(ClassTag[_], Any)]): T = c match {
    case Some(cls) =>
      val cons = cls.getConstructor(staticData.map(_._1.runtimeClass):_*)
      val obj: T = cons.newInstance(staticData.map(_._2.asInstanceOf[AnyRef]):_*).asInstanceOf[Compiled[T]].result
      obj
    case None =>
      throw new CompilationFailedException("")
  }

  def toValue2[T: TypeTag](exp: Exp[T]): T = {
    val (prefix, restSourceCode, staticData, className) = emitSourceInternal(exp)
    buildInstance(cachedInvokeCompiler(prefix, restSourceCode, className), staticData)
  }
}

case class CompilationFailedException(message: String) extends Exception(message)
