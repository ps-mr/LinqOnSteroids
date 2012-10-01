package ivm
package expressiontree

import java.lang.reflect.Constructor
import ivm.tests.Debug

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
    if (Debug.active)
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
      Console.err println "compilation: had errors"
      if (!Debug.active)
        Console.err println sourceStr
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
    /*
    import java.util.regex.Pattern
    if (str endsWith ".type") //Workaround bug: names for singleton types are not fully qualified!
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

  private val expCodeCache: collection.concurrent.Map[Exp[_], Option[Constructor[_]]] = Util.buildCache
  //Note: this map should be a concurrent map, not ThreadLocals!
  //new ScalaThreadLocal(mutable.Map[Exp[_], Option[Constructor[_]]]())

  //*Reset methods are just (or mostly?) for testing {{{
  def precompileReset() {
    cspMap.get().clear()
    varId.localReset()
  }

  def reset() {
    precompileReset()
    classId.reset()
  }

  def completeReset() {
    expCodeCache.clear()
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

  private def getStaticData(cspValues: Seq[(Any, CSPVar)]): Seq[(ClassTag[_], Any)] =
    cspValues map {
      case (value, CSPVar(memberName, memberCtag, memberType)) =>
        (memberCtag, value)
    }

  private def getDecls(cspValues: Seq[(Any, CSPVar)]) =
    cspValues map {
      case (value, CSPVar(memberName, memberCtag, memberType)) =>
        "val %s: %s" format (memberName, manifestToString(memberType))
    }

  private def compileConstlessExp[T: TypeTag](transfExp: Exp[T], cspValues: Seq[(Any, CSPVar)]): (String, String, String) = {
    val body = transfExp.toCode
    val className = "Outclass" + classId()
    val typ = typeTag[T]
    val declsStr = getDecls(cspValues) mkString ", "
    val prefix = "class %s" format className
    val restSourceCode =
      """(%s) extends Compiled[%s] {
        |  override def result = %s
        |}""".stripMargin format(declsStr, manifestToString(typ), body)
    (prefix, restSourceCode, className)
  }

  private def extractConsts[T: TypeTag](e: Exp[T]): (Exp[T], Seq[(Any, CSPVar)]) = {
    val transfExp = removeConsts(e)
    transfExp.persistValues()
    val cspValues = cspMap.get().toList.toSeq //toList forces immutability.
    (transfExp, cspValues)
  }

  //For now experimental. This should respect the binding structure! You can't define everything at the beginning.
  def compile2[T](e: Exp[T]): String = {
    val symDecls = e __find {
      case _: Sym[_] => true
    } map {
      case SymWithId(defNode, id) => "val %s = %s" format (id, defNode.toCode) //s"val s${id} = ${defNode.toCode}"
      case _ => throw new Throwable()
    } mkString ("\n")
    val constlessE = e transform {
      case c: Const[_] =>
        CrossStagePersistence.persist(c.x)(c.cTag, c.tTag)
      case SymWithId(_, id) => e
      //This has the wrong type and crashes the compiler (?)
        //NamedVar("s" + id)
    }

    val body = constlessE.toCode
    "{\n  %s\n  %s\n}" format (symDecls, body)
    s"""{
    |  ${symDecls}
    |  ${body}
    |}""".stripMargin
    /*""*/
  }

  def toValue[T: TypeTag](e: Exp[T]): T = {
    val (transfExp, cspValues) = extractConsts(e)
    val staticData = getStaticData(cspValues)

    val maybeCons = expCodeCache.getOrElseUpdate(transfExp, {
      val (prefix, restSourceCode, className) = compileConstlessExp(transfExp, cspValues)
      ScalaCompile.invokeCompiler(prefix + restSourceCode, className) map (cls => cls.getConstructor(staticData.map(_._1.runtimeClass):_*))
    })
    buildInstance(maybeCons, staticData)
  }

  //Only for testing
  private[expressiontree] def toCode[T: TypeTag](e: Exp[T]): String = {
    val (transfExp, _) = extractConsts(e)
    transfExp.toCode
  }

  private[expressiontree] def emitSource[T: TypeTag](e: Exp[T]): String = {
    val (transfExp, cspValues) = extractConsts(e)
    val (prefix, restSourceCode, _) = compileConstlessExp(transfExp, cspValues)
    prefix + restSourceCode
  }

  def buildInstance[T](maybeCons: Option[Constructor[_]], staticData: Seq[(ClassTag[_], Any)]): T = maybeCons match {
    case Some(cons) =>
      val obj: T = cons.newInstance(staticData.map(_._2.asInstanceOf[AnyRef]):_*).asInstanceOf[Compiled[T]].result
      obj
    case None =>
      throw new CompilationFailedException("")
  }
}

case class CompilationFailedException(message: String) extends Exception(message)
