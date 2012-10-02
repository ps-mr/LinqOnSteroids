package ivm
package expressiontree

import java.lang.reflect.Constructor
import ivm.tests.Debug
import optimization.OptimizationUtil.FuncExpBody
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
    assert((transfExp __find {
      case c: Const[_] =>
        true
    }).isEmpty)
    transfExp.persistValues()
    val cspValues = cspMap.get().toList.toSeq //toList forces immutability.
    (transfExp, cspValues)
  }

  private case class Scope(boundVar: Option[Var] = None, bindings: mutable.Map[Int, Def[_]] = mutable.Map.empty)

  private case class Attach[T](wrappedExp: Exp[T], attachment: Scope) extends Arity1OpExp[T, T, Attach[T]](wrappedExp) {
    override def copy(defNode: Exp[T]) = Attach(defNode, attachment)
    override def interpret() = wrappedExp.interpret()
    override def toCode = {
      val symDecls = attachment.bindings map {
        case (id, boundNode) => "val %s = %s" format (id, boundNode.toCode)
      } mkString "\n"
      s"""{
      |  ${symDecls}
      |  ${wrappedExp.toCode}
      |}""".stripMargin
    }
  }

  //This is the version for CSE, but it cannot be used during transformations - it's hundred of times slower.
  //XXX This map should be thread-local (iff Sym.gensymId is thread-local, as it is currently).
  private def definitions: mutable.Map[Def[_], Sym[_]] = new mutable.HashMap()
  private def toAtomCSE[T](d: Def[T]): Exp[T] =
    d match {
      case df: Fun[s, t] =>
        //We don't do CSE on functions.
        BaseLangImpl.toFunSym[s, t](df).asInstanceOf[Exp[T]]
      case _ =>
        definitions.asInstanceOf[mutable.Map[Def[T], Sym[T]]].getOrElseUpdate(d, Sym(d))
    }
  private val symValPrefix = "s"
  //For now experimental. This should respect the binding structure! You can't define everything at the beginning.
  //Since my only binder is lambda (right?) it should be easy to identify those nodes and treat them specially. Special
  //synthetic nodes (like NasmedVar) might be needed for the translation.
  private def toValueCSE[T: TypeTag](e: Exp[T]): T = {
    /*
    //We need a top-down traversal...
    def topDownTraverse1[U](e: Exp[U])(matcher: ExpTransformer): Exp[U] = {
      e match {
        case c @ Const(_ ) => matcher(c)
        case s @ Sym(defin) => defin.genericConstructor(matcher(e).children map (topDownTraverse1(_)(matcher)))
      }
    }
    */

    //Precondition: the bottom of scopeList is a scope without a bound variable.
    @annotation.tailrec
    def toSymRef[U](scopeList: List[Scope], s: Sym[U]): Exp[U] = {
      //Hoist symbols out of functions if possible. Hm. That makes sense only for loops, not for all function bodies. And
      //it's an optim we don't do yet (
      val scope = scopeList.head
      scope.boundVar match {
        case Some(boundVar) if !(s.defNode isOrContains boundVar) =>
          toSymRef(scopeList.tail, s)
        case _ =>
          scope.bindings put (s.id, s.defNode)
          NamedVar(symValPrefix + s.id)
      }
    }

    def collectSymbols[U](e: Exp[U]): Exp[U] = {
      var scopeList = List[Scope](Scope())
      def withNewScope[U](scope: Scope, f: => Exp[U]): Exp[U] = {
        val oldScopeList = scopeList
        scopeList = scope :: scopeList
        val res = f
        val filledScope = scopeList.head
        scopeList = oldScopeList
        Attach(res, filledScope)
      }

      //We need a top-down traversal...
      //...but with a visitor controlling the traversal (a state monad would also work to make this non-imperative).
      def topDownTraverse[V]: Exp[V] => Exp[V] = {
        case c @ Const(_) => c
        case s @ SymWithId(defNode, id) =>
          def computeNewDefNode = defNode.genericConstructor(e.children mapConserve topDownTraverse[Any])
          defNode match {
            //Do we really want CSE on functions? I don't think so. For one, we'd need manifests for that.
            case f @ FuncExpBody(body) =>
              withNewScope(Scope(Some(f.x)), computeNewDefNode)
            case ifExpr @ IfThenElse(cond, thenBody, elseBody) =>
              toSymRef(scopeList, Sym(IfThenElse(topDownTraverse(cond),
                withNewScope(Scope(None), topDownTraverse(thenBody)),
                withNewScope(Scope(None), topDownTraverse(elseBody)))))
            case _ =>
              toSymRef(scopeList, Sym(computeNewDefNode))
          }
      }
      withNewScope(Scope(), topDownTraverse(e))
    }

    val constNodes = new mutable.HashMap[Const[_], Const[_]]
    def rebuildConst[U](c: Const[U]): Const[U] = constNodes.asInstanceOf[mutable.Map[Const[U], Const[U]]].getOrElseUpdate(c, c)

    //First step: rewrite the tree while sharing common subexpression, producing a DAG.
    val cseExp = e transform {
      case Sym(defNode) => toAtomCSE(defNode)
      case c: Const[t] => rebuildConst(c) //Allow CSE to help!
    }

    //XXX: This will visit each shared subexpression multiple times: not good! But unavoidable if we don't track visited
    // nodes (as in graph algorithms). Will also persist constants multiple times (easy to fix).
    val (constlessExp, cspValues) = extractConsts(cseExp)
    val symlessExp = collectSymbols(constlessExp)

    val staticData = getStaticData(cspValues)

    val maybeCons = expCodeCache.getOrElseUpdate(symlessExp, {
      val (prefix, restSourceCode, className) = compileConstlessExp(symlessExp, cspValues)
      ScalaCompile.invokeCompiler(prefix + restSourceCode, className) map (cls => cls.getConstructor(staticData.map(_._1.runtimeClass):_*))
    })
    buildInstance[T](maybeCons, staticData)
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
