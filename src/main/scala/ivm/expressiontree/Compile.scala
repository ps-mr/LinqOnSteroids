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
  //XXX: Actually, we should do most map resets at exit, not at entry to avoid memory leaks.
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
    val constNodes = new mutable.HashMap[Const[_], Exp[_]]
    def persistConst[U](c: Const[U]): Exp[U] =
      constNodes.asInstanceOf[mutable.Map[Const[U], Exp[U]]].getOrElseUpdate(c,
        CrossStagePersistence.persist(c.x)(c.cTag, c.tTag))

    e transform {
      case c: Const[_] =>
        persistConst(c)
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
      val symDecls = attachment.bindings.toList sortBy (_._1) map {
        case (id, boundNode) => "    val %s%d = %s" format (symValPrefix, id, boundNode.toCode)
      } mkString "\n"
      s"""{
      |${symDecls}
      |    ${wrappedExp.toCode}
      |  }""".stripMargin
    }
  }

  private val symValPrefix = "s"
  //For now experimental. This should respect the binding structure! You can't define everything at the beginning.
  //Since my only binder is lambda (right?) it should be easy to identify those nodes and treat them specially. Special
  //synthetic nodes (like NasmedVar) might be needed for the translation.
  def toValueCSE[T: TypeTag](e: Exp[T]): T = {
    //This is the version for CSE, but it cannot be used during transformations - it's hundred of times slower.
    val definitions: mutable.Map[Def[_], Sym[_]] = new mutable.HashMap()
    // We add another implicit conversion in scope to prevent application of the toAtomImplicit, which does not share
    // symbols. Note that this conversion is more specific and is sometimes chosen where the other does not apply, but
    // that's safe.
    implicit def toAtomCSE[U]: Def[U] => Sym[U] = {
      case funDef: Fun[s, t] =>
        //We don't do CSE on functions.
        //The type ascription + cast makes sure that the cast is not doing more than it should.
        (BaseLangImpl.toFunSym[s, t](funDef): Sym[s => t]).asInstanceOf[Sym[U]]
      case defNode =>
        definitions.asInstanceOf[mutable.Map[Def[U], Sym[U]]].getOrElseUpdate(defNode, Sym(defNode))
    }

    //Precondition: the bottom of scopeList is a scope without a bound variable.
    @annotation.tailrec
    def toSymRef[U](scopeList: List[Scope], s: Sym[U]): Exp[U] = {
      //Hoist symbols out of functions if possible. Hm. That makes sense only for loops, not for all function bodies. And
      //it's an optim we don't do yet :-(.
      val scope = scopeList.head
      scope.boundVar match {
        case Some(boundVar) if !(s.defNode isOrContainsGen boundVar) =>
          toSymRef(scopeList.tail, s)
        case _ =>
          scope.bindings put (s.id, s.defNode)
          toAtomCSE(NamedVar(symValPrefix + s.id))
      }
    }

    def collectSymbols[U](e: Exp[U]): Exp[U] = {
      var scopeList = List[Scope](Scope())
      def withNewScope[V](scope: Scope, f: => Exp[V]): Exp[V] = {
        val oldScopeList = scopeList
        scopeList = scope :: scopeList
        val res = f
        val filledScope = scopeList.head
        scopeList = oldScopeList
        toAtomCSE(Attach(res, filledScope))
      }

      //We need a top-down traversal...
      //...but with a visitor controlling the traversal (a state monad would also work to make this non-imperative).
      def topDownTraverse[V]: Exp[V] => Exp[V] = {
        case c @ Const(_) => c
        case s @ Sym(NamedVar(_)) => s
        case s @ SymWithId(defNode, id) =>
          def computeNewDefNode = defNode.genericConstructor(s.children mapConserve topDownTraverse[Any])
          defNode match {
            //Do we really want CSE on functions? I don't think so. For one, we'd need manifests for that.
            case f @ FuncExpBody(body) =>
              withNewScope(Scope(Some(f.x)), toAtomCSE(computeNewDefNode))
            case ifExpr @ IfThenElse(cond, thenBody, elseBody) =>
              toSymRef(scopeList, IfThenElse(topDownTraverse(cond),
                withNewScope(Scope(None), topDownTraverse(thenBody)),
                withNewScope(Scope(None), topDownTraverse(elseBody))))
            case _ =>
              toSymRef(scopeList, computeNewDefNode)
          }
      }
      withNewScope(Scope(), topDownTraverse(e))
    }

    val (constlessExp, cspValues) = extractConsts(e)
    val staticData = getStaticData(cspValues)

    //rewrite the tree while sharing common subexpression, producing a DAG.
    val cseExp = constlessExp transform {
      case Sym(defNode) =>
        //XXX We need to build here a symbol which does consider the id in the equality! But write a test for that first.
        //Otherwise, CSE won't distinguish between Symbols in different scopes, since the IDs are not part of equality comparison. Darn!
        toAtomCSE(defNode)
      case c: Const[t] => throw new RuntimeException("Const expression in constlessExp")
    }

    val symlessExp = collectSymbols(cseExp)
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
