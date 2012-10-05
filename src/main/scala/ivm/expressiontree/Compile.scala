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
    compiler.settings.optimise.tryToSet(Nil)
    //      compiler.genJVM.outputDir = fileSystem

    run.compileSources(List(new util.BatchSourceFile("<stdin>", sourceStr)))
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

  private val expCodeCache: collection.concurrent.Map[(Exp[_], Seq[Class[_]]), Option[Constructor[_]]] = Util.buildCache
  //Note: this map should be a concurrent map, not ThreadLocals!
  //new ScalaThreadLocal(mutable.Map[Exp[_], Option[Constructor[_]]]())

  //*Reset methods are just (or mostly?) for testing {{{
  //XXX: Actually, we should do most map resets at exit, not at entry to avoid holding until next compilation onto
  // things we won't need, that is, and memory leaks.
  def precompileReset() {
    cspMap.get().clear()
    varId.localReset()
    Sym.gensymId.localReset()
  }

  def reset() {
    precompileReset()
    classId.reset()
  }

  //Clears the cache of compiled programs!
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

    val transfExp = e transform {
      case c: Const[_] =>
        persistConst(c)
      case exp => exp
    }
    assert((transfExp __find {
      case c: Const[_] =>
        true
    }).isEmpty)
    transfExp
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
      """(%s) extends ivm.expressiontree.Compiled[%s] {
        |  override def result = %s
        |}""".stripMargin format(declsStr, manifestToString(typ), body)
    (prefix, restSourceCode, className)
  }

  private def extractCSPData[T: TypeTag](e: Exp[T]): (Seq[(Any, CSPVar)], Seq[Class[_]], Seq[AnyRef]) = {
    e.persistValues()
    val cspValues = cspMap.get().toList.toSeq //toList forces immutability.
    val staticData = getStaticData(cspValues)
    (cspValues, staticData.map(_._1.runtimeClass), staticData.map(_._2.asInstanceOf[AnyRef]))
  }

  private case class Scope(boundVar: Option[Var] = None, bindings: mutable.Map[Int, Def[_]] = mutable.Map.empty)

  private case class Attach[T](wrappedExp: Exp[T], attachment: Scope) extends Arity1OpExp[T, T, Attach[T]](wrappedExp) {
    override def copy(defNode: Exp[T]) = Attach(defNode, attachment)
    override def interpret() = wrappedExp.interpret()
    private def bindings = attachment.bindings.toList

    override def persistValues() {
      bindings map (_._2) map (_ persistValues ())
      super.persistValues()
    }

    override def toCode = {
      val symDecls = bindings sortBy (_._1) map {
        case (id, boundNode) => "    val %s%d = %s\n" format (symValPrefix, id, boundNode.toCode)
      } mkString ""
      wrappedExp match {
        case _ =>
          s"""{
          |${symDecls}    ${wrappedExp.toCode}
          |  }""".stripMargin
      }
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
    //@annotation.tailrec
    def toSymRef[U](scopeList: List[Scope], s: Sym[U]): Exp[U] = {
      //Hoist symbols out of functions if possible. Hm. That makes sense only for loops, not for all function bodies. And
      //it's an optim we don't do yet :-(.
      val scope = scopeList.head
      scope.boundVar match {
        //case Some(boundVar) if !(s.defNode isOrContainsGen boundVar) => //XXX: I'd have to check for all the symbols bound in scope!
          //toSymRef(scopeList.tail, s)
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
        case s @ Sym(v: Var) => s
        case s @ Sym(NamedVar(_)) => s
        case s @ SymWithId(defNode, id) =>
          def computeNewDefNode = defNode.genericConstructor(s.children mapConserve topDownTraverse[Any])
          defNode match {
            //Do we really want CSE on functions? I don't think so. For one, we'd need to annotate functions with their
            // parameter type, and we'd need manifests for that.
            case f @ FuncExpBody(body) =>
              //We call withNewScope on the body! This way, a FunSym stays a FunSym (which is important).
              //Moreover, it becomes simpler to do code generation.
              toAtomCSE(f genericConstructor List(withNewScope(Scope(Some(f.x)), topDownTraverse(body))))
              //withNewScope(Scope(Some(f.x)), toAtomCSE(computeNewDefNode))
            case IfThenElse(cond, thenBody, elseBody) =>
              toSymRef(scopeList, IfThenElse(topDownTraverse(cond),
                withNewScope(Scope(None), topDownTraverse(thenBody)),
                withNewScope(Scope(None), topDownTraverse(elseBody))))
            case And(a, b) =>
              (toSymRef[Boolean](scopeList, And(
                withNewScope(Scope(None), topDownTraverse(a)),
                withNewScope(Scope(None), topDownTraverse(b))))
                //Stupid Scala GADT inference
                : Exp[Boolean]).asInstanceOf[Exp[V]]
            case _ =>
              toSymRef(scopeList, computeNewDefNode)
          }
      }
      withNewScope(Scope(), topDownTraverse(e))
    }

    //rewrite the tree while sharing common subexpression, producing a DAG.
    def doCSE[U](constlessExp: Exp[U]): Exp[U] = {
      constlessExp transform {
        case Sym(defNode) =>
          //XXX We need to build here a symbol which does consider the id in the equality! But write a test for that first.
          //Otherwise, CSE won't distinguish between Symbols in different scopes, since the IDs are not part of equality comparison. Darn!
          toAtomCSE(defNode)
        case c: Const[t] => throw new RuntimeException("Const expression in constlessExp")
      }
    }

    precompiledExpToValue(collectSymbols(doCSE(removeConsts(e))))
  }

  def toValue[T: TypeTag](e: Exp[T]): T =
    precompiledExpToValue(removeConsts(e))
//    toValueCSE(e)

  private def precompiledExpToValue[T: TypeTag](precompiledExp: Exp[T]): T = {
    val (cspValues, cspClasses, cspData) = extractCSPData(precompiledExp)

    val maybeCons = expCodeCache.getOrElseUpdate((precompiledExp, cspClasses), {
      val (prefix, restSourceCode, className) = compileConstlessExp(precompiledExp, cspValues)
      ScalaCompile.invokeCompiler(prefix + restSourceCode, className) map (cls => cls
        .getConstructor(cspClasses: _*))
    })
    buildInstance(maybeCons, cspData)
  }

  //Only for testing
  private[expressiontree] def toCode[T: TypeTag](e: Exp[T]): String = {
    val precompiledExp = removeConsts(e)
    precompiledExp.persistValues() //We ignore the return values, but we need the side effects of this call.
    precompiledExp.toCode
  }

  private[expressiontree] def emitSource[T: TypeTag](e: Exp[T]): String = {
    val precompiledExp = removeConsts(e)
    val (cspValues, _, _) = extractCSPData(precompiledExp)

    val (prefix, restSourceCode, _) = compileConstlessExp(precompiledExp, cspValues)
    prefix + restSourceCode
  }

  def buildInstance[T](maybeCons: Option[Constructor[_]], cspData: Seq[AnyRef]): T = maybeCons match {
    case Some(cons) =>
      val obj: T = cons.newInstance(cspData: _*).asInstanceOf[Compiled[T]].result
      obj
    case None =>
      throw new CompilationFailedException("")
  }
}

case class CompilationFailedException(message: String) extends Exception(message)
