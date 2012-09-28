package ivm
package expressiontree

import Lifting._
import scala.reflect.macros.Context
import language.experimental.macros
import util.matching.Regex

trait Interpreted[Sym <: LangIntf, Res] {
  type ThisLangIntf = Sym
  def apply(s: ThisLangIntf): s.Rep[Res]
}

class MacroUtils[T <: Context](ctx: T) {
  import ctx.universe._
  object TermNameDecoded {
    def unapply(t: TermName): Some[String] = Some(t.decoded)
  }
  object TermNameEncoded {
    def unapply(t: TermName): Some[String] = Some(t.encoded)
  }
  object TypeNameDecoded {
    def unapply(t: TypeName): Some[String] = Some(t.decoded)
  }
  object TypeNameEncoded {
    def unapply(t: TypeName): Some[String] = Some(t.encoded)
  }
  //This duplicates the Tree.hasSymbolWhich method of the standard library.
  def hasSymbolWhich(sym: Symbol)(pred: Symbol => Boolean) = {
    sym != null && sym != NoSymbol && pred(sym)
  }
  def hasFullName(sym: Symbol, name: String): Boolean = {
    hasSymbolWhich(sym)(_.fullName == name)
  }
  def hasFullName(tree: Tree, name: String): Boolean =
    hasFullName(tree.symbol, name)
}

trait ReusableMacrosParams {
  /*
   * Macros for now can't be moved to this trait (as I'd like) because of https://issues.scala-lang.org/browse/SI-5753.
   */
  def ConvToTuple: Regex

  protected def prefix: String
  protected val macroDebug = false
  protected def implementationPkgName: String
  protected def implementationClsName: String
  protected def implementationFQClsName = implementationPkgName + "." + implementationClsName
}

object ReusableMacrosParams {
  def wrap_squopt_gen_impl[T: c.AbsTypeTag, Sym <: LangIntf: c.AbsTypeTag]
    (c: Context)
    (expr: c.Expr[Exp[T]], implementationFQClsName: String, prefix: String, ConvToTuple: Regex, macroDebug: Boolean):
  c.Expr[Any] = {
    val res = wrap_gen_impl_transf[T, Sym](c)(
        c.Expr[Exp[Any]](squopt_impl_transf(c)(
          expr,
          implementationFQClsName, prefix, ConvToTuple, macroDebug)), implementationFQClsName)
    println(res)
    c.Expr(c.resetAllAttrs(res))
  }

  //Reusable part.
  def wrap_gen_impl_transf[T: c.AbsTypeTag, Sym <: LangIntf: c.AbsTypeTag]
    (c: Context)
    (expr: c.Expr[Exp[Any]], implementationFQClsName: String):
  c.universe.Tree = {
    import c.universe._
    val extractors = new MacroUtils[c.type](c)
    import extractors._
    //Since this transformer inspects symbols, it must be called _before_ c.resetAllAttrs!
    object resetIntfMemberBindings extends Transformer {
      override def transform(tree: Tree): Tree = {
        tree match {
          case Select(lifting, b) if hasFullName(lifting, implementationFQClsName) =>
            /* This tree is untyped, hence we must trigger typechecking again
             * by calling resetAllAttrs. When a tree is typed, the typechecker
             * does not visit its descendants. */
            Ident(b)
          case _ => super.transform(tree)
        }
      }
    }
    val clearedExpr = c.Expr[Any](resetIntfMemberBindings transform expr.tree)
    val newTree =
      reify(new Interpreted[Sym, T] {
        def apply(s: ThisLangIntf): s.Rep[T] = {
          import s._
          clearedExpr.splice.asInstanceOf[s.Rep[T]]
        }
      }).tree
    newTree
  }
  def wrap_gen_impl[T: c.AbsTypeTag, Sym <: LangIntf: c.AbsTypeTag](c: Context)
                                                                   (expr: c.Expr[Exp[T]],
                                                                    implementationFQClsName: String):
  c.Expr[Interpreted[Sym, T]] = {
    val newTree = wrap_gen_impl_transf[T, Sym](c)(expr, implementationFQClsName)
    c.Expr[Interpreted[Sym, T]](c.resetAllAttrs(newTree))
  }

  val anyUnaryMethods = List("toString", "hashCode", "##")
  val anyRefUnaryMethods = List("notify", "notifyAll", "wait")

  val anyBinaryMethods = List("!=", "==", "equals")
  val anyRefBinaryMethods = List("eq", "ne")

  val anyTypeUnaryMethod = List("asInstanceOf", "isInstanceOf")
  val anyTypeBinaryMethod = List("synchronized")

  val AnyTuple = "Tuple([0-9]+)".r

  def squopt_impl[T: c.AbsTypeTag]
    (c: Context)
    (expr: c.Expr[T], implementationFQClsName: String, prefix: String, ConvToTuple: Regex, macroDebug: Boolean):
  c.Expr[Any] = {
    c.Expr(c.resetAllAttrs(squopt_impl_transf[T](c)(expr, implementationFQClsName, prefix, ConvToTuple, macroDebug)))
  }

  def squopt_impl_transf[T: c.AbsTypeTag]
    (c: Context)
    (expr: c.Expr[T], implementationFQClsName: String, prefix: String, ConvToTuple: Regex, macroDebug: Boolean):
  c.universe.Tree = {
    import c.universe._

    val extractors = new MacroUtils[c.type](c)
    import extractors._

    def println(x: => Any) = if (macroDebug) Predef println x
    def newline() = if (macroDebug) Predef println ()
    object squoptTransformer extends Transformer {
      var level = 0
      override def transform(tree: Tree): Tree = {
        //println("Level %d, tree %s" format(level, showRaw(tree)))
        level += 1
        val ret = tree match {
          //Drop type applications which are introduced by type inference.
          case TypeApply(polyterm, args)
            //TypeTree.original returns the type before type inference, if any.
            //Hence, t.original == null means that t is inferred.
            if (for (arg @ TypeTree() <- args) yield arg) exists (_.original == null)
          =>
            //We assume that if one arg is inferred, all are inferred, so we can just drop all.
            assert((for (arg @ TypeTree() <- args) yield arg) forall (_.original == null))
            transform(polyterm)

          //this duplicates the check but also checks arity. Do it even more
          //generic. But later.
          case Apply(Select(op1, member), l @ List())
            if (anyUnaryMethods ++ anyRefUnaryMethods ++ anyTypeUnaryMethod) contains member.decoded
          =>
            Apply(Ident(newTermName(prefix + member.encoded)), transform(op1) :: l)
          case Apply(Select(op1, member), l @ List(op2)) if anyBinaryMethods contains member.decoded =>
            Apply(Ident(newTermName(prefix + member.encoded)), (op1 :: l) map (transform(_)))
          case TypeApply(Select(op1, member), typeArgs @ List(typeArg))
            if anyTypeUnaryMethod contains member.decoded
          =>
            if (typeArg.asInstanceOf[TypeTree].original == null)
              printf("Argument of %s deduced by type inference!\n", member.decoded)
            Apply(TypeApply(
              Ident(newTermName(prefix + member.encoded)), typeArgs), List(transform(op1)))
          case Apply(
            TypeApply(Select(op1, member), typeArgs @ List(typeArg)),
            l2 @ List(arg))
            if anyTypeBinaryMethod contains member.decoded
          =>
            Apply(TypeApply(
              Ident(newTermName(prefix + member.encoded)), typeArgs), (op1 :: l2) map (transform(_)))

          case Apply(TypeApply(Select(Select(scala, TermNameEncoded(AnyTuple(arity))), TermNameEncoded("apply")), tArgs),
            args @ List(_*))
            if hasFullName(scala, "scala")
          =>
            Apply(Ident(newTermName("LiftTuple" + arity)), args map (transform(_)))

          //Start removing implicit conversions: they will be readded by the second layer of typechecking if needed.
          case Apply(Apply(TypeApply(Select(lifting, TermNameEncoded("pure")), tArgs), List(convertedTerm)), implicitArgs)
            if hasFullName(lifting, implementationFQClsName)
          =>
            //println(tArgs map (showRaw(_, printTypes = true)))
            assert(tArgs.length == 1)
            println(tArgs(0).tpe)
            val TypeRef(pre, sym, args) = tArgs(0).tpe
            println(sym.fullName)
            println(tArgs(0).tpe.toString)
            //println(tArgs map (showRaw(_, printTypes = true)))
            //transform(convertedTerm)
            //Typed(transform(convertedTerm), Ident(newTypeName("ivm.expressiontree.Exp[" + sym.fullName + "]")))
            //Still Scalac doesn't like it.
            //Typed(transform(convertedTerm), AppliedTypeTree(Ident(newTypeName("Exp")), List(Ident(newTypeName(sym.fullName)))))
            transform(convertedTerm)
          case Apply(TypeApply(Select(lifting, TermNameEncoded(ConvToTuple())), tArgs), List(convertedTerm))
            if hasFullName(lifting, implementationFQClsName)
          =>
            transform(convertedTerm)
          case _ => super.transform(tree)
        }
        level -= 1
        ret
      }
    }
    newline()
    println("#### Before transform: " + expr.tree)
    val transformed = squoptTransformer.transform(expr.tree)
    println("#### Transformed: " + transformed)
    transformed
  }
}

object Macros extends ReusableMacrosParams {
  //TODO: move this misc macros to a separate class.
  def stringify[T](arg: T): String = macro stringify_impl[T]
  def show[T](arg: T) = macro show_impl[T]
  def ctShow[T](arg: T) = macro ctShow_impl[T]
  def ctShowDebug[T](arg: T) = macro ctShowDebug_impl[T]


  def macroId[T](arg: T): T = macro macroId_impl[T]
  def macroId_impl[T: c.AbsTypeTag](c: Context)(arg: c.Expr[T]): c.Expr[T] = arg


  def stringify_base(c: Context)(arg: c.Expr[Any]): String =
    arg.tree.toString //The result here is a bit ugly - we need to print the tree before desugaring.
  def stringify_impl[T: c.AbsTypeTag](c: Context)(arg: c.Expr[T]): c.Expr[String] = {
    import c.universe._
    //c.Expr[String](Literal(Constant(stringify_base(c)(arg))))
    //reify(c.Expr[String](Literal(Constant(stringify_base(c)(arg)))).splice)
    val v = stringify_base(c)(arg)
    //reify(v) // This reifies the variable reference.
    //println(showRaw(reify(v).tree))
    c.Expr[String](Literal(Constant(v)))
  }

  def show_impl[T: c.AbsTypeTag](c: Context)(arg: c.Expr[T]): c.Expr[Unit] = {
    import c.universe._
    val v = stringify_base(c)(arg)
    //val v1 = reify(v)
    val v1 = c.Expr[String](Literal(Constant(v)))
    reify(println("Expr: %s evaluates to %s" format (v1.splice, arg.splice)))
  }
  def ctShow_impl[T: c.AbsTypeTag](c: Context)(arg: c.Expr[T]): c.Expr[Unit] = {
    import c.universe._
    println(stringify_base(c)(arg))
    reify(())
  }

  def ctShowDebug_impl[T: c.AbsTypeTag](c: Context)(arg: c.Expr[T]): c.Expr[T] = {
    import c.universe._
    println("\n## Stringify: " + stringify_base(c)(arg) + "\n")
    println("## showRaw: " + showRaw(arg.tree) + "\n")//, printTypes = true, printIds = true
    arg
  }

  def wrap_squopt[T](expr: Exp[T]) = macro wrap_squopt_impl[T]
  def wrap_squopt_impl[T: c.AbsTypeTag](c: Context)(expr: c.Expr[Exp[T]]) =
    wrap_squopt_gen_impl[T, LiftingLangIntf](c)(expr)
  def wrap[T](expr: Exp[T]) = macro wrap_impl[T]
  def wrap_impl[T: c.AbsTypeTag](c: Context)(expr: c.Expr[Exp[T]]) =
    wrap_gen_impl[T, LiftingLangIntf](c)(expr)
  //TODO: merge this macro within squopt.

  def squopt[T](expr: T): Any = macro squopt_impl[T]
  override protected def prefix = "squopt_"

  override protected val macroDebug = true
  override protected def implementationPkgName = "ivm.expressiontree"
  override protected def implementationClsName = "Lifting"

  val ConvToTuple = "tuple[0-9]+ToTuple[0-9]+Exp".r
  def wrap_squopt_gen_impl[T: c.AbsTypeTag, Sym <: LangIntf: c.AbsTypeTag](c: Context)(expr: c.Expr[Exp[T]]): c.Expr[Any] = {
    ReusableMacrosParams.wrap_squopt_gen_impl[T, Sym](c)(expr, implementationFQClsName, prefix, ConvToTuple, macroDebug)
  }
  def wrap_gen_impl[T: c.AbsTypeTag, Sym <: LangIntf: c.AbsTypeTag](c: Context)(expr: c.Expr[Exp[T]]): c.Expr[Interpreted[Sym, T]] = {
    ReusableMacrosParams.wrap_gen_impl[T, Sym](c)(expr, implementationFQClsName)
  }
  def squopt_impl[T: c.AbsTypeTag](c: Context)(expr: c.Expr[T]): c.Expr[Any] = {
    ReusableMacrosParams.squopt_impl[T](c)(expr, implementationFQClsName, prefix, ConvToTuple, macroDebug)
  }
}

// vim: set sw=2 et:
