package ivm
package expressiontree

import Lifting._
import scala.reflect.macros.Context
import language.experimental.macros

//trait ModularFrontendDefs {
  trait LangIntf {
    type Rep[+T]
  }

  trait BaseLangIntf extends LangIntf {
    //Add a typeclass constraint, instead of ugly tricks to disable the conversion for specific classes.
    //implicit def pure[T](t: T): Rep[T]
    implicit def pure[T: ClassTag: TypeTag](t: T): Rep[T]
  }

  trait ScalaLangIntf {
    this: LangIntf =>
    //Why not an implicit abstract class? Ah I see.
    implicit def expToNumOps[T: Numeric](t: Rep[T]): NumericOps[T]
    abstract class NumericOps[T: Numeric](t: Rep[T]) {
      def +(that: Rep[Int]): Rep[Int]
    }
  }

  trait Interpreted[Sym <: LangIntf, Res] {
    type ThisLangIntf = Sym
    def apply(s: ThisLangIntf): s.Rep[Res]
  }
//}

object UtilsForMacros {
  def extractors(c: Context) = new {
    import c.universe._
    object TermNameDecoded {
      def unapply(t: TermName): Some[String] = Some(t.decoded)
    }
    object TermNameEncoded {
      def unapply(t: TermName): Some[String] = Some(t.encoded)
    }
  }
}
object Macros /*extends ModularFrontendDefs*/ {
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

  import UtilsForMacros._

  //val anyUnaryMethods = List("toString", "hashCode", "getClass", "##")
  val anyUnaryMethods = List("toString", "hashCode", "##")
  val anyRefUnaryMethods = List("notify", "notifyAll", "wait")

  val anyBinaryMethods = List("!=", "==", "equals")
  val anyRefBinaryMethods = List("eq", "ne")

  val anyTypeUnaryMethod = List("asInstanceOf", "isInstanceOf")
  val anyTypeBinaryMethod = List("synchronized")
  //def anyUnary(v: String): Boolean

  /*
  def smart(expr: Any): Any = macro smart_impl
  def smart_impl(c: Context)(expr: c.Expr[Any]): c.Expr[Any] = {
    import c.universe._
    //Main problem: we need to visit the tree recursively.
    expr.tree match {
      case Apply(Select(op1, member), l @ List()) if anyUnaryMethods contains member.decoded =>
        c.Expr(Apply(Ident(newTermName(prefix + member.encoded)), op1 :: l))
        //c.Expr(Apply(Ident(newTermName("toString")), List(op1)))
        //c.Expr(Apply(Ident(member), List(op1)))
      case Apply(Select(op1, member), l @ List(op2)) if anyBinaryMethods contains member.decoded =>
        c.Expr(Apply(Ident(newTermName(prefix + member.encoded)), op1 :: l))
      //case Apply(Select(op1, member), List(op2)) if member.decoded == "==" =>
        //c.Expr(Apply(Ident(newTermName("eq")), List(op1, op2)))
      //case Apply(Select(op1, member), List(op2)) if member.decoded == "!=" =>
        //c.Expr(Apply(Ident(newTermName("neq")), List(op1, op2)))
      case _ => expr
    }
  }
   */
  /*
  def wrap[T](expr: T): Any = macro wrap_impl[T, BaseLangIntf with ScalaLangIntf]
  def wrap_impl[T: c.AbsTypeTag, Sym <: LangIntf: c.AbsTypeTag](c: Context)(expr: c.Expr[T]): c.Expr[Any] = {
   */
  def wrap[T](expr: Exp[T]): Any = macro wrap_impl[T, BaseLangIntf with ScalaLangIntf]
  def wrap_impl[T: c.AbsTypeTag, Sym <: LangIntf: c.AbsTypeTag](c: Context)(expr: c.Expr[Exp[T]]): c.Expr[Any] = {
    import c.universe._
    object transformer2 extends Transformer {
      override def transform(tree: Tree): Tree = {
        object TermNameEncoded {
          def unapply(t: TermName): Some[String] = Some(t.encoded)
        }
        tree match {
          //case Select(a, TermNameEncoded(b)) if a hasSymbolWhich (_.name == newTermName("ivm.expressiontree.Lifting")) =>
          //case Select(a, TermNameEncoded(b)) =>
          case Select(a @ Ident(TermNameEncoded("Lifting")), TermNameEncoded(b))
            if a.symbol != null && a.symbol != NoSymbol && a.symbol.fullName == "ivm.expressiontree.Lifting"
          =>
            //println(showRaw(a), b, showRaw(a.symbol))
            //println(showRaw(a), b, a.symbol, a.symbol.name, a.symbol.fullName, tree.symbol.fullName)
            val res = Ident(newTermName(b))
            //println(showRaw(res))
            res //Returning res causes failures; Scalac does not do name resolution again... unless we call resetAllAttrs after the transformation
            //super.transform(tree)
          case _ => super.transform(tree)
        }
      }
    }
    //val clearedExpr = c.Expr[Any](transformer2 transform c.resetAllAttrs(expr.tree))
    val clearedExpr = c.Expr[Any](transformer2 transform expr.tree)
    //val res = c.Expr[Interpreted[Sym, T]](c.resetAllAttrs(reify(new Interpreted[Sym, T]
    val res = c.Expr[Any](c.resetAllAttrs(reify(new Interpreted[Sym, T] {
        def apply(s: ThisLangIntf): s.Rep[T] = {
          import s._
          //expr.splice.asInstanceOf[s.Rep[T]]
          clearedExpr.splice.asInstanceOf[s.Rep[T]]
        }
      }).tree))
    println(showRaw(res))
    //println(showRaw(res.tree, printTypes = false))
    //println("Typechecking:")
    val typechecked = c.typeCheck(res.tree, silent = false)
    //println(typechecked)
    //println(showRaw(typechecked, printTypes = false))
    object visitor extends Transformer {
      override def transform(tree: Tree): Tree = {
        object TermNameEncoded {
          def unapply(t: TermName): Some[String] = Some(t.encoded)
        }
        tree match {
          //case Select(a, TermNameEncoded(b)) if a hasSymbolWhich (_.name == newTermName("ivm.expressiontree.Lifting")) =>
          //case Select(a, TermNameEncoded(b)) =>
          case TypeApply(fn, args) if fn.tpe == null =>
            printf("%s has null tpe after typechecking\n", fn)
            super.transform(tree)
          case _ => super.transform(tree)
        }
      }
    }
    visitor transform typechecked
    println("Typechecking done")
    res
  }
  def smart[T](expr: T): Any = macro smart_impl[T]
  def prefix = "smart_"
  /* To handle:
  scala> showRaw(reify(1.asInstanceOf: String).tree)
  res20: String = Typed(Select(Literal(Constant(1)), newTermName("asInstanceOf")), Ident(newTypeName("String")))
    */
  private val macroDebug = true
  val AnyTuple = "Tuple([0-9]+)".r
  val ConvToTuple = "tuple[0-9]+ToTuple[0-9]+Exp".r
  def smart_impl[T: c.AbsTypeTag](c: Context)(expr: c.Expr[T]): c.Expr[Any] = {
    import c.universe._

    //Too clever for Scalac - it doesn't get it:
    //val extr = extractors(c)
    //import extr._
    //causes below:
    /*
    [error] /app/home/pgiarrusso/src/LinqOnSteroids/src/main/scala/ivm/expressiontree/MacroFrontend.scala:123: pattern type is incompatible with expected type;
    [error]  found   : c.universe.NameApi
    [error]  required: c.universe.NameApi
    [error]           case Apply(Select(Select(Ident(TermNameEncoded("scala")), TermNameEncoded(AnyTuple)), TermNameEncoded("apply")), args @ List(_*)) =>
    */

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

    def println(x: => Any) = if (macroDebug) Predef println x
    def newline() = if (macroDebug) Predef println ()
    object smartTransformer extends Transformer {
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
            //TODO move this case at the end, so that explicit type applications which
            //are matched earlier are possible. We do match on isInstanceOf and
            //asInstanceOf, which are most cases.

            //println("#### Pos: " + tree.pos)
            //println("#### Positions: " + args.map(_.pos))
            //println("#### Tree: " + tree)
            //println("#### Polyterm: " + polyterm)
            transform(polyterm)

          //this duplicates the check but also checks arity. Do it even more
          //generic. Later.
          case Apply(Select(op1, member), l @ List())
            if (anyUnaryMethods ++ anyRefUnaryMethods ++ anyTypeUnaryMethod) contains member.decoded
          =>
            //Use reify and splices:
            //reify((c.Expr[Any => Nothing](Ident(newTermName(prefix +
              //member.encoded))).value)(op1))
              ////member.encoded))).value)(op1, l.map(c.Expr[Any](_).value):_*))
            //println("Op1: " + showRaw(op1))
            Apply(Ident(newTermName(prefix + member.encoded)), transform(op1) :: l)
          case Apply(Select(op1, member), l @ List(op2)) if anyBinaryMethods contains member.decoded =>
            Apply(Ident(newTermName(prefix + member.encoded)), (op1 :: l) map (transform(_)))
          case TypeApply(Select(op1, member), typeArgs @ List(typeArg))
            if anyTypeUnaryMethod contains member.decoded
          =>
            /*println("#### Pos: " + tree.pos)
            println("#### typeArg: " + typeArg)
            println("#### typeArg Position: " + typeArg.pos)
            println("#### typeArg Position: " + typeArg.pos.show)
            println("#### typeArg Position: " + typeArg.pos.isDefined)
            println("#### typeArg Position: " + typeArg.pos.pos)
            //println("#### typeArg Position: " + typeArg.pos.fileContent.map(identity)(collection.breakOut): String)
            println("#### Tree: " + tree)
            println("#### Tree: " + showRaw(tree))
            println("#### showRaw(typeArg): " + showRaw(typeArg))
            println("#### showRaw(typeArg.original): " + showRaw(typeArg.asInstanceOf[TypeTree].original))
            println("#### typeArg.original == null: " + (typeArg.asInstanceOf[TypeTree].original == null))*/
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

          case Apply(TypeApply(Select(Select(Ident(TermNameEncoded("scala")),
            TermNameEncoded(AnyTuple(arity))), TermNameEncoded("apply")), tArgs),
            args @ List(_*))
          =>
            Apply(Ident(newTermName("LiftTuple" + arity)), args map (transform(_)))

          case Apply(TypeApply(Select(Select(This(TypeNameEncoded("scala")),
            TermNameEncoded(AnyTuple(arity))), TermNameEncoded("apply")), tArgs),
            args @ List(_*))
          =>
            Apply(Ident(newTermName("LiftTuple" + arity)), args map (transform(_)))

          //Start removing implicit conversions, hoping they are readded later if needed.
          //It is strange that Lifting is not fully qualified here - I suspect that's only the case inside the ivm.expressiontree package!
//          case Apply(Apply(TypeApply(
//            Select(Select(Select(Ident(TermNameEncoded("ivm")), TermNameEncoded("expressiontree")), TermNameEncoded("Lifting")), TermNameEncoded("pure")),
//            tArgs), List(convertedTerm)), implicitArgs)
//          =>
          case Apply(Apply(TypeApply(Select(Ident(TermNameEncoded("Lifting")),
            TermNameEncoded("pure")), tArgs), List(convertedTerm)), implicitArgs)
          =>
            transform(convertedTerm)
          case Apply(TypeApply(Select(Ident(TermNameEncoded("Lifting")),
            TermNameEncoded(ConvToTuple())), tArgs), List(convertedTerm))
          =>
            transform(convertedTerm)
          case _ => super.transform(tree)
        }
        level -= 1
        ret
      }
    }
    //val thisReplaced = expr.tree.substituteThis(/*Get a symbol for the type scala*/
    //Then, using substituteSymbols (?) or sth. like that, replace references to Lifting with references to ivm.expressiontree.Lifting :-).
    newline() //Predef println ()
    println("#### Before transform: " + expr.tree)
    //println("#### Before transform: " + showRaw(expr.tree))
    val transformed = smartTransformer.transform(expr.tree)
    println("#### Transformed: " + transformed)
    //println("#### Transformed: " + showRaw(transformed))
    //println("#### Transformed: " + showRaw(transformed, printTypes = true))
    //resetAllAttrs comes from: https://github.com/retronym/macrocosm/blob/171be7e/src/main/scala/com/github/retronym/macrocosm/Macrocosm.scala#L171
    val afterReset = c.resetAllAttrs(transformed)
    //println("#### After reset: " + showRaw(afterReset, printTypes = true))
    //c.Expr(c.typeCheck(afterReset))
    c.Expr(afterReset)
  }
}

// vim: set sw=2 et:
