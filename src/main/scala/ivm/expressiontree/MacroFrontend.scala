package ivm
package expressiontree

import Lifting._
import scala.reflect.macros.Context
import language.experimental.macros

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
object Macros {
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
  //def anyUnary(v: String): Boolean =

  /*def smart(expr: Any): Any = macro smart_impl
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
//      case Apply(Select(op1, member), List(op2)) if member.decoded == "==" =>
//        c.Expr(Apply(Ident(newTermName("eq")), List(op1, op2)))
//      case Apply(Select(op1, member), List(op2)) if member.decoded == "!=" =>
//        c.Expr(Apply(Ident(newTermName("neq")), List(op1, op2)))
      case _ => expr
    }
  }*/
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
    object smartTransformer extends Transformer {
      var level = 0
      override def transform(tree: Tree): Tree = {
        //println("Level %d, tree %s" format(level, showRaw(tree)))
        level += 1
        val ret = tree match {
          case TypeApply(polyterm, arg) =>
            //Drops result of type inference, as well as explicit type application. Hm.
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
    val transformed = smartTransformer.transform(expr.tree)
    //println("#### Transformed: " + showRaw(transformed, printTypes = true))
    println("#### Transformed: " + transformed)
    //println("#### Transformed: " + transformed)
    //resetAllAttrs comes from: https://github.com/retronym/macrocosm/blob/171be7e/src/main/scala/com/github/retronym/macrocosm/Macrocosm.scala#L171
    val afterReset = c.resetAllAttrs(transformed)
    //println("#### After reset: " + showRaw(afterReset, printTypes = true))
    //c.Expr(c.typeCheck(afterReset))
    c.Expr(afterReset)
  }
}

// vim: set sw=2 et:
