package ivm
package expressiontree

import Lifting._
import scala.reflect.macros.Context
import language.experimental.macros

object UtilsForMacros {
  def extractors(c: Context) = new {
    import c.universe._
    /*
    object TermNameDecoded {
      def unapply(t: TermName): Some[String] = Some(t.decoded)
    }
    object TermNameEncoded {
      def unapply(t: TermName): Some[String] = Some(t.encoded)
    }
    */
    object TermNameDecoded {
      def unapply(t: NameApi): Option[String] =
        t match {
          case _: TermName =>
            Some(t.decoded)
          case _ => None
        }
    }
    object TermNameEncoded {
      def unapply(t: NameApi): Option[String] =
        t match {
          case _: TermName =>
            Some(t.encoded)
          case _ => None
        }
    }
  }
}
object Macros {
  def stringify(arg: Any): String = macro stringify_impl
  def show(arg: Any) = macro show_impl
  def ctShow(arg: Any) = macro ctShow_impl
  def ctShowDebug(arg: Any) = macro ctShowDebug_impl

  def stringify_base(c: Context)(arg: c.Expr[Any]): String =
    arg.tree.toString //The result here is a bit ugly - we need to print the tree before desugaring.
  def stringify_impl(c: Context)(arg: c.Expr[Any]): c.Expr[String] = {
    import c.universe._
    //c.Expr[String](Literal(Constant(stringify_base(c)(arg))))
    //reify(c.Expr[String](Literal(Constant(stringify_base(c)(arg)))).splice)
    val v = stringify_base(c)(arg)
    //reify(v) // This reifies the variable reference.
    //println(showRaw(reify(v).tree))
    c.Expr[String](Literal(Constant(v)))
  }

  def show_impl(c: Context)(arg: c.Expr[Any]): c.Expr[Unit] = {
    import c.universe._
    val v = stringify_base(c)(arg)
    //val v1 = reify(v)
    val v1 = c.Expr[String](Literal(Constant(v)))
    reify(println("Expr: %s evaluates to %s" format (v1.splice, arg.splice)))
  }
  def ctShow_impl(c: Context)(arg: c.Expr[Any]): c.Expr[Unit] = {
    import c.universe._
    println(stringify_base(c)(arg))
    reify(())
  }

  def ctShowDebug_impl(c: Context)(arg: c.Expr[Any]): c.Expr[Any] = {
    import c.universe._
    println(stringify_base(c)(arg))
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
  def smart(expr: Any): Any = macro smart_impl
  def prefix = "smart_"
  /* To handle:
  scala> showRaw(reify(1.asInstanceOf: String).tree)
  res20: String = Typed(Select(Literal(Constant(1)), newTermName("asInstanceOf")), Ident(newTypeName("String")))
    */
  private val macroDebug = true
  val AnyTuple = "Tuple([0-9]+)".r
  def smart_impl(c: Context)(expr: c.Expr[Any]): c.Expr[Any] = {
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

    def println(x: => Any) = if (macroDebug) Predef println x
    object smartTransformer extends Transformer {
      var level = 0
      override def transform(tree: Tree): Tree = {
        //println("Level %d, tree %s" format(level, showRaw(tree)))
        level += 1
        val ret = tree match {
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
          case Apply(TypeApply(Select(Select(Ident(TermNameEncoded("scala")), TermNameEncoded(AnyTuple(arity))), TermNameEncoded("apply")), tArgs), args @ List(_*)) =>
            //println("Foo! " + showRaw(tree))
            Apply(Ident(newTermName("LiftProduct" + arity)), args map (transform(_)))

          //Start removing implicit conversions, hoping they are readded later if needed.
          //It is strange that Lifting is not fully qualified here - I suspect that's only the case inside the ivm.expressiontree package!
//          case Apply(Apply(TypeApply(
//            Select(Select(Select(Ident(TermNameEncoded("ivm")), TermNameEncoded("expressiontree")), TermNameEncoded("Lifting")), TermNameEncoded("pure")),
//            tArgs), List(convertedTerm)), implicitArgs)
//          =>
          case Apply(Apply(TypeApply(Select(Ident(TermNameEncoded("Lifting")), TermNameEncoded("pure")), tArgs), List(convertedTerm)), implicitArgs) =>
            //println("#### Depure: a: %s; b: %s; c: %s" format (showRaw(a, printTypes = true, printIds = true), showRaw(b, printIds = true), showRaw(c, printTypes = true)))
            val afterResetCT = c.resetAllAttrs(convertedTerm)
            println("#### Depure: " + showRaw(tree, printTypes = true))
            println("#### gives: " + showRaw(convertedTerm, printTypes = true))
            println("#### after reset: " + showRaw(afterResetCT, printTypes = true))
            //println("#### gives: " + showRaw(convertedTerm))
            //super.transform(tree)
            super.transform(convertedTerm)

          case _ => super.transform(tree)
        }
        level -= 1
        ret
      }
    }
    //println(expr.tree)
    //println(showRaw(expr.tree))
    val transformed = smartTransformer.transform(expr.tree)
    println(transformed)
    val afterReset = c.resetAllAttrs(transformed)
    println(afterReset)
    c.Expr(afterReset)
    //Or maybe
    //c.Expr(c.resetAllAttrs(...)), as below?
    //https://github.com/retronym/macrocosm/blob/171be7e/src/main/scala/com/github/retronym/macrocosm/Macrocosm.scala#L171
  }
}

// vim: set sw=2 et:
