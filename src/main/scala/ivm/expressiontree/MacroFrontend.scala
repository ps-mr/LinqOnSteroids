package ivm
package expressiontree

import Lifting._
import scala.reflect.macros.Context
import language.experimental.macros

object Macros {
  val anyUnaryMethods = List("toString", "hashCode", "getClass", "##")
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
  private val macroDebug = false
  def smart_impl(c: Context)(expr: c.Expr[Any]): c.Expr[Any] = {
    import c.universe._
    def println(x: => Any) = if (macroDebug) Predef println x
    object smartTransformer extends Transformer {
      var level = 0
      override def transform(tree: Tree): Tree = {
        println("Level %d, tree %s" format(level, showRaw(tree)))
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
            println("Op1: " + showRaw(op1))
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
          case _ => super.transform(tree)
        }
        level -= 1
        ret
      }
    }
    println(showRaw(expr.tree))
    c.Expr(c.resetAllAttrs(smartTransformer.transform(expr.tree)))
    //Or maybe
    //c.Expr(c.resetAllAttrs(...)), as below?
    //https://github.com/retronym/macrocosm/blob/171be7e/src/main/scala/com/github/retronym/macrocosm/Macrocosm.scala#L171
  }
}

// vim: set ts=4 sw=4 et:
