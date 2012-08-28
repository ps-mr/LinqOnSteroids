import scala.reflect.macros.Context
import collection.mutable.ListBuffer
import collection.mutable.Stack

trait MacroUtils {
  import MacroUtils._
  def stringify(arg: Any): String = macro stringify_impl
  def show(arg: Any) = macro show_impl
}
object MacroUtils {
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
}
object Macros extends MacroUtils {
  // macro definition is a normal function with anything you fancy in its signature
  // its body, though, is nothing more that a reference to an implementation
  def printf(format: String, params: Any*): Unit = macro printf_impl

  // macro implementation must correspond to macro definitions that use it
  // required signature is quite involved, but don't be scared
  // if the compiler is unhappy, it will print the signature it wants in the error message
  def printf_impl(c: Context)(format: c.Expr[String], params: c.Expr[Any]*): c.Expr[Unit] = {
    // compiler API is exposed in scala.reflect.macros.Context
    // its most important part, reflection API, is accessible via c.mirror
    // it's customary to import c.mirror._, because it includes a lot of routinely used stuff
    import c.universe._

    // first of all, we parse the provided format string
    // macros run during the compile-time, so they operate on trees, not on values
    // this means that the format parameter of our macro will be a compile-time literal
    // not an object of type java.lang.String
    // this also means that the code below won't work for printf("%d" + "%d", ...)
    // because in that case format won't be a string literal
    // but rather an AST that represents addition of two string literals
    // adjusting the macro to work for arbitrary stuff is left as an excercise for the reader
    val Literal(Constant(s_format: String)) = format.tree

    // here we jump straight into the compiler
    // the paragraph below creates temporary vals that precompute expressions being formatted
    // to learn more about dynamic generation of Scala code, take a look at our slides:
    // http://scalamacros.org/talks/2012-04-28-MetaprogrammingInScala210.pdf
    val evals = ListBuffer[ValDef]()
    def precompute(value: Tree, tpe: Type): Ident = {
      val freshName = newTermName(c.fresh("eval$"))
      evals += ValDef(Modifiers(), freshName, TypeTree(tpe), value)
      Ident(freshName)
    }

    // nothing fancy here, just bread and butter AST manipulations
    // extract trees from parameters of a macro, decompose/analyze and transform them
    // note how we get a hold of Scala types that correspond to Int and String
    // this works for a small set of core types
    // but in most cases you will have to create types by yourself
    // read up the aforementioned slides to learn more about types
    val paramsStack = Stack[Tree]((params map (_.tree)): _*)
    val refs = s_format.split("(?<=%[\\w%])|(?=%[\\w%])") map {
      case "%d" => precompute(paramsStack.pop, typeOf[Int])
      case "%s" => precompute(paramsStack.pop, typeOf[String])
      case "%%" => Literal(Constant("%"))
      case part => Literal(Constant(part))
    }

    // now we combine all the code we have generated into a Block
    // note the call to reify, which provides a shortcut for creating ASTs
    // learn more about reify in our documentation
    val stats = evals ++ refs.map(ref => reify(print(c.Expr[Any](ref).splice)).tree)
    c.Expr[Unit](Block(stats.toList, Literal(Constant(()))))
  }

  def smartOld(expr: Any): Any = macro smartOld_impl
  def smartOld_impl(c: Context)(expr: c.Expr[Any]): c.Expr[Any] = {
    import c.universe._
    expr.tree match {
      case Apply(Select(op1, member), List()) if member.decoded == "toString" =>
        c.Expr(Apply(Ident(newTermName("toString")), List(op1)))
      case _ => expr
    }
  }
  def smart2(expr: Any): Any = macro smart2_impl
  def smart2_impl(c: Context)(expr: c.Expr[Any]): c.Expr[Any] = {
    import c.universe._
    expr.tree match {
      case Apply(Select(op1, member), List(op2)) if member.decoded == "==" =>
        c.Expr(Apply(Ident(newTermName("eq")), List(op1, op2)))
      case _ => expr
    }
  }
  //def anyBinary(v: String): Boolean
  //def anyRefBinary
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
        c.Expr(Apply(Ident(newTermName("smart_" + member.encoded)), op1 :: l))
        //c.Expr(Apply(Ident(newTermName("toString")), List(op1)))
        //c.Expr(Apply(Ident(member), List(op1)))
      case Apply(Select(op1, member), l @ List(op2)) if anyBinaryMethods contains member.decoded =>
        c.Expr(Apply(Ident(newTermName("smart_" + member.encoded)), op1 :: l))
//      case Apply(Select(op1, member), List(op2)) if member.decoded == "==" =>
//        c.Expr(Apply(Ident(newTermName("eq")), List(op1, op2)))
//      case Apply(Select(op1, member), List(op2)) if member.decoded == "!=" =>
//        c.Expr(Apply(Ident(newTermName("neq")), List(op1, op2)))
      case _ => expr
    }
  }*/
  def smart(expr: Any): Any = macro smart_impl
  /* To handle:
  scala> showRaw(reify(1.asInstanceOf: String).tree)
  res20: String = Typed(Select(Literal(Constant(1)), newTermName("asInstanceOf")), Ident(newTypeName("String")))
    */
  private val macroDebug = true
  val AnyTuple = "Tuple[0-9]+".r
  def smart_impl(c: Context)(expr: c.Expr[Any]): c.Expr[Any] = {
    import c.universe._

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
        println("Level %d, tree %s" format(level, showRaw(tree)))
        level += 1
        val ret = tree match {
          //this duplicates the check but also checks arity. Do it even more
          //generic. Later.
          case Apply(Select(op1, member), l @ List())
            if (anyUnaryMethods ++ anyRefUnaryMethods ++ anyTypeUnaryMethod) contains member.decoded
          =>
            //Use reify and splices:
            //reify((c.Expr[Any => Nothing](Ident(newTermName("smart_" +
              //member.encoded))).value)(op1))
              ////member.encoded))).value)(op1, l.map(c.Expr[Any](_).value):_*))
            println("Op1: " + showRaw(op1))
            Apply(Ident(newTermName("smart_" + member.encoded)), transform(op1) :: l)
          case Apply(Select(op1, member), l @ List(op2)) if anyBinaryMethods contains member.decoded =>
            Apply(Ident(newTermName("smart_" + member.encoded)), (op1 :: l) map (transform(_)))
          case TypeApply(Select(op1, member), typeArgs @ List(typeArg))
            if anyTypeUnaryMethod contains member.decoded
          =>
            Apply(TypeApply(
              Ident(newTermName("smart_" + member.encoded)), typeArgs), List(transform(op1)))
          case Apply(
            TypeApply(Select(op1, member), typeArgs @ List(typeArg)),
            l2 @ List(arg))
            if anyTypeBinaryMethod contains member.decoded
          =>
            Apply(TypeApply(
              Ident(newTermName("smart_" + member.encoded)), typeArgs), (op1 :: l2) map (transform(_)))
          //case Apply(TypeApply(Select(Select(Ident(TermNameEncoded("scala")), TermNameEncoded(AnyTuple)), TermNameEncoded("apply")), tArgs), args @ List(_*)) =>
          case Apply(TypeApply(Select(Select(Ident(TermNameEncoded("scala")), TermNameEncoded(AnyTuple())), TermNameEncoded("apply")), tArgs), args @ List(_*)) =>
          //case Apply(TypeApply(Select(Select(Ident(t1 /*TermNameEncoded("scala")*/), t2 /*TermNameEncoded(AnyTuple)*/), t3 /*TermNameEncoded("apply")*/), tArgs), args @ List(_*)) =>
            println("####### " + showRaw(tree))
            //println(showRaw(t1.encoded))
            //println(showRaw(t2.encoded))
            //println(showRaw(t3.encoded))
            // Apply(TypeApply(Ident(newTermName("asExp")), tArgs), super.transform(tree) :: Nil)
            Apply(Ident(newTermName("asExp")), super.transform(tree) :: Nil)
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

