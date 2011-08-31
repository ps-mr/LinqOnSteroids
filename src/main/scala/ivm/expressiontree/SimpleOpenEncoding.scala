package ivm.expressiontree

/**
 * Here I show yet another encoding of expression trees, where methods
 * like +, &lt;= and so on can be added by other classes, rather than having to
 * be inserted in the original object.
 *
 * The original encoding of Klaus had this property but relied on an implicit
 * conversion from T to Exp[T] and then on additional ones from Exp[Double] to
 * DoubleExp, Exp[String] to StringExp, and so on. Quite a few times this did
 * not work because Scala never applies two implicit conversions on top of one
 * another.
 * This can be solved by making their composition available as another implicit conversion, and that's the solution
 * we show here.
 */
object SimpleOpenEncoding {
  import OpenEncoding.{ExpModule, BaseExprTree}

  trait OpsExpressionTree extends BaseExprTree with ExpModule {
    case class FuncExp[-T, +U](f: Exp[T] => Exp[U]) extends Exp[T => U] {
      def interpret = t => f(Const(t)).interpret
    }
  }

  /**
   * In comparison to the other encoding, we don't use CanBuildExp to get most specific types as result types, as
   * that implies that the type is obtained through type inference.
   * Instead, we use conversions from T => Exp[T], and
   * then specific ones Pi T: Numeric. Exp[T] => NumExp[T]; Pi T. Exp[Traversable[T]] => TraversableExp[T]
   */

  trait NumOpsExps {
    this: NumOpsExpressionTree with ExpModule =>
    class NumOps[T](val t: Exp[T])(implicit val isNum: Numeric[T])
    {
      def +(that: Exp[T]): Exp[T] = Plus(this.t, that)
    }
  }

  trait NumOpsExpressionTree {
    this: NumOpsExps with ExpModule =>
    //Root node for all binary, associative and commutative operations. The
    //intuition is that many operations (including optimizations) might apply
    //for all of those - e.g. expression normalization.
    abstract class CommutativeOp[T](a: Exp[T], b: Exp[T]) extends Exp[T]

    case class Plus[T](a: Exp[T], b: Exp[T])(implicit val isNum: Numeric[T]) extends CommutativeOp(a, b) {
      def interpret =
        isNum.plus(a.interpret, b.interpret)
    }
  }
  trait TraversableOpsExps {
    this: OpsExpressionTree =>
    class TraversableOps[T](val t: Exp[Traversable[T]]) /*extends Exp[Traversable[T]]*/ {
      //XXX: Here I should use Map, FlatMap and WithFilter nodes, instead of generic application nodes. But that's beside the point I'm making for this encoding.
      def map[U](f: Exp[T] => Exp[U]): Exp[Traversable[U]] =
        App((_: Traversable[T]) map (FuncExp(f).interpret), this.t)

      def flatMap[U](f: Exp[T] => Exp[Traversable[U]]): Exp[Traversable[U]] =
        App((_: Traversable[T]) flatMap FuncExp(f).interpret, this.t)

      def withFilter(f: Exp[T] => Exp[Boolean]): Exp[Traversable[T]] =
        App((_: Traversable[T]) filter FuncExp(f).interpret, this.t) //We can't use withFilter underneath.

      def union[U >: T](u: Exp[Traversable[U]]): Exp[Traversable[U]] =
        App((_: Traversable[T]) ++ u.interpret, this.t) //Should use an App node with two params.
    }
  }

  trait SimpleOpenEncodingBase extends OpsExpressionTree with NumOpsExps with NumOpsExpressionTree with TraversableOpsExps {
    implicit def toExp[T](t: T): Exp[T] = Const(t)

    implicit def expToNumExp[T : Numeric](t: Exp[T]): NumOps[T] = new NumOps(t)
    implicit def tToNumExp[T : Numeric](t: T): NumOps[T] = {
      //toExp(t)
      expToNumExp(t) //Doesn't work, unless we make NumOps not extends Exp. It should expand to:
      //expToNumExp(toExp(t)) //but it does not, because also the next expansion typechecks:
      //expToNumExp(tToNumExp(t))
    }

    implicit def expToTravExp[T](t: Exp[Traversable[T]]): TraversableOps[T] = new TraversableOps(t)
    implicit def tToTravExp[T](t: Traversable[T]): TraversableOps[T] = {
      //toExp(t)
      expToTravExp(t)
    }
  }

  object SimpleOpenEncoding extends SimpleOpenEncodingBase {
    class ToQueryable[T](t: Traversable[T]) {
      def asQueryable = Const(t)
    }
    implicit def toQueryable[T](t: Traversable[T]) = new ToQueryable(t)

    def show(name: String, v: Any) {
      print(name + ": ")
      println(v)
    }
    def moreTests() {
      println("testBug:")

      val i: Exp[Int] = 1
      show("i", i)
      val i1: Exp[Int] = 1
      show("i1", i1)
      //One of the syntaxes we want to support - both ones just fail, with "could not find implicit value for parameter cTTE: ivm.expressiontree.OpenEncoding.CanBuildExp[Int,ExpT]"
      //val a0: Exp[Traversable[Int]] = Seq(1, 2, 3, 5)
      //val a0: Exp[Traversable[Int]] = Seq(1, 2, 3, 5).toTraversable
      val a0: Exp[Traversable[Int]] = Seq(1, 2, 3, 5)
      show("a0", a0)

      /*val a1 = toExpTempl(Seq(1, 2, 3, 5)) //Doesn't work well - canBuildExp[Seq[Int]]: CanBuildExp[Seq[Int], Exp[Seq[Int]]] is preferred to canBuildExpTrav[Int, NumExp[Int]]: CanBuildExp[Traversable[Int], TraversableExp[Int]].
      show("a1", a1)
      val a2 = toExpTempl(Seq(1, 2, 3, 5).toTraversable) //Doesn't work well either
      show("a2", a2)
      val a3: Any = toExpTempl(Seq(1, 2, 3, 5)) //Works better
      show("a3", a3)
      val a4: Exp[Seq[Int]] = toExpTempl(Seq(1, 2, 3, 5)) //Doesn't work well - that's equivalent to a1
      show("a4", a4)
      val a5: Exp[Traversable[Int]] = toExpTempl(Seq(1, 2, 3, 5)) //Works well apparently.
      show("a5", a5)
      val a6: TraversableExp[Int, NumExp[Int]] = Seq(1, 2, 3, 5) //This one obviously works.
      show("a6", a6)

      show("(like a3) toExpTempl(Seq(1, 2, 3, 5))", toExpTempl(Seq(1, 2, 3, 5)))
      //show("toExpTempl(Seq(1, 2, 3, 5))(canBuildExpTrav)", toExpTempl(Seq(1, 2, 3, 5))(canBuildExpTrav))
      show("toExpTempl(Seq(1, 2, 3, 5).toTraversable)(canBuildExpTrav)", toExpTempl(Seq(1, 2, 3, 5).toTraversable)(canBuildExpTrav))*/

      println()
    }

    def testTraversable() {
      moreTests()

      val a: Exp[Traversable[Int]] = Seq(1, 2, 3, 5)
      val a2 = Seq(1, 2, 3, 5).asQueryable
      val b1 = a.map(_ + 1)
      val b2 = a2.map(1 + _)
      val b3 = b1.map(2 + _)
      show("b1", b1)
      show("b1.interpret", b1.interpret)
      show("b2", b2)
      show("b2.interpret", b2.interpret)
      show("b3", b3)
      show("b3.interpret", b3.interpret)
    }

    //Analogues of Exp.app. Given the different argument order, I needed to rename them to get a sensible name:
    def withExp[T, U](t: Exp[T])(f: T => U): Exp[U] = App(f, t)
    def withExpFunc[T, U](t: Exp[T])(f: Exp[T] => Exp[U]): Exp[U] = f(t)

    def main(args: Array[String]) {
      val a: Exp[Int] = 1
      val b = a + 2
      //Here we type inference fails:
      val c = Exp.app((x: Int) => x + 1, 1)
      //With smarter signatures, it works:
      val c2 = withExp(1)(_ + 1)
      val c3 = withExpFunc(1)(_ + 1)

      println(a)
      println(b)
      println(c)
      println(c2)
      println(c3)
      testTraversable()
    }
  }

  def main(args: Array[String]) {
    OpenEncoding.main(args)
    SimpleOpenEncoding.main(args)
  }
}