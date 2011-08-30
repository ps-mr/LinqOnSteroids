package ivm.expressiontree
import scala.math.Numeric

/**
 * Here I experiment with an alternative encoding of Expression, where methods
 * like +, &lt;= and so on can be added by other classes, rather than having to
 * be inserted in the original object.
 *
 * The original encoding of Klaus had this property but relied on an implicit
 * conversion from T to Exp[T] and then on additional ones from Exp[Double] to
 * DoubleExp, Exp[String] to StringExp, and so on. Quite a few times this did
 * not work because Scala never applies two implicit conversions on top of one
 * another.
 * The approach here is to convert directly Double to DoubleExp, String to
 * StringExp, and so on.
 * We have a generic conversion toExp: T => Exp[T], but we ensure that it has
 * lower priority than the others, by defining toExp in a trait and the
 * others in an object inheriting from that trait.
 * This extra step would not be needed if toExpTempl were found by the Scala
 * compiler, which is not because of what I believe to be a bug.
 *
 * Currently, we experience ambiguity when triggering the initial conversion - see testBug().
 * The Scala spec explains that for Scala type inference there is no single best solution, and the compiler is free
 * to choose any one. When inferring implicit parameters or conversions, it chooses always the most specific solution
 * (where "most specific" is defined as in overload resolution) and gives an error if no single solution exists.
 * However, the encoding we use is the same as for Scala collection libraries. Therefore, it's not clear at all what happens.
 *
 * Alternatively, if we avoid supporting polymorphic lifting like in the current
 * encoding, I think we would solve all current problems would disappear. 
 * @author Paolo G. Giarrusso
 */

object OpenEncoding {
  trait Exp[+T] {
    def interpret: T
  }

  //When building an expression, we need to construct the right object, but I think that a
  //trick like CanBuildFrom should work.
  //Note: To is supposed to be a subclass of Exp[Elem], but the following
  //declaration does not compile because of variance problems - I should make
  //Elem invariant, which is not what I want:
  //trait CanBuildExp[-Elem, +To <: Exp[Elem]]
  trait BaseExprTree {
    //A few nodes for expression trees.
    case class Const[T](t: T) extends Exp[T] {
      def interpret = t
    }
    //Not exactly sure what I should use to represent applications.
    case class App[T, U](f: T => U, t: Exp[T]) extends Exp[U] {
      def interpret = f(t.interpret)
    }
    object Exp {
      def app[T, U](f: T => U, t: Exp[T]): Exp[U] = App(f, t)
    }
  }

  trait OpsExpressionTree extends BaseExprTree {
    case class FuncExp[-T, +U](f: Exp[T] => Exp[U]) extends Exp[T => U] {
      def interpret = t => f(Const(t)).interpret
    }
  }

  trait CanBuildExpExpressionTree extends BaseExprTree {
    //Instances work as implicit conversions themselves because this trait extends Function.
    trait CanBuildExp[-Elem, +To] extends (Exp[Elem] => To) {
      def apply(e: Exp[Elem]): To
    }
    //XXX: in comparison to Scala collection, we are less flexible - if you upcast a value, a different implicit might be selected, leading to different code being executed.
    //In Scala, implicits are used just to have a finer static type, but the actual code to be executed is chosen at runtime - most instances of CanBuildFrom delegate construction to the original collection!
    //However, having a precise static type determines which operations are available. So, even if we make our code more dynamic like for Scala collections, we still need to ensure the best static types are inferred.
    //Moreover, in the current implementation I need to use wrapper nodes; while they probably cannot be avoided altogether, we should save a few of them - in particular, instead of App nodes we should
    //use specialized Map, FlatMap and WithFilter nodes, extending TraversableExp.

    class ExpWrapper[+T](t: Exp[T]) extends Exp[T] {
      def interpret = t.interpret
    }

    /*case class App2[T, U](f: Exp[T] => Exp[U], t: Exp[T]) extends Exp[U] {
      def interpret = f(t).interpret
    }*/

    //T cannot be contravariant here. Also ExpT must be invariant. Damn!
    //case class FuncExp[-T, +U, +ExpT2 <: ExpT, -ExpT <: Exp[T]](f: ExpT => Exp[U])(implicit val cbeT: CanBuildExp[T, ExpT2]) extends Exp[T => U]
    case class FuncExp[T, +U, ExpT <: Exp[T]](f: ExpT => Exp[U])(implicit val cbeT: CanBuildExp[T, ExpT]) extends Exp[T => U] {
      def interpret = t => f(cbeT(Const(t))).interpret
    }
  }

  trait NumExps {
    this: CanBuildExpExpressionTree with NumExpressionTree =>
    //Alternative encoding of operations which are not available for every Exp[T]
    //but only for e.g. Exp[String], Exp[T <% Numeric] and so on.
    //Idea: instead of having all those methods in Exp[T], and instead of having to
    //convert instances of Exp[T] to instances of DoubleExp, StringExp, and so
    //on, ensure that automatic conversions automatically and directly produce
    //instances of DoubleExp, StringExp. This avoids the old problem with
    //needing to apply two implicit conversions in sequence. Additionally,
    //methods like map and so on reuse a trick like CanBuildFrom in scala
    //collections to produce the right expression type.
    //The mixins containing extra methods must be mixed in with ExpWrapper.

    //trait NumExp[T](implicit isNum: Numeric[T]) extends Exp[T]
    trait NumExp[T] extends Exp[T] {
      //If this were not a mixin, this would be an implicit parameter:
      implicit val isNum: Numeric[T]
      def +(that: NumExp[T]): NumExp[T] = Plus(this, that)
    }

    case class NumExpWrap[T](t: Exp[T])(implicit val isNum: Numeric[T]) extends
      ExpWrapper[T](t) with NumExp[T]
  }

  trait TraversableExps {
    this: CanBuildExpExpressionTree =>
    trait TraversableExp[T, ExpT <: Exp[T]] extends Exp[Traversable[T]] {
      implicit val cbeT: CanBuildExp[T, ExpT]
      //XXX: Here I should use Map, FlatMap and WithFilter nodes, instead of generic application nodes. But that's beside the point I'm making for this encoding.
      def map[U, That <: Exp[Traversable[U]]](f: ExpT => Exp[U])(implicit c: CanBuildExp[Traversable[U], That]): That =
        c apply App((_: Traversable[T]) map (FuncExp[T, U, ExpT](f).interpret), this) //c apply can be omitted, but that's less clear

      def flatMap[U, That <: Exp[Traversable[U]]](f: ExpT => Exp[Traversable[U]])(implicit c: CanBuildExp[Traversable[U], That]): That =
        c apply App((_: Traversable[T]) flatMap FuncExp[T, Traversable[U], ExpT](f).interpret, this)

      //XXX: probably we could just return the same type as us... couldn't we? Probably yes, but we still need a CanBuildExp to actually do the creation.
      def withFilter[That <: Exp[Traversable[T]]](f: ExpT => Exp[Boolean])(implicit c: CanBuildExp[Traversable[T], That]): That =
        c apply App((_: Traversable[T]) filter FuncExp[T, Boolean, ExpT](f).interpret, this) //We can't use withFilter underneath.

      def union[U >: T, That <: Exp[Traversable[U]]](u: Exp[Traversable[U]])(implicit c: CanBuildExp[Traversable[U], That]): That =
        c apply App((_: Traversable[T]) ++ u.interpret, this) //Should use an App node with two params.
    }
    case class TraversableExpWrap[T, ExpT <: Exp[T]](t: Exp[Traversable[T]])(implicit val cbeT: CanBuildExp[T, ExpT]) extends
      ExpWrapper[Traversable[T]](t) with TraversableExp[T, ExpT]
  }
  trait NumExpressionTree {
    this: NumExps =>
    //Root node for all binary, associative and commutative operations. The
    //intuition is that many operations (including optimizations) might apply
    //for all of those - e.g. expression normalization.
    abstract class CommutativeOp[T](a: NumExp[T], b: NumExp[T]) extends NumExp[T]

    case class Plus[T](a: NumExp[T], b: NumExp[T])(implicit val isNum: Numeric[T]) extends CommutativeOp(a, b) {
      def interpret =
        isNum.plus(a.interpret, b.interpret)
    }
  }

  trait OpenEncodingBase extends CanBuildExpExpressionTree with NumExps with TraversableExps with NumExpressionTree {
    //Use CanBuildExp for more precise automatic lifting. Problem: Scala seems
    //too shy to apply this conversion automatically, because "That" does not
    //match so obviously with the target type. Therefore, specializations such
    //as toNumExp are needed.

    //Proposal 1:
    implicit def toExpTempl[T, That](t: T)(implicit c: CanBuildExp[T, That]): That =
    //Problem: That can be too generic, say That = Any.

    //Proposal 2:
    //implicit def toExpTempl[T, That <: Exp[T]](t: T)(implicit c: CanBuildExp[T, That]): That =
    //Problem: T is the exact static type of t, we might not have a specific CanBuildExp.
    // E.g.: T = Seq[U] => That <: Exp[Seq[T]] => That == Exp[Seq[T]], as TraversableExp[T] and Exp[Seq[T]] are incomparable.

    //Proposal 3:
    //implicit def toExpTempl[T, U >: T, That <: Exp[U]](t: T)(implicit c: CanBuildExp[U, That]): That =
    //Problem: canBuildExpTrav does not get picked unless you pass it explicitly, even if the right types are deduced for T and U.
    //In those cases, That is decided too early; and T is never inferred to be different from U.
    //Insight: different best solutions are possible for type inference, and any of them can be chosen (SLS 2.9, Sec. 6.26.4, Local Type Inference).

      //c apply toExp(t)
      c apply Const(t)

    //Examples of CanBuildExp.
    implicit def canBuildExp[T]: CanBuildExp[T, Exp[T]] = new CanBuildExp[T, Exp[T]] {
      def apply(e: Exp[T]) = e
    }
    implicit def toExp[T](t: T): Exp[T] = toExpTempl(t)

    /*
    implicit def canBuildExpString: CanBuildExp[String, Exp[String]] = new CanBuildExp[String, Exp[String]] {
      def apply(e: Exp[String]) = e
    }
    implicit def toExp(t: String): Exp[String] = toExpTempl(t)
    */

    //implicit object canBuildExpStr extends CanBuildExp[String, StringExp]
    implicit def canBuildExpNum[T](implicit numT: Numeric[T]) =
      new CanBuildExp[T, NumExp[T]] {
        def apply(e: Exp[T]) = NumExpWrap(e)
      }
    implicit def canBuildExpTrav[T, ExpT <: Exp[T]](implicit cCBET: CanBuildExp[T, ExpT]): CanBuildExp[Traversable[T], TraversableExp[T, ExpT]] =
      new CanBuildExp[Traversable[T], TraversableExp[T, ExpT]] {
        def apply(e: Exp[Traversable[T]]) = TraversableExpWrap(e)//(c)
      }
    /*implicit def canBuildExpTrav[T, TravT <: Traversable[T], ExpT <: Exp[T]](implicit c: CanBuildExp[T, ExpT]): CanBuildExp[TravT, TraversableExp[T, ExpT]] =
      new CanBuildExp[Traversable[T], TraversableExp[T, ExpT]] {
        def apply(e: Exp[Traversable[T]]) = TraversableExpWrap(e)(c)
      }*/
  }

  object OpenEncoding extends OpenEncodingBase {
    //Overload resolution, here, can select the most specific implicit to pass
    //to toExpTempl. The difference between these two methods is thus in the
    //implicitly passed parameter (canBuildExpNum vs canBuildExp)
    implicit def toNumExp[T : Numeric](t: T): NumExp[T] = toExpTempl(t)
    implicit def toTraversableExp[T, ExpT <: Exp[T]](t: Traversable[T])(implicit cTTE: CanBuildExp[T, ExpT]): TraversableExp[T, ExpT] = toExpTempl(t)

    //Question: does implicit lookup find a most-specific solution just because
    //the target type is NumExp[T], and only passing canBuildExpNum gives that
    //type? toNumExp2 shows that this is not the case.
    def _toNumExp2[T : Numeric](t: T): Exp[T] = toExpTempl(t)

    def testBug() {
      println("testBug:")

      def show(name: String, v: Any) {
        print(name + ": ")
        println(v)
      }
      val i: Exp[Int] = 1
      show("i", i)
      val i1: Exp[Int] = toExpTempl(1)
      show("i1", i1)
      //One of the syntaxes we want to support - both ones just fail, with "could not find implicit value for parameter cTTE: ivm.expressiontree.OpenEncoding.CanBuildExp[Int,ExpT]"
      //val a0: Exp[Traversable[Int]] = Seq(1, 2, 3, 5)
      //val a0: Exp[Traversable[Int]] = Seq(1, 2, 3, 5).toTraversable
      val a0: Exp[Traversable[Int]] = toTraversableExp(Seq(1, 2, 3, 5))
      //show("a0", a0)

      val a1 = toExpTempl(Seq(1, 2, 3, 5)) //Doesn't work well - canBuildExp[Seq[Int]]: CanBuildExp[Seq[Int], Exp[Seq[Int]]] is preferred to canBuildExpTrav[Int, NumExp[Int]]: CanBuildExp[Traversable[Int], TraversableExp[Int]].
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
      show("toExpTempl(Seq(1, 2, 3, 5).toTraversable)(canBuildExpTrav)", toExpTempl(Seq(1, 2, 3, 5).toTraversable)(canBuildExpTrav))

      println()
    }

    def testTraversable() {
      testBug()
      
      val a = toTraversableExp(Seq(1, 2, 3, 5))
      val b = a.map(_ + 1)
      val b2 = a.map(1 + _)
      println(b)
      println(b.interpret)
      println(b2)
      println(b2.interpret)
    }
    def main(args: Array[String]) {
      val a: NumExp[Int] = 1
      val b = a + 2
      val c = Exp.app((x: Int) => x + 1, 1)

      println(b)
      println(c)
      println()
      testTraversable()
    }
  }

  /**
   * Now we start with a much simpler encoding. We don't use CanBuildExp to get most specific types as result types, as
   * that implies that the type is obtained through type inference. Instead, we use conversions from T => Exp[T], and
   * then specific ones Pi T: Numeric. Exp[T] => NumExp[T]; Pi T. Exp[Traversable[T]] => TraversableExp[T]
   */

  trait NumOpsExps {
    this: NumOpsExpressionTree =>
    class NumOps[T](val t: Exp[T])(implicit val isNum: Numeric[T])
      //extends ExpWrapper[T](t) //this line is optional!
    {
      def +(that: Exp[T]): Exp[T] = Plus(this.t, that)
    }
  }

  trait NumOpsExpressionTree {
    this: NumOpsExps =>
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
      val b1 = a.map(_ + 1)
      val b2 = a.map(1 + _)
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
    println("SimpleOpenEncoding.main")
    SimpleOpenEncoding.main(args)
  }
}

// vim: set sw=4 et:
