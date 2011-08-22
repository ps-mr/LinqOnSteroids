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
 * StringExp, and so on. However, it seems it does not quite work yet - see the
 * definition of c in main. It would work if toExpTempl were found by the Scala
 * compiler, which is not because of what I believe to be a bug.
 *
 * An alternative solution is to make the generic conversion toExp: T => Exp[T]
 * have lower priority than the others, by defining toExp in a trait and the
 * others in an object inheriting from that trait.
 * @author Paolo G. Giarrusso
 */

object OpenEncoding {
  trait Exp[+T] {
    def interpret: T
    /*def map[U, That <: Exp[U]](f: T => U)(implicit c: CanBuildExp[U, That]): That =
      c buildExp Exp.app(f, this)*/
    //TODO add flatMap and withFilter.
  }

  //The other problem is constructing the right object, but I think that a
  //trick like CanBuildFrom should work.
  //Note: To is supposed to be a subclass of Exp[Elem], but the following
  //declaration does not compile because of variance problems - I should make
  //Elem invariant, which is not what I want:
  //trait CanBuildExp[-Elem, +To <: Exp[Elem]]
  trait CanBuildExp[-Elem, +To] {
    def buildExp(e: Exp[Elem]): To
    //XXX: This naming is probably better - maybe instances would then work as implicit conversions themselves!
    def apply(e: Exp[Elem]): To = buildExp(e)
  }

  class ExpWrapper[+T](t: Exp[T]) extends Exp[T] {
    def interpret = t.interpret
  }

  trait ExpressionTree {
    //A few nodes for expression trees.
    case class Const[T](t: T) extends Exp[T] {
      def interpret = t
    }
    //Not exactly sure what I should use to represent applications.
    case class App[T, U](f: T => U, t: Exp[T]) extends Exp[U] {
      def interpret = f(t.interpret)
    }
    /*case class App2[T, U](f: Exp[T] => Exp[U], t: Exp[T]) extends Exp[U] {
      def interpret = f(t).interpret
    }*/

    object Exp {
      def app[T, U](f: T => U, t: Exp[T]): Exp[U] = App(f, t)
    }
    //T cannot be contravariant here. Damn!
    case class FuncExp[T, +U, ExpT <: Exp[T]](f: ExpT => Exp[U])(implicit val cbeT: CanBuildExp[T, ExpT]) extends Exp[T => U] {
      def interpret = t => f(cbeT(Const(t))).interpret
    }
  }

  trait NumExps {
    this: ExpressionTree with NumExpressionTree =>
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
      def isNum: Numeric[T]
      def +(that: NumExp[T]): NumExp[T] = Plus(this, that)(isNum)
    }

    case class NumExpWrap[T](t: Exp[T])(implicit val isNum: Numeric[T]) extends
      ExpWrapper[T](t) with NumExp[T]
  }

  trait TraversableExps {
    this: ExpressionTree =>
    trait TraversableExp[T, ExpT <: Exp[T]] extends Exp[Traversable[T]] {
      implicit val cbeT: CanBuildExp[T, ExpT]
      //XXX: Here I should use Map, FlatMap and WithFilter nodes, instead of generic application nodes. But that's beside the point I'm making for this encoding.
      def map[U, That <: Exp[Traversable[U]]](f: ExpT => Exp[U])(implicit c: CanBuildExp[Traversable[U], That]): That =
        c buildExp App((_: Traversable[T]) map (FuncExp[T, U, ExpT](f).interpret), this)

      def flatMap[U, That <: Exp[Traversable[U]]](f: ExpT => Exp[Traversable[U]])(implicit c: CanBuildExp[Traversable[U], That]): That =
        c buildExp App((_: Traversable[T]) flatMap FuncExp[T, Traversable[U], ExpT](f).interpret, this)

      //XXX: probably we could just return the same type as us... couldn't we? Probably yes, but we still need a CanBuildExp to actually do the creation.
      def withFilter[That <: Exp[Traversable[T]]](f: ExpT => Exp[Boolean])(implicit c: CanBuildExp[Traversable[T], That]): That =
        c buildExp App((_: Traversable[T]) filter FuncExp[T, Boolean, ExpT](f).interpret, this) //We can't use withFilter underneath.

      def union[U >: T, That <: Exp[Traversable[U]]](u: Exp[Traversable[U]])(implicit c: CanBuildExp[Traversable[U], That]): That =
        c buildExp App((_: Traversable[T]) ++ u.interpret, this) //Should use an App node with two params.
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

  trait OpenEncodingBase extends NumExps with TraversableExps with ExpressionTree with NumExpressionTree {
    //Use CanBuildExp for more precise automatic lifting. Problem: Scala seems
    //to shy to apply this conversion automatically, because "That" does not
    //match so obviously with the target type. Therefore, specializations such
    //as toNumExp are needed.
    //implicit def toExpTempl[T, That](t: T)(implicit c: CanBuildExp[T, That]): That = c buildExp toExp(t)
    implicit def toExpTempl[T, That](t: T)(implicit c: CanBuildExp[T, That]): That = c buildExp Const(t)

    //Examples of CanBuildExp.
    implicit def canBuildExp[T] = new CanBuildExp[T, Exp[T]] {
      def buildExp(e: Exp[T]) = e
    }
    //implicit object canBuildExpStr extends CanBuildExp[String, StringExp]
    implicit def canBuildExpNum[T](implicit numT: Numeric[T]) =
      new CanBuildExp[T, NumExp[T]] {
        def buildExp(e: Exp[T]) = NumExpWrap(e)
      }
    implicit def toExp[T](t: T): Exp[T] = toExpTempl(t)
    implicit def canBuildExpTrav[T, ExpT <: Exp[T]](implicit c: CanBuildExp[T, ExpT]) =
      new CanBuildExp[Traversable[T], TraversableExp[T, ExpT]] {
        def buildExp(e: Exp[Traversable[T]]) = TraversableExpWrap(e)(c)
      }
  }

  object OpenEncoding extends OpenEncodingBase {
    //Overload resolution, here, can select the most specific implicit to pass
    //to toExpTempl. The difference between these two methods is thus in the
    //implicitly passed parameter (canBuildExpNum vs canBuildExp)
    implicit def toNumExp[T : Numeric](t: T): NumExp[T] = toExpTempl(t)
    implicit def toTraversableExp[T, ExpT <: Exp[T]](t: Traversable[T])(implicit c: CanBuildExp[T, ExpT]): TraversableExp[T, ExpT] = toExpTempl(t)

    //Question: does implicit lookup find a most-specific solution just because
    //the target type is NumExp[T], and only passing canBuildExpNum gives that
    //type? toNumExp2 shows that this is not the case.
    def _toNumExp2[T : Numeric](t: T): Exp[T] = toExpTempl(t)

    def testTraversable() {
      val a1: TraversableExp[Int, NumExp[Int]] = Seq(1, 2, 3, 5)
      val a2 = toExpTempl(Seq(1, 2, 3, 5).toTraversable) //Doesn't work well
      val a = toTraversableExp(Seq(1, 2, 3, 5))
      val b = a.map(_ + 1)
      val b2 = a.map(1 + _)
      //assert (b == b2) //fails!
      println(b)
      println(b.interpret)
      println(b2)
      println(b2.interpret)
    }
    def main(args: Array[String]) {
      val a: NumExp[Int] = 1
      val b = a + 2
      val c = Exp.app((x: Int) => x + 1, 1)
      //Does not work! Implicit conversion does not choose the most-specific one.
      //However, it does prefer to choose implicits defined in a subclass to implicits in a superclass.
      val d = Exp.app((x: Int) => x + 1, 1) //This works.
      println(b)
      testTraversable()
    }
  }
  def main(args: Array[String]) {
    OpenEncoding.main(args)
  }
}

// vim: set sw=4 et:
