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
    def map[U, That <: Exp[U]](f: T => U)(implicit c: CanBuildExp[U, That]): That =
      c buildExp Exp.app(f, this)
    //TODO add flatMap and withFilter.
  }
  object Exp {
    def app[T, U](f: T => U, t: Exp[T]): Exp[U] = App(f, t)
  }

  //A few nodes for expression trees.
  case class Const[T](t: T) extends Exp[T] {
    def interpret = t
  }

  case class App[T, U](f: T => U, t: Exp[T]) extends Exp[U] {
    def interpret = f(t.interpret)
  }
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

  class ExpWrapper[+T](t: Exp[T]) extends Exp[T] {
    def interpret = t.interpret
  }
  //trait NumExp[T](implicit isNum: Numeric[T]) extends Exp[T]
  trait NumExp[T] extends Exp[T] {
    //If this were not a mixin, this would be an implicit parameter:
    def isNum: Numeric[T]
    def +(that: NumExp[T]): NumExp[T] = Plus(this, that)(isNum)
  }

  case class NumExpWrap[T](t: Exp[T])(implicit val isNum: Numeric[T]) extends
  ExpWrapper[T](t) with NumExp[T]

  //Root node for all binary, associative and commutative operations. The
  //intuition is that many operations (including optimizations) might apply
  //for all of those - e.g. expression normalization.
  abstract class CommutativeOp[T](a: NumExp[T], b: NumExp[T]) extends NumExp[T]

  case class Plus[T](a: NumExp[T], b: NumExp[T])(implicit val isNum: Numeric[T]) extends CommutativeOp(a, b) {
    def interpret =
      isNum.plus(a.interpret, b.interpret)
  }

  //The other problem is constructing the right object, but I think that a
  //trick like CanBuildFrom should work.
  //Note: To is supposed to be a subclass of Exp[Elem], but the following
  //declaration does not compile because of variance problems - I should make
  //Elem invariant, which is not what I want:
  //trait CanBuildExp[-Elem, +To <: Exp[Elem]]
  trait CanBuildExp[-Elem, +To] {
    def buildExp(e: Exp[Elem]): To
  }
  trait OpenEncodingBase {

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
  }

  object OpenEncoding extends OpenEncodingBase {
    //Overload resolution, here, can select the most specific implicit to pass
    //to toExpTempl. The difference between these two methods is thus in the
    //implicitly passed parameter (canBuildExpNum vs canBuildExp)
    implicit def toNumExp[T : Numeric](t: T): NumExp[T] = toExpTempl(t)
    //Question: does implicit lookup find a most-specific solution just because
    //the target type is NumExp[T], and only passing canBuildExpNum gives that
    //type? toNumExp2 shows that this is not the case.
    def _toNumExp2[T : Numeric](t: T): Exp[T] = toExpTempl(t)

    def main(args: Array[String]) {
      val a: NumExp[Int] = 1
      val b = a + 2
      val c = Exp.app((x: Int) => x + 1, 1)
      //Does not work! Implicit conversion does not choose the most-specific one.
      //However, it does prefer to choose implicits defined in a subclass to implicits in a superclass.
      val d = Exp.app((x: Int) => x + 1, 1) //This works.
      println(b)
    }
  }
}

// vim: set sw=4 et:
