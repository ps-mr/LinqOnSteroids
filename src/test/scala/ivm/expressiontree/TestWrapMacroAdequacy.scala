package ivm.expressiontree

import language.reflectiveCalls
import org.scalatest._
import org.scalatest.matchers.ShouldMatchers

class TestWrapMacroAdequacy extends FunSuite with ShouldMatchers {
  val expected = Sym(Plus(Const(1), Const(2)))

  //Convert some compile-time failures into run-time ones.
  //Tillmann took this idea from a blog post to me :-)
  implicit def toInterpret[T](x: BaseLangIntf#Rep[T]) = new {
    def interpret(): T = throw new NotImplementedError()
  }

  test("A manually written polymorphically embedded expression should produce an expression tree") {
    new Interpreted[LiftingConvsLangIntf with NumOpsLangIntf, Int] {
      def apply(s: ThisLangIntf): s.Rep[Int] = {
        import s._
        pure(1) + 2
      }
    } apply Lifting should be (expected)
  }

  import Lifting._
  test("A macro-generated polymorphically embedded expression should produce an expression tree") {
    //This macro produces the same code as above, after expansion. What is important is that this macro guarantees adequacy.
    println(Macros wrap {
      pure(1) + 2
    } apply Lifting)
  }
  //This is accepted, although it is an exotic term:
  test("An exotic term produces another tree") {
    pure(pure(1).interpret() + 2) should be (Const(3))
  }

  test("wrap rejects exotic terms") {
    //But this isn't:
    evaluating { Macros.wrap {
      pure(1).interpret() + 2
    } apply Lifting} should produce [NotImplementedError]
  }
}

// vim: set ts=8 sw=2 et:
