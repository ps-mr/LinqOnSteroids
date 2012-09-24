package ivm.expressiontree
import org.scalatest._
import org.scalatest.matchers.ShouldMatchers

/*
trait MyIntf extends Interface with ScalaIntf {
  type LangIntf <: BaseLangIntf with ScalaLangIntf
}
*/

class ModularFrontendExample extends FunSuite with ShouldMatchers {
  val expected = Plus(Const(1), Const(2))
  test("A manually written polymorphically embedded expression should produce an expression tree") {
    new Interpreted[BaseLangIntf with ScalaLangIntf, Int] {
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
  //But this isn't:
  /*
  println(Macros.wrap {
    pure(1).interpret() + 2
  })
  */
}

// vim: set ts=8 sw=2 et:
