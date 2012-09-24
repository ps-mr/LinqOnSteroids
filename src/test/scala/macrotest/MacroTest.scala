package macrotest
import ivm.expressiontree._
import Lifting._
import Macros._
import org.scalatest._
import org.scalatest.matchers.ShouldMatchers

//Test that my macros work even outside of ivm.expressiontree; I know that currently they will instead break.
class MacroTest extends FunSuite with ShouldMatchers {
  test("A macro-generated polymorphically embedded expression should produce an expression tree") {
    println(Macros wrap {
      pure(1) + 2
    } apply Lifting)
  }
}
