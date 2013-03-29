package ivm
package optimization

import scala.language.postfixOps

import expressiontree._
import Lifting._

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
//import tests.TestUtil

//Try to have an assertFusable annotation
class ForceFusion extends FunSuite with ShouldMatchers {
  val testQuery = Set(1).asSquopt map (_+1) map (_+2)

  //Trivial case
  def detectNonFusion[T](e: Exp[T]): Boolean =
    e __find {
      case Sym(MapNode(Sym(MapNode(coll, f1)), f2)) =>
        println(s"${coll} ${f1} ${f2}")
        true
    } nonEmpty

  test("detectNonFusion") {
    assert(detectNonFusion(testQuery))
    assert(!detectNonFusion(testQuery optimize))
  }

  test("assertFusable") {
    //The API
    def assertFusable[T](e: Exp[T]) = {
      val e_ = e.optimize
      assert(!detectNonFusion(e_))
      e_
    }
    //Example using the API:
    assertFusable(testQuery)
  }

  //OK, but what should this mean in general?
  //1. We can't expect to completely fuse a realistic query.
  //2. What happens when we have operators different from Map?

  //Here's a solution
  case class Marker[T](e: Exp[T]) extends Arity1OpExp[T, T, Marker[T]](e) {
    def copy(e: Exp[T]) = Marker(e)
    def interpret() = e.interpret()
  }
  //Could also use delimited continuations, but that's clearly overkill.
  def assertFusableFrom[T](e: Exp[T]): Exp[T] = Marker(e)
  def assertFusableTo[T](e: Exp[T]): Exp[T] = {
    val e_ = e.optimize
    val res: List[Exp[_]] = e_.children
    println(res)
    assert(res flatMap (_ __find {
      case Sym(Marker(_)) => true
    }) nonEmpty)
    import TransformationCombinators._

    e_ transform (fromPoly(Transformer {
      case Sym(Marker(t)) => t
    } | emptyTransform))
  }

  test("fromTo") {
    val testQuery2 = assertFusableTo(assertFusableFrom(Set(1) asSquopt) map (_ + 1) map (_ + 2))
  }
  //I mailed Yvonne Coady about the general goal.
}
