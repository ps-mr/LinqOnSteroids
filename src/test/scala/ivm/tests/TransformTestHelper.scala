package ivm
package tests

import org.scalatest.Matchers
import org.scalatest.junit.{JUnitSuite, AssertionsForJUnit}

import expressiontree._

trait TransformTestHelper {
  this: JUnitSuite with Matchers with AssertionsForJUnit =>
  val forceRebuild: Exp[_] => Exp[_] =
    e => e match {
      case Sym(f: Fun[_, _]) => Fun(f.f)
      case _ => e
    }

  def testRebuildP[T](t: Exp[T], transf: Exp[_] => Exp[_]) = {
    val tTransf = t transform transf
    tTransf should equal (t)
    assert(tTransf equals t)
    (t, tTransf)
  }

  def testRebuild[T](t: Exp[T]) = {
    testRebuildP(t, forceRebuild)
    testRebuildP(t, identity)
  }
}
