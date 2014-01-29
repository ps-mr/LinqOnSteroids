package ivm
package tests

import org.scalatest.Matchers
import org.scalatest.junit.{JUnitSuite, AssertionsForJUnit}
import org.junit.Test
import ivm.expressiontree._

/**
 * User: pgiarrusso
 * Date: 11/8/2011
 */

class FixpointTest extends JUnitSuite with Matchers with AssertionsForJUnit {
  @Test def staticAnal() {

  }

  @Test def transClosure() {
    TransitiveClosure(Const(List(1 -> 2, 2 -> 3))).value() should be (Set((1,2), (2,3), (1,3)))
  }
}
