package ivm
package tests

import org.scalatest.junit.{ShouldMatchersForJUnit, JUnitSuite}
import org.junit.Test
import ivm.expressiontree._

/**
 * User: pgiarrusso
 * Date: 11/8/2011
 */

class FixpointTest extends JUnitSuite with ShouldMatchersForJUnit {
  @Test def staticAnal() {

  }

  @Test def transClosure() {
    TransitiveClosure(Const(List(1 -> 2, 2 -> 3))).expResult() should be (Set((1,2), (2,3), (1,3)))
  }
}
