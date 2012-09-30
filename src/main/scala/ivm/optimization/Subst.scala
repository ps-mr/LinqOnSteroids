package ivm
package optimization

import expressiontree._
import Lifting._

/**
 * User: pgiarrusso
 * Date: 30/6/2012
 */

object Subst {
  def subst[S, T](fun: Fun[S, T])(arg: Exp[S]) = {
    //See TAPL page 75 - we need this transformation whenever we might duplicate terms. XXX check if other
    // transformations duplicate terms. Map fusion uses letExp, to allow for a smarter inliner - I hope I was
    // consistent in doing this.
    fun.f(arg) transform {
      case fun: Fun[_, _] => Fun.rename(fun)
      case e => e
    }
  }
}
