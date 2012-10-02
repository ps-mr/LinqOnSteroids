package squopt

import ivm._
import expressiontree.{LiftingTrait}
import optimization.Optimization

/*package object squopt*/
object imports extends LiftingTrait {
  type Exp[+T] = expressiontree.Exp[T]
  def Fun[S, T](f: Exp[S] => Exp[T]): Exp[S => T] = expressiontree.BaseLangImpl.toFunSym(expressiontree.Fun[S, T](f))
}

// vim: set ts=4 sw=4 et:
