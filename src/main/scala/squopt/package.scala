package squopt

import ivm._
import expressiontree.{Compile, LiftingTrait, TypeTag}
import optimization.Optimization

/*package object squopt*/
object imports extends LiftingTrait {
  type Exp[+T] = expressiontree.Exp[T]
  def Fun[S, T](f: Exp[S] => Exp[T]) = expressiontree.Fun[S, T](f)
}

// vim: set ts=4 sw=4 et:
