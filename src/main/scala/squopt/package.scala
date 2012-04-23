import ivm._
import expressiontree.LiftingTrait

package object squopt extends LiftingTrait {
  type Exp[+T] = expressiontree.Exp[T]
  def Fun[S, T](f: Exp[S] => Exp[T]) = expressiontree.Fun[S, T](f)
}

// vim: set ts=4 sw=4 et:
