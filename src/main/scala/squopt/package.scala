package squopt

import ivm._
import expressiontree.{Compile, LiftingTrait, TypeTag}
import optimization.Optimization

/*package object squopt*/
object imports extends LiftingTrait {
  type Exp[+T] = expressiontree.Exp[T]
  def Fun[S, T](f: Exp[S] => Exp[T]) = expressiontree.Fun[S, T](f)

  implicit def withOptimize[T](t: Exp[T]) = new WithOptimize(t)
  class WithOptimize[T](t: Exp[T]) {
    def optimize = Optimization.optimize(t)
  }

  implicit def withEval[T: TypeTag](t: Exp[T]) = new WithEval(t)
  class WithEval[T: TypeTag](t: Exp[T]) {
    def eval = Compile toValue t
  }
}

// vim: set ts=4 sw=4 et:
