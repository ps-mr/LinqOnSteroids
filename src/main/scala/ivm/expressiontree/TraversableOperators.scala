package ivm.expressiontree

import collection.generic.{CanBuildFrom, FilterMonadic}
import collection.{TraversableViewLike, TraversableView, TraversableLike, GenTraversableOnce}

/**
* User: pgiarrusso
* Date: 19/9/2011
*/

// It's amazing that Scala accepts "extends Exp[That]", since it would not accept That; most probably that's thanks to erasure.
case class FlatMap[T, Repr <: FilterMonadic[T, Repr],
                   This <: FilterMonadic[T, Repr],
                   U, That](base: Exp[This], f: FuncExp[T, GenTraversableOnce[U]])
                            (implicit c: CanBuildFrom[Repr, U, That]) extends BinaryOp[Exp[This], FuncExp[T, GenTraversableOnce[U]], That](base, f) {
  override def interpret = base.interpret flatMap f.interpret()
  override def copy(base: Exp[This], f: FuncExp[T, GenTraversableOnce[U]]) = FlatMap[T, Repr, This, U, That](base, f)
}

case class MapOp[T, Repr <: FilterMonadic[T, Repr],
                 This <: FilterMonadic[T, Repr],
                 U, That](base: Exp[This], f: FuncExp[T, U])
                          (implicit c: CanBuildFrom[Repr, U, That]) extends BinaryOp[Exp[This], FuncExp[T, U], That](base, f) {
  override def interpret = base.interpret map f.interpret()
  override def copy(base: Exp[This], f: FuncExp[T, U]) = MapOp[T, Repr, This, U, That](base, f)
}

case class WithFilter[T, Repr <: FilterMonadic[T, Repr],
                      This <: FilterMonadic[T, Repr]](base: Exp[This],
                                                      f: FuncExp[T, Boolean])
                                                      extends BinaryOp[Exp[This], FuncExp[T, Boolean], FilterMonadic[T, Repr]](base, f) {
  override def interpret = base.interpret withFilter f.interpret()
  override def copy(base: Exp[This], f: FuncExp[T, Boolean]) = WithFilter[T, Repr, This](base, f)
}

case class View[T, Repr <: TraversableLike[T, Repr]](base: Exp[Repr]) extends UnaryOpExp[Repr, TraversableView[T, Repr]](base) {
  override def interpret = base.interpret.view
  override def copy(base: Exp[Repr]) = View[T, Repr](base)
}

case class Force[T, Repr <: TraversableLike[T, Repr] with Traversable[T],
                 ViewColl <: Repr with TraversableViewLike[T, Repr, ViewColl] with TraversableView[T, Repr] with TraversableLike[T, ViewColl]]
                (base: Exp[ViewColl]) extends UnaryOpExp[ViewColl, Traversable[T]](base) {
  override def interpret = base.interpret.force
  override def copy(base: Exp[ViewColl]) = Force[T, Repr, ViewColl](base)
}
