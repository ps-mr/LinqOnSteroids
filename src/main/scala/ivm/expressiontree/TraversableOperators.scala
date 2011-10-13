package ivm.expressiontree

import collection.generic.{CanBuildFrom, FilterMonadic}
import collection.{TraversableViewLike, TraversableView, TraversableLike}

/**
* User: pgiarrusso
* Date: 19/9/2011
*/

// It's amazing that Scala accepts "extends Exp[That]", since it would not accept That; most probably that's thanks to erasure.
case class FlatMap[T, Repr <: FilterMonadic[T, Repr],
                   U, That](base: Exp[Repr], f: FuncExp[T, TraversableOnce[U]])
                            (implicit protected[this] val c: CanBuildFrom[Repr, U, That]) extends BinaryOp[Exp[Repr], FuncExp[T, TraversableOnce[U]], That](base, f) {
  override def interpret = base.interpret flatMap f.interpret()
  override def copy(base: Exp[Repr], f: FuncExp[T, TraversableOnce[U]]) = FlatMap[T, Repr, U, That](base, f)
}

case class MapOp[T, Repr <: FilterMonadic[T, Repr],
                 U, That](base: Exp[Repr], f: FuncExp[T, U])
                          (implicit protected[this] val c: CanBuildFrom[Repr, U, That]) extends BinaryOp[Exp[Repr], FuncExp[T, U], That](base, f) {
  override def interpret = base.interpret map f.interpret()
  override def copy(base: Exp[Repr], f: FuncExp[T, U]) = MapOp[T, Repr, U, That](base, f)
}

case class Filter[T, Repr <: TraversableLike[T,Repr]](base: Exp[Repr],
                                                      f: FuncExp[T, Boolean]) extends BinaryOp[Exp[Repr], FuncExp[T, Boolean], Repr](base, f) {
  override def interpret = base.interpret filter f.interpret()
  override def copy(base: Exp[Repr], f: FuncExp[T, Boolean]) = Filter(base, f)
 }

case class WithFilter[T, Repr <: TraversableLike[T, Repr]](base: Exp[Repr],
                                                      f: FuncExp[T, Boolean])
                                                      extends BinaryOp[Exp[Repr], FuncExp[T, Boolean], TraversableView[T, Repr]](base, f) {
  override def interpret = base.interpret.view filter f.interpret()
  override def copy(base: Exp[Repr], f: FuncExp[T, Boolean]) = WithFilter[T, Repr](base, f)
}

case class View[T, Repr <: TraversableLike[T, Repr]](base: Exp[Repr]) extends UnaryOpExp[Repr, TraversableView[T, Repr]](base) {
  override def interpret = base.interpret.view
  override def copy(base: Exp[Repr]) = View[T, Repr](base)
}

case class Force[T, Repr <: TraversableLike[T, Repr] with Traversable[T],
                 ViewColl <: TraversableViewLike[T, Repr, ViewColl] with TraversableView[T, Repr] with TraversableLike[T, ViewColl], That]
                (base: Exp[ViewColl])(implicit protected[this] val bf: CanBuildFrom[Repr, T, That]) extends UnaryOpExp[ViewColl, That](base) {
  override def interpret = base.interpret.force
  override def copy(base: Exp[ViewColl]) = Force[T, Repr, ViewColl, That](base)
}

case class TypeFilter[T, C[_] <: Traversable[_], S /* is this too strict? <: T */](base: Exp[C[T]])
                                 (implicit cS: ClassManifest[S])
                                  extends UnaryOp[Exp[C[T]], C[S]](base) {
  private[this] val classS = cS.erasure

  override def interpret = base.interpret.filter(classS.isInstance(_)).asInstanceOf[C[S]]
  override def copy(base: Exp[C[T]]) = TypeFilter[T,C,S](base)
}