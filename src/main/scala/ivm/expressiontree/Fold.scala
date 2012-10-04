package ivm
package expressiontree

import collection.TraversableLike

case class FoldLeft[T, U, Repr <: TraversableLike[T, Repr]](base: Exp[Repr],
                                                            z: Exp[U], f: FunSym[(U, T), U])
                                                      extends Arity3Op[Exp[Repr], Exp[U], FunSym[(U, T), U], U, FoldLeft[T, U, Repr]](base, z, f) {
  override def interpret() =
    base.interpret().foldLeft(z.interpret())(Function.untupled(f.interpret()))
  override def copy(base: Exp[Repr], z: Exp[U], f: FunSym[(U, T), U]) = FoldLeft[T, U, Repr](base, z, f)
  override def toCode = "%s.foldLeft(%s)(Function.untupled(%s))" format (base.toCode, z.toCode, f.toCode)
}

case class OptionFold[T, U](base: Exp[Option[T]], ifEmpty: Exp[U], f: FunSym[T, U])
  extends Arity3Op[Exp[Option[T]], Exp[U], FunSym[T, U], U, OptionFold[T, U]](base, ifEmpty, f) {
  override def interpret() = base.interpret().fold(ifEmpty.interpret())(f.interpret())
  override def copy(base: Exp[Option[T]], z: Exp[U], f: FunSym[T, U]) = OptionFold[T, U](base, z, f)
  override def toCode = "%s.fold(%s)(%s)" format (base.toCode, ifEmpty.toCode, f.toCode)
}
// vim: set ts=8 sw=2 et:
