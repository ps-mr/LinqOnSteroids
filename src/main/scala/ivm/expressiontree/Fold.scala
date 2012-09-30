package ivm
package expressiontree

import collection.TraversableLike

/*
case class FoldLeft[T, U, Repr <: TraversableLike[T, Repr]](base: Exp[Repr],
                                                            z: Exp[U], f: Fun[(U, T), U])
                                                      extends Arity3Op[Exp[Repr], Exp[U], Fun[(U, T), U], U, FoldLeft[T, U, Repr]](base, z, f) {
  override def interpret() =
    base.interpret().foldLeft(z.interpret())(Function.untupled(f.interpret()))
  override def copy(base: Exp[Repr], z: Exp[U], f: Fun[(U, T), U]) = FoldLeft[T, U, Repr](base, z, f)
  override def toCode = "%s.foldLeft(%s)(Function.untupled(%s))" format (base.toCode, z.toCode, f.toCode)
}
*/

// vim: set ts=8 sw=2 et:
