package ivm
package expressiontree

import collection.TraversableLike

case class Exists[T, Repr <: Traversable[T] with TraversableLike[T, Repr]](base: Exp[Repr with Traversable[T]],
                                                      f: FunSym[T, Boolean]) extends Arity2Op[Exp[Repr], FunSym[T, Boolean], Boolean, Exists[T, Repr]](base, f) with InfixPrinting {
  override def interpret() = base.interpret() exists f.interpret()
  override def copy(base: Exp[Repr], f: FunSym[T, Boolean]) = Exists(base, f)
  def operator = "exists"
}

// vim: set ts=4 sw=4 et:
