package ivm.expressiontree

case class Pair[A, B](a: Exp[A], b: Exp[B])(implicit cm: ClassManifest[_ <: (A, B)]) extends BinaryOpExp[A, B, (A, B)](a, b) {
  def copy(a: Exp[A], b: Exp[B]) = Pair(a, b)
  def interpret() = (a.interpret(), b.interpret())
}

