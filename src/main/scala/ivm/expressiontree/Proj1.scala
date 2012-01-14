package ivm.expressiontree

case class Proj1[A, B](p: Exp[(A, B)]) extends UnaryOpExp[(A, B), A, Proj1[A, B]](p) {
  def interpret() = p.interpret()._1
  def copy(p: Exp[(A, B)]) = Proj1(p)
}
