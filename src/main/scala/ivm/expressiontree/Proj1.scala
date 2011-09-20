package ivm.expressiontree

case class Proj1[A: ClassManifest, B](p: Exp[(A, B)]) extends UnaryOpExp[(A, B), A](p) {
  def interpret() = p.interpret()._1
  def copy(p: Exp[(A, B)]) = Proj1(p)
}
