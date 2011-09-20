package ivm.expressiontree

case class Proj2[A, B: ClassManifest](p: Exp[(A, B)]) extends UnaryOpExp[(A, B), B](p) {
  def interpret() = p.interpret()._2
  def copy(p: Exp[(A, B)]) = Proj2(p)
}
