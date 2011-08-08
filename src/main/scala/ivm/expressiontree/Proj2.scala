package ivm.expressiontree

case class Proj2[A,B](p: Exp[(A,B)]) extends Exp[B] {
  def interpret() = p.interpret()._2
  def children = Seq(p)
  def genericConstructor = (v) => Proj2(v(0).asInstanceOf[Exp[(A,B)]])
}
