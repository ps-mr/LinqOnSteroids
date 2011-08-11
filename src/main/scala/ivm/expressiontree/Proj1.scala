package ivm.expressiontree

case class Proj1[A,B](p: Exp[(A,B)]) extends Exp[A] {
  def interpret() = p.interpret()._1
  def children = Seq(p)
  def genericConstructor = (v) => Proj1(v(0).asInstanceOf[Exp[(A,B)]])
}
