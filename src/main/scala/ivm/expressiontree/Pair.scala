package ivm.expressiontree

case class Pair[A, B](a: Exp[A], b: Exp[B]) extends Exp[(A, B)] {
  def interpret() = (a.interpret(), b.interpret())
  def children = Seq(a,b)
  def genericConstructor = (v) => Pair(v(0).asInstanceOf[Exp[A]], 
                                       v(1).asInstanceOf[Exp[B]])
}

