package ivm.expressiontree

case class Plus[T](x: Exp[T], y: Exp[T])(implicit val sum: Summable[T, Plus[T]]) extends Exp[T] {
  def interpret() = sum.plus(x.interpret(), y.interpret())
  def children = Seq(x,y)
  def genericConstructor = (v) => Plus(v(0).asInstanceOf[Exp[T]], 
                                       v(1).asInstanceOf[Exp[T]])
  
}
