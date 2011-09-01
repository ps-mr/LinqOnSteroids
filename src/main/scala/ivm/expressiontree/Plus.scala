package ivm.expressiontree

case class Plus[T](x: Exp[T], y: Exp[T])(implicit val isNum: Numeric[T]) extends Exp[T] {
  def interpret() = isNum.plus(x.interpret(), y.interpret())
  def children = Seq(x,y)
  def genericConstructor = (v) => Plus(v(0).asInstanceOf[Exp[T]], 
                                       v(1).asInstanceOf[Exp[T]])
  
}
