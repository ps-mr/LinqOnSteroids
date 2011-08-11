package ivm.expressiontree

case class LEq[T](x: Exp[T], y: Exp[T])(implicit val ord: Ordering[T]) extends Exp[Boolean] {
  def interpret() = ord.lteq(x.interpret(), y.interpret())
  def children = Seq(x,y)
  def genericConstructor = (v) => LEq(v(0).asInstanceOf[Exp[T]], 
                                       v(1).asInstanceOf[Exp[T]])
}
