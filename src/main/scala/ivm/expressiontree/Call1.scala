package ivm.expressiontree

case class Call1[A0, Res](callfunc: A0 => Res, arg0: Exp[A0]) extends Call[Res] {
  def children = Seq(arg0)
  def genericConstructor = (v) => Call1[A0,Res](callfunc, v(0).asInstanceOf[Exp[A0]])
  def interpret() = callfunc(arg0.interpret())
}
