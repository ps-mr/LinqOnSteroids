package ivm.expressiontree

case class Call2[A0, A1, Res](callfunc: (A0, A1) => Res, arg0: Exp[A0], arg1: Exp[A1]) extends Call[Res] {
  def children = Seq(arg0,arg1)
  def genericConstructor = (v) => Call2(callfunc, v(0).asInstanceOf[Exp[A0]], v(1).asInstanceOf[Exp[A1]])
  def interpret() = callfunc(arg0.interpret(), arg1.interpret())
}
