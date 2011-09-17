package ivm.expressiontree

class Call2[A0, A1, Res](id: String, callfunc: (A0, A1) => Res, arg0: Exp[A0], arg1: Exp[A1]) extends Call[Res](id) {
  def children = Seq(arg0,arg1)
  def genericConstructor = (v) => new Call2(id,callfunc, v(0).asInstanceOf[Exp[A0]], v(1).asInstanceOf[Exp[A1]])
  def interpret() = callfunc(arg0.interpret(), arg1.interpret())
}
