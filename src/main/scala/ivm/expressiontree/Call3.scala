package ivm.expressiontree

class Call3[A0, A1, A2, Res](id: String, callfunc: (A0, A1, A2) => Res, arg0: Exp[A0], arg1: Exp[A1], arg2: Exp[A2]) extends Call[Res](id) {
  def children = Seq(arg0,arg1,arg2)
  def genericConstructor = (v) => new Call3(id,callfunc, v(0).asInstanceOf[Exp[A0]], v(1).asInstanceOf[Exp[A1]],  v(2).asInstanceOf[Exp[A2]])
  def interpret() = callfunc(arg0.interpret(), arg1.interpret(), arg2.interpret())
}
