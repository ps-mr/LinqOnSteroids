package ivm.expressiontree

class Call5[A0, A1, A2, A3, A4, Res](id: String, callfunc: (A0, A1, A2, A3, A4) => Res, arg0: Exp[A0], arg1: Exp[A1], arg2: Exp[A2], arg3: Exp[A3], arg4: Exp[A4]) extends Call[Res](id) {
  def children = Seq(arg0,arg1,arg2,arg3,arg4)
  def genericConstructor = (v) => new Call5(id,callfunc, v(0).asInstanceOf[Exp[A0]], v(1).asInstanceOf[Exp[A1]],  v(2).asInstanceOf[Exp[A2]],  v(3).asInstanceOf[Exp[A3]], v(4).asInstanceOf[Exp[A4]])
  def interpret() = callfunc(arg0.interpret(), arg1.interpret(), arg2.interpret(), arg3.interpret(), arg4.interpret())
}
