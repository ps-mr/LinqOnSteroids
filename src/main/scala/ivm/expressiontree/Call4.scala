package ivm.expressiontree

class Call4[A0, A1, A2, A3, Res: ClassManifest](val id: Symbol,
                                                callfunc: (A0, A1, A2, A3) => Res,
                                                arg0: Exp[A0],
                                                arg1: Exp[A1],
                                                arg2: Exp[A2],
                                                arg3: Exp[A3]) extends QuaternaryOpExp[A0, A1, A2, A3, Res](arg0, arg1, arg2, arg3) with Call[Res] {
  def copy(arg0: Exp[A0], arg1: Exp[A1], arg2: Exp[A2], arg3: Exp[A3]) = new Call4(id, callfunc, arg0, arg1, arg2, arg3)
  def interpret() = callfunc(arg0.interpret(), arg1.interpret(), arg2.interpret(), arg3.interpret())
}
