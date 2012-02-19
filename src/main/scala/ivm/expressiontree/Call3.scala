package ivm.expressiontree

class Call3[A0, A1, A2, Res](val id: Symbol,
                                        callfunc: (A0, A1, A2) => Res,
                                        arg0: Exp[A0],
                                        arg1: Exp[A1],
                                        arg2: Exp[A2]) extends TernaryOpExp[A0, A1, A2, Res, Call3[A0, A1, A2, Res]](arg0, arg1, arg2) with Call[Res] {
  def copy(arg0: Exp[A0], arg1: Exp[A1], arg2: Exp[A2]) = new Call3(id, callfunc, arg0, arg1, arg2)
  def interpret() = callfunc(arg0.interpret(), arg1.interpret(), arg2.interpret())
}
