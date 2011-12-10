package ivm.expressiontree

class Call5[A0, A1, A2, A3, A4, Res](val id: Symbol,
                                                    callfunc: (A0, A1, A2, A3, A4) => Res,
                                                    arg0: Exp[A0],
                                                    arg1: Exp[A1],
                                                    arg2: Exp[A2],
                                                    arg3: Exp[A3],
                                                    arg4: Exp[A4]) extends QuinaryOpExp[A0, A1, A2, A3, A4, Res](arg0, arg1, arg2, arg3, arg4) with Call[Res] {
  def copy(arg0: Exp[A0], arg1: Exp[A1], arg2: Exp[A2], arg3: Exp[A3], arg4: Exp[A4]) = new Call5(id, callfunc, arg0, arg1, arg2, arg3, arg4)
  def interpret() = callfunc(arg0.interpret(), arg1.interpret(), arg2.interpret(), arg3.interpret(), arg4.interpret())
}
