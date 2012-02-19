package ivm.expressiontree

case class Call2[A0, A1, Res](id: Symbol,
                                        callfunc: (A0, A1) => Res,
                                        arg0: Exp[A0],
                                        arg1: Exp[A1]) extends Arity2OpExp[A0, A1, Res, Call2[A0, A1, Res]](arg0, arg1) with Call[Res] {
  def copy(arg0: Exp[A0], arg1: Exp[A1]) = new Call2(id, callfunc, arg0, arg1)
  def interpret() = callfunc(arg0.interpret(), arg1.interpret())
}
