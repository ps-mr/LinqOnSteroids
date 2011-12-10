package ivm.expressiontree

class Call1[A0, Res](val id: Symbol, callfunc: A0 => Res, arg0: Exp[A0]) extends UnaryOpExp[A0, Res](arg0) with Call[Res] {
  def copy(arg0: Exp[A0]) = new Call1(id, callfunc, arg0)
  def interpret() = callfunc(arg0.interpret())
}
