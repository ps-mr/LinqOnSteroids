package ivm.expressiontree

class Call1[A0, Res](id: Symbol, callfunc: A0 => Res, arg0: Exp[A0]) extends Call[Res](id) {
  def children = Seq(arg0)
  def genericConstructor = (v) => new Call1[A0,Res](id,callfunc, v(0).asInstanceOf[Exp[A0]])
  def interpret() = callfunc(arg0.interpret())
}
