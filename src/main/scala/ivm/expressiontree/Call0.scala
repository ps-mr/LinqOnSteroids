package ivm.expressiontree

class Call0[Res](val id: Symbol, callfunc: () => Res) extends Arity0Exp[Res] with Call[Res] {
  def interpret() = callfunc()
}
