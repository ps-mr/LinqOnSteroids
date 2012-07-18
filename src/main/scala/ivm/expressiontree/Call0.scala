package ivm.expressiontree

case class Call0[Res](name: Symbol, restId: Symbol, callfunc: () => Res) extends Arity0Exp[Res] with Call[Res] {
  def interpret() = callfunc()
}
