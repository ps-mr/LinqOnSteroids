package ivm.expressiontree

case class Call0[Res](id: Symbol, callfunc: () => Res) extends NullaryExp[Res] with Call[Res] {
  def interpret() = callfunc()
}
