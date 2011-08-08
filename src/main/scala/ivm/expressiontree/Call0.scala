package ivm.expressiontree

case class Call0[Res](callfunc: () => Res) extends Call[Res] {
  def children = Seq()
  def genericConstructor = (_) => this 
  def interpret() = callfunc()
}
