package ivm.expressiontree

class
Call0[Res](id: Symbol, callfunc: () => Res) extends Call[Res](id) {
  def children = Seq()
  def genericConstructor = (_) => this 
  def interpret() = callfunc()
}
