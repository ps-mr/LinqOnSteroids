package ivm.expressiontree

class Call0[Res: ClassManifest](val id: Symbol, callfunc: () => Res) extends NullaryExp[Res] with Call[Res] {
  def interpret() = callfunc()
}
