package ivm.expressiontree


case class MapLookup[S,T](m: Exp[Map[S,T]], key: Exp[S]) extends Exp[T] {
  override def children = Seq(m,key)
  override def genericConstructor = (v) => MapLookup(v(0).asInstanceOf[Exp[Map[S,T]]], v(1).asInstanceOf[Exp[S]])
  override def interpret() = m.interpret()(key.interpret())

}