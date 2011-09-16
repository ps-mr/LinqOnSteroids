package ivm.expressiontree


trait MapReifier[S,T] extends Exp[MapReifier[S,T]] {
  override def interpret() = this
}