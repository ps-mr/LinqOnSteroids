package ivm.expressiontree

trait QueryOp[T] extends QueryReifier[T] {
  override def interpret() = this
}
