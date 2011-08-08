package ivm.expressiontree

abstract class QueryOp[T] extends QueryReifier[T] with Exp[QueryReifier[T]] {
  override def interpret() = this
}
