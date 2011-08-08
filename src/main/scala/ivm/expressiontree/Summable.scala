package ivm.expressiontree

trait Summable[T, S] {
  def plus(a: T, b: T): T
  def plusNode(a: Exp[T], b: Exp[T]): S
}
