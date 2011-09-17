package ivm.expressiontree

case class Const[T](x: T) extends NullaryExp[T] {
  override def interpret() = x
  override def toString() = x.toString()
}
