package ivm.expressiontree

case class Const[T](x: T) extends ChildlessExp[T] {
  override def interpret() = x
  override def potentiallyEquals[S](other: Exp[S]) = other match {
    case Const(y) => y.equals(x)
    case _ => false
  }
  override def toString() = x.toString()
}
