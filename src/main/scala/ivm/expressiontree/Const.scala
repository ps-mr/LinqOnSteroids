package ivm.expressiontree

trait ConstBase[T] extends Exp[T] {
  def x: T
  def children = Seq()
  def genericConstructor = (_) => this
  def interpret() = x
}

case class Const[T](x: T) extends ConstBase[T] {
  override def potentiallyEquals[S](other: Exp[S]) = other match {
    case Const(y) => y.equals(x)
    case _ => false
  }
  override def toString() = x.toString()
}
