package ivm.expressiontree

object Var {
  def apply(id: Int) = TypedVar[Nothing](id)
  def unapply(x: Var): Option[Int] = TypedVar.unapply(x)
}
//case class Var(override val id: Int) extends TypedVar[Nothing](id)
