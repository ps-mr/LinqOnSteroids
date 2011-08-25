package ivm.expressiontree

case class Var[T](name: String) extends ChildlessExp[T] {
	override def toString() = name
	def interpret() = throw new Exception("interpret on var")
  override def potentiallyEquals[S](other: Exp[S]) = other match {
    case Var(n) => n.equals(name)
    case _ => false
  }}
