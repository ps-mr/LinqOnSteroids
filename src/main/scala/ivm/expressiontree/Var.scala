package ivm.expressiontree

case class Var(name: String) extends NullaryExp[Nothing] {
	override def toString() = name
	def interpret() = throw new Exception("interpret on var")
  override def potentiallyEquals[S](other: Exp[S]) = other match {
    case Var(n) => n.equals(name)
    case _ => false
  }}
