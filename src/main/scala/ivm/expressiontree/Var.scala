package ivm.expressiontree

case class Var(id: Int) extends NullaryExp[Nothing] {
  def name = "v" + id
	override def toString() = name
	def interpret() = throw new Exception("interpret on var")
}
