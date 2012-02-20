package ivm.expressiontree

/**
 * User: pgiarrusso
 * Date: 16/11/2011
 */

case class ExoticTermException(msg: String) extends IllegalArgumentException(msg)

case class TypedVar[+T](id: Int) extends Arity0Exp[T] {
  def name = "v" + id
  override def toString() = name
  def interpret(): T = {
    FuncExpInt.env.get().get(id) match {
      case Some(v) => v.asInstanceOf[T]
      case None => throw new ExoticTermException("interpret on Var")
    }
  }
}
