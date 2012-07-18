package ivm.expressiontree

/**
 * User: pgiarrusso
 * Date: 18/7/2012
 */

case class NamedVar[+T](name: String) extends Arity0Exp[T] {
  override def toString = name
  override def toCode = name
  def interpret(): T =
    throw new ExoticTermException("interpret on NamedVar")
}

