package ivm.expressiontree

/**
 * User: pgiarrusso
 * Date: 11/12/2011
 */

case class Let[T](v: Exp[T]) {
  def map[U](f: Exp[T] => Exp[U]) = f(v)
  def flatMap[U](f: Exp[T] => Exp[Traversable[U]]) = map(f)
  import Lifting._
  def withFilter(f: Exp[T] => Exp[Boolean]): Exp[Option[T]] = onExp(f(v), v)('trueFalseV,
    (res, interpV) => if (res) Some(interpV) else None
  )
}
