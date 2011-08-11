package ivm.expressiontree

trait Call[Res] extends Exp[Res] {
  // treats all calls of the same arity as potentially equal
/*  override def potentiallyEquals[S](other: Exp[S]) = other match {
    case other: Call[_] => getClass().equals(other.getClass()) &&
    case _ => false
  }*/

}
