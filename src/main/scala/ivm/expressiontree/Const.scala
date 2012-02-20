package ivm.expressiontree

case class Const[T](x: T) extends Arity0Exp[T] {
  override def interpret() = x
  override def toString = {
    val s =
    x match {
      //Printing all elements and then cutting the output is horribly expensive for huge collections, so try to avoid it.
      //Of course, this does not work when x is not a collection but e.g. contains one, or when for any reason toString()
      //takes a lot of time for any reason. Still, better than nothing.
      case coll: Traversable[_] =>
        coll.take(3).toString() + (if (coll.size > 3) "..." else "")
      case _ =>
        x.toString
    }
    "Const(" + (if (s.length() > 100) s.take(100)+"..." else s) + ")"
  }
}
