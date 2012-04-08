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
        case s: String =>
          """"%s"""" format s
        case _ =>
          x.toString
      }
    val shortened =
      if (s.length() > 100) {
        val begin = s take 100
        begin + "..." + ")" * (begin.count('(' == ) - begin.count(')' == ))
      }
      else
        s
    "Const(" + shortened + ")"
  }
}

//This class has much faster hashing and comparison; we use it when we can semantically afford it, that is within asSmart.
class ConstByIdentity[T](content: T) extends Const(content) {
  override def canEqual(o: Any) = o.isInstanceOf[ConstByIdentity[_]]

  override def equals(other: Any) = other match {
    case that: ConstByIdentity[_] => (that canEqual this) && (x.asInstanceOf[AnyRef] eq that.x.asInstanceOf[AnyRef])
    case _ => false
  }

  override def hashCode() = System.identityHashCode(x.asInstanceOf[AnyRef])
}
