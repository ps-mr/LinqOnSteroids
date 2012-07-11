package ivm.expressiontree

case class Const[T](x: T)(implicit val classManifest: ClassManifest[T]) extends Arity0Exp[T] {
  override def interpret() = x
  private def show(toEval: Boolean = false): String = {
    val str = x match { //XXX: Doesn't work for most values. Barely good for testing.
      case s: String =>
        """"%s"""" format s
      case x: Number =>
        if (toEval)
          "(" + x.toString + ")"
        else
          x.toString
      case _ =>
        String.valueOf(x)
    }
    if (toEval)
      "(%s: %s)" format (str, classManifest)
    else
      str
  }

  override def toCode = show(toEval = true)
  override def toString = {
    val s =
      x match {
        //Printing all elements and then cutting the output is horribly expensive for huge collections, so try to avoid it.
        //Of course, this does not work when x is not a collection but e.g. contains one, or when for any reason toString()
        //takes a lot of time for any reason. Still, better than nothing.
        case coll: Traversable[_] =>
          coll.take(3).toString() + (if (coll.size > 3) "..." else "")
        case _ =>
          show()
      }
    val shortened =
      if (s.length() > 100) {
        val begin = s take 100
        val quoteCloser = if (begin.contains('"')) "\"" else if (begin.contains('\'')) "'" else ""
        begin + "..." + quoteCloser + ")" * (begin.count('(' == ) - begin.count(')' == ))
      }
      else
        s
    productPrefix + "(" + shortened + ")"
  }
}

//This class has much faster hashing and comparison; we use it when we can semantically afford it, that is within asSmart.
class ConstByIdentity[T](content: T, wit: ClassManifest[T]) extends Const(content)(wit) {
  override def canEqual(o: Any) = o.isInstanceOf[ConstByIdentity[_]]

  override def equals(other: Any) = other match {
    case that: ConstByIdentity[_] => (that canEqual this) && (x.asInstanceOf[AnyRef] eq that.x.asInstanceOf[AnyRef])
    case _ => false
  }

  override def hashCode() = System.identityHashCode(x.asInstanceOf[AnyRef])
  override def productPrefix = "ConstByIdentity"
}

object ConstByIdentity {
  def apply[T: ClassManifest](content: T) = new ConstByIdentity[T](content, classManifest[T])
}
