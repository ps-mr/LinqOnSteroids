package ivm.expressiontree

object Const {
  private val maxInlineStringLength = 10
  val allowInlineInEval = true
}

case class Const[T](x: T)(implicit val classManifest: ClassManifest[T]) extends Arity0Exp[T] {
  import Const._

  override def interpret() = x

  //Pretty-printing: we print some constants inline, sometimes (but not always) using toString.

  private def showFP[U](t: U): String = {
    //val typ = classManifest.toString
    //"""java.lang.%s.parse%s("%s")""".format(typ, typ, x.toString)
    CrossStagePersistence.addVar(this)
  }

  private def baseShow: PartialFunction[T, String] = {
    case s: String if s.length < maxInlineStringLength =>
      "\"%s\"" format s
    case c: Char =>
      "'%c'" format c
  }
  import java.{lang => jl}

  private def inlineShow: PartialFunction[T, String] = {
    case x: jl.Integer =>
      x.toString
    case x: Double =>
      showFP(x)
    case x: Float =>
      showFP(x)
    case x: Number =>
      "(%s: %s)".format(x.toString, classManifest)
    case _ =>
      CrossStagePersistence.addVar(this)
  }

  private def show(toEval: Boolean = false): String = {
    val allowInline = allowInlineInEval || !toEval
    /*if (toEval) {
      if (allowInlineInEval) {
        (baseShow orElse inlineShow)(x)
      } else {
        CrossStagePersistence.addVar(this)
      }
    } else {
      (baseShow orElse (String.valueOf((_: Any))).asPartial)(x)
    }*/

    (toEval, x) match {
      //Strings can be always represented directly, but for long strings that's not a good idea, since the string will
      //have to be parsed again.
      case (_, s: String) if allowInline || s.length < maxInlineStringLength =>
        "\"%s\"" format s
      case (_, c: Char) if allowInline =>
        "'%c'" format c
      case (false, x: Number) =>
        x.toString
      //case (true, x: Int) => //Doesn't work.
      case (true, x: jl.Integer) if allowInlineInEval =>
        x.toString
      // More precision for non-integral numbers. We might want to drop this case and use standard CSP for non-integers
      // or at least for floating-point values - where some numbers pretty-print as strings that are not valid literals
      // (such as "Infinity"). Integer numbers can be added by suffixes.
      case (true, x: Double) if allowInlineInEval =>
        showFP(x)
      case (true, x: Float) if allowInlineInEval =>
        showFP(x)
      case (true, x: Number) if allowInlineInEval =>
        "(%s: %s)".format(x.toString, classManifest)
      case (true, _) =>
        CrossStagePersistence.addVar(this)
      case (false, _) =>
        String.valueOf(x)
    }
  }
  /*private def show(toEval: Boolean = false): String = {
    val str = x match {
      case s: String =>
        """"%s"""" format s
      case x: Number =>
        x.toString
      case _ =>
        String.valueOf(x)
    }
    if (toEval) {
      CrossStagePersistence.addVar(this)
    } else
      str
  }*/

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
