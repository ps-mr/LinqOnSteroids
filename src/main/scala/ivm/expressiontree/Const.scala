package ivm.expressiontree

object Const {
  private val maxInlineStringLength = 10
  val allowInlineInEval = false
}

case class Const[T](x: T)(implicit val cTag: ClassTag[T], val tTag: TypeTag[T]) extends Arity0Exp[T] {
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
      "(%s: %s)".format(x.toString, cTag)
    case _ =>
      CrossStagePersistence.addVar(this)
  }

  private def show(toEval: Boolean = false): String = {
    val allowInline = allowInlineInEval || !toEval
    if (toEval) {
      if (allowInlineInEval) {
        (baseShow orElse inlineShow)(x)
      } else {
        CrossStagePersistence.addVar(this)
      }
    } else {
      (baseShow orElse (String.valueOf((_: Any))).asPartial)(x)
    }
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
class ConstByIdentity[T](content: T, wit: ClassTag[T], wit2: TypeTag[T]) extends Const(content)(wit, wit2) {
  override def canEqual(o: Any) = o.isInstanceOf[ConstByIdentity[_]]

  override def equals(other: Any) = other match {
    case that: ConstByIdentity[_] => (that canEqual this) && (x.asInstanceOf[AnyRef] eq that.x.asInstanceOf[AnyRef])
    case _ => false
  }

  override def hashCode() = System.identityHashCode(x.asInstanceOf[AnyRef])
  override def productPrefix = "ConstByIdentity"
}

object ConstByIdentity {
  //implicit val cTag: ClassTag[T], tTag: TypeTag[T]
  def apply[T: ClassTag: TypeTag](content: T) = new ConstByIdentity[T](content, classTag[T], typeTag[T])
  def apply[T](content: T, cTag: ClassTag[_], tTag: TypeTag[_])(implicit v: DummyImplicit) =
    new ConstByIdentity[T](content, cTag.asInstanceOf[ClassTag[T]], tTag.asInstanceOf[TypeTag[T]])
}
