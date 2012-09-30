package ivm.expressiontree

/*
case class Const[T](x: T)(implicit val cTag: ClassTag[T], val tTag: TypeTag[T]) extends Arity0Exp[T] {
  import Const._

  override def interpret() = x

  override def toCode = throw new RuntimeException("Const.toCode should never be called")
  override def toString = Const toString (x, productPrefix)
}
*/

//This class has much faster hashing and comparison; we use it when we can semantically afford it, that is within asSquopt.
class ConstByIdentity[T](content: T, wit: ClassTag[T], wit2: TypeTag[T]) extends Const(content)(wit, wit2) {
  override def canEqual(o: Any) = o.isInstanceOf[ConstByIdentity[_]]

  override def equals(other: Any) = other match {
    case that: ConstByIdentity[_] => (that canEqual this) && (x.asInstanceOf[AnyRef] eq that.x.asInstanceOf[AnyRef])
    case _ => false
  }

  override def hashCode() = System identityHashCode x.asInstanceOf[AnyRef]
  override def productPrefix = "ConstByIdentity"
}

object ConstByIdentity {
  def apply[T: ClassTag: TypeTag](content: T) = new ConstByIdentity[T](content, classTag[T], typeTag[T])
  def apply[T](content: T, cTag: ClassTag[_], tTag: TypeTag[_])(implicit v: DummyImplicit) =
    new ConstByIdentity[T](content, cTag.asInstanceOf[ClassTag[T]], tTag.asInstanceOf[TypeTag[T]])
}
