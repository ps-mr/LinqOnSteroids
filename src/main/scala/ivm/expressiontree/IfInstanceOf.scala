package ivm.expressiontree

object IfInstanceOf {
  /*
   * This is required to get the expected behavior also for primitive types. Class.isInstance(Object) documents that
   * "If this Class object represents a primitive type, this method returns false.", which makes sense since an Object
   * can never be an instance of a primitive type.
   */
  def apply[T, S: ClassTag: TypeTag](x: Exp[T]): IfInstanceOf[T, S] = apply[T, S](x, ClassUtil.boxedErasure(classTag[S]))
}

//Equality comparison must consider also classS. Therefore, classS must be a class parameter.
case class IfInstanceOf[T, S](x: Exp[T], classS: Class[_])(implicit val tS: TypeTag[S]) extends Arity1OpExp[T, Option[S], IfInstanceOf[T, S]](x) with PersistClassS {
  def copy(x: Exp[T]) = IfInstanceOf(x, classS)
  def interpret() =
    Util.ifInstanceOfBody(x.interpret(), classS)
  //ifInstanceOfBody has type parameters which by default are deduced to be Nothing - and has _two_ of them. To avoid needing a TypeTag for T, use ascription on the result.
  override def toCode = "ivm.expressiontree.Util.ifInstanceOfBody(%s, %s): Option[%s]" format (x.toCode, persistedValue, Compile manifestToString tS)
}

case class IsInstanceOf[T, S](x: Exp[T])(implicit val cS: ClassTag[S], val tS: TypeTag[S]) extends Arity1OpExp[T, Boolean, IsInstanceOf[T, S]](x) {
  def copy(x: Exp[T]) = IsInstanceOf(x)
  def interpret() = {
    val res = x.interpret()
    res != null && (cS.runtimeClass isInstance res)
  }
  override def toCode = "%s.isInstanceOf[%s]" format (x.toCode, Compile manifestToString tS)
}

case class AsInstanceOf[T, S](x: Exp[T])(implicit val cS: ClassTag[S], val tS: TypeTag[S]) extends Arity1OpExp[T, S, AsInstanceOf[T, S]](x) {
  def copy(x: Exp[T]) = AsInstanceOf(x)
  def interpret() = (cS.runtimeClass cast x.interpret()).asInstanceOf[S]
  override def toCode = "%s.asInstanceOf[%s]" format (x.toCode, Compile manifestToString tS)
}
