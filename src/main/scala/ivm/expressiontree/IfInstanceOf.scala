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
case class IfInstanceOf[T, S](x: Exp[T], classS: Class[_])(implicit val cS: TypeTag[S]) extends Arity1OpExp[T, Option[S], IfInstanceOf[T, S]](x) {
  def copy(x: Exp[T]) = IfInstanceOf(x, classS)
  def interpret() =
    Util.ifInstanceOfBody(x.interpret(), classS)
}
