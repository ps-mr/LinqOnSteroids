package ivm.expressiontree

object IfInstanceOf {
  /*
   * This is required to get the expected behavior also for primitive types. Class.isInstance(Object) documents that
   * "If this Class object represents a primitive type, this method returns false.", which makes sense since an Object
   * can never be an instance of a primitive type.
   */
  def apply[T, S](x: Exp[T], cS: reflect.ClassTag[S]): IfInstanceOf[T, S] = apply[T, S](x, ClassUtil.boxedErasure(cS))
}

//Equality comparison must consider also classS. Therefore, classS must be a class parameter.
case class IfInstanceOf[T, S](x: Exp[T], classS: Class[_]) extends Arity1OpExp[T, Option[S], IfInstanceOf[T, S]](x) {
  def copy(x: Exp[T]) = IfInstanceOf(x, classS)
  def interpret() =
    Util.ifInstanceOfBody(x.interpret(), classS)
}
