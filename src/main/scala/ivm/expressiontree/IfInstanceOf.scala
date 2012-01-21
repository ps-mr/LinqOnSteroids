package ivm.expressiontree

object IfInstanceOf {
  import java.{lang => jl}
  val primitiveToWrapper = Map[Class[_], Class[_]](
    classOf[Byte] -> classOf[jl.Byte],
    classOf[Short] -> classOf[jl.Short],
    classOf[Char] -> classOf[jl.Character],
    classOf[Int] -> classOf[jl.Integer],
    classOf[Long] -> classOf[jl.Long],
    classOf[Float] -> classOf[jl.Float],
    classOf[Double] -> classOf[jl.Double],
    classOf[Boolean] -> classOf[jl.Boolean],
    classOf[Unit] -> classOf[jl.Void]
  )
  def getErasure(cS: ClassManifest[_]) =
    /*if (cS <:< ClassManifest.AnyVal)
      primitiveToWrapper(cS.erasure)
    else
      cS.erasure*/
    primitiveToWrapper.getOrElse(cS.erasure, cS.erasure)
  /*
   * This is required to get the expected behavior also for primitive types. Class.isInstance(Object) documents that
   * "If this Class object represents a primitive type, this method returns false.", which makes sense since an Object
   * can never be an instance of a primitive type.
   */
  def apply[T, S](x: Exp[T], cS: ClassManifest[S]) = apply[T, S](x, getErasure(cS))
}

//Equality comparison must consider also classS. Therefore, classS must be a class parameter.
case class IfInstanceOf[T, S](x: Exp[T], classS: Class[_]) extends UnaryOpExp[T, Option[S], IfInstanceOf[T, S]](x) {
  def copy(x: Exp[T]) = IfInstanceOf(x, classS)
  def interpret() =
    Util.ifInstanceOfBody(x.interpret(), classS)
}
