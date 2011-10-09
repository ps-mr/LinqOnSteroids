package ivm.expressiontree


case class IfInstanceOf[T,S](x: Exp[T])(implicit cS: ClassManifest[S]) extends UnaryOpExp[T,Option[S]](x) {
  private[this] val classS = cS.erasure
  def copy(x: Exp[T]) = IfInstanceOf(x)
  def interpret() = {
    val v = x.interpret()
    if (v == null || !classS.isInstance(v))
      None
    else
      Some(v.asInstanceOf[S])
  }
}
