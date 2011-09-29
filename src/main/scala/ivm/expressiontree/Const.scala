package ivm.expressiontree

case class Const[T: ClassManifest](x: T) extends NullaryExp[T] {
  override def interpret() = x
  override def toString() = {
    val s = x.toString()
    if (s.length() > 100) s.take(100)+"..." else s
  }
}
