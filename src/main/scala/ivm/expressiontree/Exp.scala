package ivm.expressiontree

sealed trait Exp[+T]

trait Def[+T]

case class Sym[+T](defNode: Def[T]) extends Exp[T]

case class Const[T](x: T)(implicit val cTag: ClassTag[T], val tTag: TypeTag[T]) extends Exp[T]
