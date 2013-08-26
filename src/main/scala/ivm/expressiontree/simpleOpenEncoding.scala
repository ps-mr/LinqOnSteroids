package ivm.expressiontree

trait BaseExps {
  //Keeping the type abstract gives the same stack trace with a rather
  //different "Enclosing template or block" in the error message.
  //type Rep[+T]
  type Rep[+T] = Exp[T]
  //implicit def pure[T: ClassTag: TypeTag](t: T): Rep[T]
}
