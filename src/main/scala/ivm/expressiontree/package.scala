package ivm

package object expressiontree {
  import scala.reflect.runtime.universe
  val u = universe
  type ClassTag[T] = reflect.ClassTag[T]
  val ClassTag = reflect.ClassTag
  type TypeTag[T] = u.TypeTag[T]
  def classTag[T](implicit c: ClassTag[T]) = c
  def typeTag[T](implicit ttag: TypeTag[T]) = ttag
  implicit def toAtomImpl[T](d: Def[T]): Exp[T] = BaseLangImpl toAtom d

  /*
  type TravMessage[+T] = Message[Traversable[T]]
  type TravMsgSeqPublisher[+T, +Pub <: TravMsgSeqPublisher[T, Pub]] = MsgSeqPublisher[Traversable[T], Pub]
  type TravMsgSeqSubscriber[-T, -Repr] = MsgSeqSubscriber[Traversable[T], Repr]
  */
  type Var = TypedVar[_]
  //With 2.10, these must be defined together.
  object Var {
    def apply(id: Int) = TypedVar[Nothing](id)
    def unapply(x: Var): Option[Int] = TypedVar.unapply(x)
  }
  //case class Var(override val id: Int) extends TypedVar[Nothing](id)

  //From http://stackoverflow.com/questions/6834000/scala-partialfunctions-from-concrete-ones, altered by avoiding
  //refinement types.
  implicit def funcAsPartial[A, B](f: A => B) = new PartialFunctionOps(f)

  class PartialFunctionOps[A, B](f: A => B) {
    /** only use if `f` is defined everywhere */
    def asPartial = new PartialFunction[A, B] {
      def isDefinedAt(a: A) = true
      def apply(a: A) = f(a)
    }
  }
}
