package ivm

package object expressiontree {
  type TravMessage[+T] = Message[Traversable[T]]
  type TravMsgSeqPublisher[+T, +Pub <: TravMsgSeqPublisher[T, Pub]] = MsgSeqPublisher[Traversable[T], Pub]
  type TravMsgSeqSubscriber[-T, -Repr] = MsgSeqSubscriber[Traversable[T], Repr]
  type Var = TypedVar[_]

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
