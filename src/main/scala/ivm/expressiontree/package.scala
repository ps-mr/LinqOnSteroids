package ivm

package object expressiontree {
  type TravMessage[+T] = Message[Traversable[T]]
  type TravMsgSeqPublisher[+T, +Pub <: TravMsgSeqPublisher[T, Pub]] = MsgSeqPublisher[Traversable[T], Pub]
  type TravMsgSeqSubscriber[-T, -Repr] = MsgSeqSubscriber[Traversable[T], Repr]
  type Var = TypedVar[Nothing]

  //From http://stackoverflow.com/questions/6834000/scala-partialfunctions-from-concrete-ones
  private[expressiontree] def funcAsPartial[A, B](f: A => B) = new {
    /** only use if `f` is defined everywhere */
    def asPartial = new PartialFunction[A, B] {
      def isDefinedAt(a: A) = true
      def apply(a: A) = f(a)
    }
  }
}
