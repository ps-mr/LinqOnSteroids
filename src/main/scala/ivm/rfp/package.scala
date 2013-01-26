package ivm

package object rfp {
  type TravMessage[+T] = Message[Traversable[T]]
  type TravMsgSeqPublisher[+T, +Pub <: TravMsgSeqPublisher[T, Pub]] = MsgSeqPublisher[Traversable[T], Pub]
  type TravMsgSeqSubscriber[-T, -Repr] = MsgSeqSubscriber[Traversable[T], Repr]
}
