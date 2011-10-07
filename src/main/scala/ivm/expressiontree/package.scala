package ivm

package object expressiontree {
  type TravMessage[+T] = Message[Traversable[T]]
  type TravMsgSeqPublisher[T] = MsgSeqPublisher[Traversable[T]]
  type TravMsgSeqSubscriber[-T, -Repr] = MsgSeqSubscriber[Traversable[T], Repr]
}
