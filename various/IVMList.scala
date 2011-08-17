import scala.util.continuations._
import scala.collection.TraversableOnce
//class IVMList[T](l: List[T]) /*extends Seq[T]*/ extends Publisher[Message[A]] {
//    def foreach(p: T => Unit) {
//        l foreach p
//    }
//    def flatMap(
//}

/* Rewrite three
// pseudocode:
val l: Seq[String]
for {s <- l
    c <- chars(s)
} yield c
//Equiv
f.flatMap(s => chars(s).map(c => c))  ==  f.flatMap(s => chars(s))
String: Seq[Char] //???
chars(s) = reset {s.toChars}
    def flatMap():SeqView[] = {
        l.flatMap(this, f (el => shift {k => (k, el)}))
    }
*/

import scala.collection //Redundant
import collection.immutable
import collection.mutable._
import collection.script._
import collection.Traversable

case class MyString(s: String)
object MyString {
    def chars(s: MyString): immutable.Seq[Char] = s.s
}

//class IVMList2[A] private (l: List[A]) {
class IVMList2[A] private (l: A) {
    def capt[T, U](a: T) = shift {k: (T => U) => k}
    def capt2[T, U](a: T): T @cpsParam[U, (T => U, T)] = shift {k: (T=>U) => (k, a)}
    //def flatMap[B, U](f: (A => collection.Traversable[B]) @cpsParam[U, (A => U, A)]): IVMList2[B] = { // wrong

    //def flatMap[B, U](f: A => collection.Traversable[B] @cpsParam[U, (A => U, A)]): IVMList2[B] = { // wrong!

    //Does not work:
    //def flatMap[B](f: (A @cpsParam[collection.Traversable[B], (A => collection.Traversable[B], A)]) => collection.Traversable[B]): IVMList2[B] = {
    //Does work:
    type FlatMapParam[B] = A @cpsParam[collection.Traversable[B], (A => collection.Traversable[B], A)]
    def flatMap[B](f: FlatMapParam[B] => collection.Traversable[B]): IVMList2[B] = {
        val f1 = (x: A) => f(capt2[A, Traversable[B]](x))
        val (f2, caughtEl) = reset {
            f1(l)
            //new IVMList2(f1(l))
        }
        case class Bla(f: A => collection.Traversable[B], a: A) extends IVMList2[B](f(a).head) {
        //TODO: view-like implementation
        }
        Bla(f2, caughtEl)
    }
}

/*object IVMList2 {
    def buildList[A](l: List[A]) = reset(new IVMList2(l))
    //def buildList(a: Int) = reset(new IVMList2(List(a)))
    def main(args: Array[String]) {
        val l1 = buildList(List(1, 2, 3))
    }
}*/

trait ObservableBuffer2[T] extends ObservableBuffer[T] {
    abstract override def insertAll(n: Int, iter: collection.Traversable[T]): Unit = {
        super.insertAll(n, iter)
        var curr = n
        var msg = new Script[T]() with Undoable {
            def undo() { throw new UnsupportedOperationException("cannot undo") }
        }
        for (elem <- iter) {
            msg += Include(Index(curr), elem)
            curr += 1
        }
        publish(msg)
    }
}

object Test {
    def main(args: Array[String]) {
        val buf = new ArrayBuffer[Int] with ObservableBuffer[Int] {
            override def toString = "buf: " + super.toString
        }
        val bufFixed = new ArrayBuffer[Int] with ObservableBuffer2[Int] {
            override def toString = "bufFixed: " + super.toString
        }
        //class MySubscr extends Subscriber[ObservableBuffer[Int]]
        //bufFixed subscribe (new buf.Sub {
        buf subscribe (new buf.Sub {
            //override def notify(p: buf.Pub, evt: buf.Sub.Evt) {
            //override def notify(p: buf.Pub, evt: Message[Int
            override def notify(p: buf.Pub, evt: Message[Int] with Undoable) {
            }
        })
        val notifier = new Subscriber[Message[Int] with Undoable, ObservableBuffer[Int]] {
            override def notify(p: ObservableBuffer[Int], evt: Message[Int] with Undoable) {
                println("Event %s from %s" format (evt, p))
            }
        }
        buf subscribe notifier
        bufFixed subscribe notifier
        buf insertAll (0, List(1, 2, 3))
        bufFixed insertAll (0, List(1, 2, 3))
        buf remove 0
        bufFixed remove 0
    }
}
// vim: set ts=4 sw=4 et:
