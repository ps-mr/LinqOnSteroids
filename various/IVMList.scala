import scala.util.continuations._
import collection.TraversableOnce
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

//XXX?
object IVMList2 {
    def buildList[A](l: List[A]) = reset(new IVMList2(l))
    //def buildList2(a: Int) = reset(new IVMList2(List(a)))
    def main(args: Array[String]) {
        val l1 = buildList(List(1, 2, 3))
    }
}

// vim: set ts=4 sw=4 et:
