
import collection.TraversableOnce
import collection.immutable
import collection.mutable._
import collection.script._
import collection.Traversable

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
        List(4, 5, 6) ++=: buf
        List(4, 5, 6) ++=: bufFixed
        buf remove 0
        bufFixed remove 0
    }
}

// vim: set ts=4 sw=4 et:
