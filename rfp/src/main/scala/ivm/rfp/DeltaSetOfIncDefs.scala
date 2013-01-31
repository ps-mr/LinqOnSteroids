package ivm.rfp

/*
import scalaz._ 
import Scalaz._ 
*/

trait DeltaSetOfIncDefs {
  case class SetChange[T, DT](changes: Seq[(T, DT)])
  implicit def deltaSet[T, DT](implicit d: Delta[T, DT]): Delta[Set[T], SetChange[T, DT]] = new Delta[Set[T], SetChange[T, DT]] {
    // Members declared in ivm.rfp.Delta
    def embed(t: Set[T]): SetChange[T, DT] = SetChange((t map (el => (el, d embed el)))(collection.breakOut)) //Very bad, performance-wise, having to do a mapping step.
    def reassemble(base: Set[T], delta: SetChange[T, DT]): Set[T] =
      (delta.changes foldLeft base) { (state, change) =>
        state - change._1 + (d reassemble (change._1, change._2)) //This is also quite inefficient. But it's O(1) (relative to d.reassemble)!
        //However, it highlights that this framework makes sense for purely functional mutation - because the reassemble is a pure function!
        //When T is a mutable, incremental collection, we want the update to happen as a side effect.
      }

    // Members declared in ivm.rfp.Group
    def inverse(c: SetChange[T, DT]): SetChange[T, DT] = SetChange(c.changes map (change => (change._1, d inverse (change._2))))

    // Members declared in scalaz.Semigroup
    def append(s1: SetChange[T, DT], s2thunk: => SetChange[T, DT]): SetChange[T, DT] = {
      val s2 = s2thunk

      SetChange(s1.changes ++ s2.changes)
    }

    // Members declared in scalaz.Zero
    val zero: SetChange[T, DT] = ???
  }
}
