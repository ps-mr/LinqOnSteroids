package ivm.rfp

trait DeltaSetOfIncDefs {
  //We might never need deltas on Sets but only on Bags. But at the base level, a Set can change. Hm.
  case class SetChange[T, DT](added: Set[T], removed: Set[T], changes: Seq[(T, DT)])
  implicit def deltaSet[T, DT](implicit d: Delta[T, DT]): Delta[Set[T], SetChange[T, DT]] = new Delta[Set[T], SetChange[T, DT]] {
    // Members declared in ivm.rfp.Delta
    def reassemble(base: Set[T], delta: SetChange[T, DT]): Set[T] =
      (delta.changes foldLeft base) { (state, change) =>
        state - change._1 + (d reassemble (change._1, change._2)) //This is also quite inefficient. But it's O(1) (relative to d.reassemble)!
        //However, it highlights that this framework makes sense for purely functional mutation - because the reassemble is a pure function!
        //When T is a mutable, incremental collection, we want the update to happen as a side effect.
      } ++ delta.added -- delta.removed

    // Members declared in ivm.rfp.Group
    def inverse(c: SetChange[T, DT]): SetChange[T, DT] =
      c match {
        case SetChange(added, removed, changes) =>
          SetChange(removed, added, changes map (change => (change._1, d inverse (change._2))))
      }

    // Members declared in scalaz.Semigroup
    def append(s1: SetChange[T, DT], s2: => SetChange[T, DT]): SetChange[T, DT] =
      (s1, s2) match {
        case (SetChange(added1, removed1, changes1), SetChange(added2, removed2, changes2)) =>
          //asNormalizedSetChange(added1 + added2, removed1 + removed2)
          SetChange(added1 -- removed2 ++ added2, removed1 -- added1 ++ removed2, changes1 ++ changes2)
          //Check correctness. I think this assumes that the view object is
          //normalized, a bit like in that paper I hated,
          //Incremental View Maintenance with Duplicates (1995?).
          //However, here the normalization is done dynamically, not at the
          //symbolic level in the derivation formulas. This makes derivation
          //formulas easier but gives less opportunities for simplification.
          //Probably things would be simpler with a single multiset.
      }

    // Members declared in scalaz.Zero
    val zero: SetChange[T, DT] = SetChange(Set.empty, Set.empty, Seq.empty)
  }
}
