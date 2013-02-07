package ivm.rfp

trait DeltaSetDefs {
  sealed trait UnorderedCollDelta[T]
  //We might never need deltas on Sets but only on Bags. But at the base level, a Set can change. Hm.
  case class SetChange[T](added: Set[T], removed: Set[T]) extends UnorderedCollDelta[T]

  implicit def deltaSet[T]: Delta[Set[T], UnorderedCollDelta[T]] = new Delta[Set[T], UnorderedCollDelta[T]] {
    // Members declared in ivm.rfp.Delta
    def reassemble(base: Set[T], delta: UnorderedCollDelta[T]): Set[T] = delta match {
      case SetChange(added, removed) => base ++ added -- removed
    }
    
    // Members declared in ivm.rfp.Group
    def inverse(a: UnorderedCollDelta[T]): UnorderedCollDelta[T] = a match {
      case SetChange(added, removed) => SetChange(removed, added)
    }

    // Members declared in scalaz.Semigroup
    def append(s1: UnorderedCollDelta[T], s2: => UnorderedCollDelta[T]): UnorderedCollDelta[T] =
      (s1, s2) match {
        case (SetChange(added1, removed1), SetChange(added2, removed2)) =>
          //asNormalizedSetChange(added1 + added2, removed1 + removed2)
          SetChange(added1 -- removed2 ++ added2, removed1 -- added1 ++ removed2)
          //Check correctness. I think this assumes that the view object is
          //normalized, a bit like in that paper I hated,
          //Incremental View Maintenance with Duplicates (1995?).
          //However, here the normalization is done dynamically, not at the
          //symbolic level in the derivation formulas. This makes derivation
          //formulas easier but gives less opportunities for simplification.
          //Probably things would be simpler with a single set.
      }
    
    // Members declared in scalaz.Zero
    val zero: UnorderedCollDelta[T] = SetChange(Set.empty, Set.empty)
  }
}

