package ivm.rfp

trait DeltaValueDefs {
  case class ChangeValue[T](src: T, dst: T) extends GenArrow[T, T]

  /*
   * This satisfies indeed group laws? No. Either the A + (-A) != the identity (we get ChangeValue(x, x)), or we normalize that to NoChange, but then we lose associativity since
   * (A + (-A)) + B = B, while A + (-A + B) where A = (A1, A2), B = (B1, B2), gives (A1, A2) + ((A2, A1) + (B1, B2)) = (A1, B2) != B.
   *
   * That's all because we allow composing elements when the values in-between don't match.
   *
   * In fact, ChangeValue values naturally correspond to the arrows of a
   * (rather boring) category (with a single arrow between each pair of
   * objects), and append to arrow composition. Since all arrows are
   * invertible, we get in fact a categorical groupoid (which induces an
   * algebraic groupoid, but the categorical definition fits better)!
   * http://en.wikipedia.org/wiki/Groupoid
   */
  implicit def deltaValue[T]: Delta[T, ChangeValue[T]] = new Delta[T, ChangeValue[T]] {
    //Maybe split out the group instance from here (?)
    def reassemble(base: T, delta: ChangeValue[T]): T = delta match {
      case ChangeValue(oldVal, newVal) =>
        if (oldVal != base) {
          println(s"Warning: oldVal '${oldVal}' != base '${base}', what should we do?")
        }
        newVal
    }
    
    // Members declared in scalaz.Group
    def inverse(a: ChangeValue[T]): ChangeValue[T] = a match {
      case ChangeValue(oldVal, newVal) => ChangeValue(newVal, oldVal)
    }
    
    // Members declared in scalaz.Semigroup
    def append(s1: ChangeValue[T], s2: => ChangeValue[T]): ChangeValue[T] = {
      (s1, s2) match {
        case (ChangeValue(old1, new1), ChangeValue(old2, new2)) =>
          //Does this make sense when new1 != old2?
          ChangeValue(old1, new2)
      }
    }
    
    // Members declared in scalaz.Zero
    val zero: ChangeValue[T] = ???
  }
}

