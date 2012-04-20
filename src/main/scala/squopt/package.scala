import ivm._
import expressiontree.LiftingTrait

package object squopt extends LiftingTrait {
    type Exp[+T] = expressiontree.Exp[T]
}

// vim: set ts=4 sw=4 et:
