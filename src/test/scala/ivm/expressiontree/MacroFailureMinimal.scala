package ivm.expressiontree

object MacroFailureMinimal {
  trait Exp[+T]
  case class Const[+T](t: T) extends Exp[T]
  implicit def pure[T](t: T): Exp[T] = Const(t)
  def asExp[T](t: Exp[T]) = t

  import collection.generic.CanBuildFrom
  import collection.TraversableLike
  //case class MapNode[T, Repr <: Traversable[T] with TraversableLike[T, Repr],
                   //U, That <: Traversable[U] with TraversableLike[U, That]](base: Exp[Repr with Traversable[T]], f: Fun[T, U])
                            //(implicit c: CanBuildFrom[Repr with Traversable[T], U, That with Traversable[U]])
  implicit class TraversableLikeOps[T, Repr <: Traversable[T] with TraversableLike[T, Repr]](t: Exp[Repr with TraversableLike[T, Repr]]) {
    def map[U, That <: Traversable[U] with TraversableLike[U, That]](f: Exp[T] => Exp[U])(implicit c: CanBuildFrom[Repr, U, That]): Exp[That] = ??? //MapNode(base, f)
  }
  val coll = asExp(List(1, 2, 3))

  val f15_noMacro = {
    coll map (i => i)
  }

  import Macros._

  val f15 = macroId {
    coll map (i => i)
  }
}

// vim: set sw=2 et:
