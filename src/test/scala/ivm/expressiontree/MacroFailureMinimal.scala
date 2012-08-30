package ivm.expressiontree

object MacroFailureMinimalHelper {
  trait Exp[+T]
  case class Const[+T](t: T) extends Exp[T]
  implicit def pure[T](t: T): Exp[T] = Const(t)
  def asExp[T](t: Exp[T]) = t

  import collection.generic.CanBuildFrom
  import collection.TraversableLike
  implicit class TraversableLikeOps[T, Repr <: Traversable[T] with TraversableLike[T, Repr]](t: Exp[Repr with TraversableLike[T, Repr]]) {
    def map[U, That <: Traversable[U] with TraversableLike[U, That]](f: Exp[T] => Exp[U])(implicit c: CanBuildFrom[Repr, U, That]): Exp[That] = ???
    def filter(f: Exp[T] => Exp[Boolean]): Exp[Repr] = ???
  }
  val coll = asExp(List(1, 2, 3))
}

object MacroFailureMinimal {
  import Macros._
  import MacroFailureMinimalHelper._

  val f15_noMacro = {
    coll map (i => i)
  }

  val f15 = macroId {
    coll map (i => i)
  }

/*
  val f15_show = ctShowDebug {
    coll map (i => i)
  }

  val f15_show_2 = ctShowDebug {
    coll map (i => i) filter (i => false)
  }
  */
}

// vim: set sw=2 et:
