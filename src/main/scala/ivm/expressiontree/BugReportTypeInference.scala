package ivm.expressiontree

import collection.{TraversableView, TraversableLike}

/**
 * User: pgiarrusso
 * Date: 11/12/2011
 */

object BugReportTypeInference {
  case class View1[T, Repr <: TraversableLike[T, Repr]](base: Repr) {
    def copy(base: Repr): View1[T, Repr] = View1(base)
  }

/*
  trait Foo[T, Repr <: TraversableLike[T, Repr]] {
    def copy(base: Repr): ViewInheritingReturnType[T, Repr]
  }
  //I thought this wouldn't compile
  case class ViewInheritingReturnType[T, Repr <: TraversableLike[T, Repr]](base: Repr)
    extends Foo[T, Repr]
  {
    def copy(base: Repr) = ViewInheritingReturnType(base)
  }

  trait Foo2[T, Repr <: TraversableLike[T, Repr], Ret] {
    def copy(base: Repr): Ret
  }
  case class ViewInheritingReturnType2[T, Repr <: TraversableLike[T, Repr]](base: Repr)
    extends Foo2[T, Repr, ViewInheritingReturnType2[T, Repr]]
  {
    def copy(base: Repr) = ViewInheritingReturnType2(base) //Does not compile: Nothing is inferred instead of T
  }
  */

  case class ViewNoTypeAnnot[T, Repr <: TraversableLike[T, Repr]](base: Repr)
  {
    //def copy(base: Repr) = ViewNoTypeAnnot(base) //Does not compile: Nothing is inferred instead of T
    def copy(base: Repr) = ViewNoTypeAnnot[T, Repr](base)
  }

  case class ViewNoTypeAnnotWorkingInference[T, Repr <: TraversableLike[T, Repr]](base: Repr with TraversableLike[T, Repr]) {
    def copy(base: Repr) = ViewNoTypeAnnotWorkingInference(base)
  }

  trait Exp[+T]
  case class View[T, Repr <: TraversableLike[T, Repr]](base: Exp[Repr with TraversableLike[T, Repr]]) {
    def copy(base: Exp[Repr]) = View(base)
  }
}