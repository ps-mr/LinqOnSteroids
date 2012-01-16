object ForceableDef {
  import collection.TraversableView

  sealed trait Forceable[T, Coll] {
    def force(t: Coll): Traversable[T]
  }
  implicit def TraversableForceable[T]: Forceable[T, Traversable[T]] = new Forceable[T, Traversable[T]] {
    def force(t: Traversable[T]) = t
  }
  //Note: type inference does not pick supertypes of arguments unless needed (i.e. if inferring T from t: T, the type
  //of T will be picked usually), therefore this implicit will be picked when needed. Note that since Forceable is invariant,
  //implicit resolution will not have other alternatives
  implicit def TraversableViewForceable[T]: Forceable[T, TraversableView[T, Traversable[_]]] = new Forceable[T, TraversableView[T, Traversable[_]]] {
    def force(t: TraversableView[T, Traversable[_]]) = t.force
  }
}

object ImplicitParamBugReport1 {
  import ForceableDef._

  implicit def pimpForce[T, Coll](t: Coll)(implicit f: Forceable[T, Coll]) = new ForceOps[T, Coll](t)
  class ForceOps[T, Coll](t: Coll)(implicit f: Forceable[T, Coll]) {
    def force: Traversable[T] = f.force(t)
  }
  def fun1[T, Coll <: Traversable[T]](t: Coll)(implicit f: Forceable[T, Coll]) = pimpForce(t).force
  //Does not compile, but the typechecker does figure out that pimpForce is needed:
  def fun2[T, Coll <: Traversable[T]](t: Coll)(implicit f: Forceable[T, Coll]) = t.force
  /*
ImplicitParamBugReport.scala:126: could not find implicit value for parameter f: ivm.expressiontree.ForceableDef.Forceable[T,Coll]
[error]   def fun2[T, Coll <: Traversable[T]](t: Coll)(implicit f: Forceable[T, Coll]) = t.force
[error]                                                                                  ^
[error] one error found
   */
}

object ImplicitParamBugReport2 {
  import ForceableDef._

  //Note: "Coll with Traversable[T]" seems to help deduction of T, since apparently the type-class parameter is not enough
  // for that.
  implicit def pimpForce[T, Coll](t: Coll with Traversable[T])(implicit f: Forceable[T, Coll]) = new ForceOps[T, Coll](t)
  class ForceOps[T, Coll](t: Coll)(implicit f: Forceable[T, Coll]) {
    def force: Traversable[T] = f.force(t)
  }
  def fun1[T, Coll <: Traversable[T]](t: Coll)(implicit f: Forceable[T, Coll]) = pimpForce(t).force
  //Does compile:
  def fun2[T, Coll <: Traversable[T]](t: Coll)(implicit f: Forceable[T, Coll]) = t.force
}
