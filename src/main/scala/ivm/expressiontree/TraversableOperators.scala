package ivm.expressiontree

import collection.generic.{CanBuildFrom, FilterMonadic}
import collection._

/**
* User: pgiarrusso
* Date: 19/9/2011
*/

// It's amazing that Scala accepts "extends Exp[That]", since it would not accept That; most probably that's thanks to erasure.
case class FlatMap[T, Repr <: FilterMonadic[T, Repr],
                   U, That](base: Exp[Repr], f: FuncExp[T, TraversableOnce[U]])
                            (implicit protected[this] val c: CanBuildFrom[Repr, U, That]) extends BinaryOp[Exp[Repr], FuncExp[T, TraversableOnce[U]], That, FlatMap[T, Repr, U, That]](base, f) {
  override def interpret() = base.interpret() flatMap f.interpret()
  override def copy(base: Exp[Repr], f: FuncExp[T, TraversableOnce[U]]) = FlatMap[T, Repr, U, That](base, f)
}

case class MapOp[T, Repr <: FilterMonadic[T, Repr],
                 U, That](base: Exp[Repr], f: FuncExp[T, U])
                          (implicit protected[this] val c: CanBuildFrom[Repr, U, That]) extends BinaryOp[Exp[Repr], FuncExp[T, U], That, MapOp[T, Repr, U, That]](base, f) {
  override def interpret() = base.interpret() map f.interpret()
  override def copy(base: Exp[Repr], f: FuncExp[T, U]) = MapOp[T, Repr, U, That](base, f)
}

case class Filter[T, Repr <: TraversableLike[T, Repr]](base: Exp[Repr],
                                                      f: FuncExp[T, Boolean]) extends BinaryOp[Exp[Repr], FuncExp[T, Boolean], Repr, Filter[T, Repr]](base, f) {
  override def interpret() = base.interpret() filter f.interpret()
  override def copy(base: Exp[Repr], f: FuncExp[T, Boolean]) = Filter(base, f)
 }

case class WithFilter[T, Repr <: TraversableLike[T, Repr]](base: Exp[Repr],
                                                      f: FuncExp[T, Boolean])
                                                      extends BinaryOp[Exp[Repr], FuncExp[T, Boolean], TraversableView[T, Repr], WithFilter[T, Repr]](base, f) {
  override def interpret() = base.interpret().view filter f.interpret()
  override def copy(base: Exp[Repr], f: FuncExp[T, Boolean]) = WithFilter[T, Repr](base, f)
}

case class View[T, Repr <: TraversableLike[T, Repr]](base: Exp[Repr with TraversableLike[T, Repr]]) extends UnaryOpExp[Repr, TraversableView[T, Repr], View[T, Repr]](base) {
  override def interpret() = base.interpret().view
  override def copy(base: Exp[Repr]) = View(base)
}

case class Force[T, Repr <: Traversable[T] with TraversableLike[T, Repr],
                 ViewColl <: TraversableViewLike[T, Repr, ViewColl] with TraversableView[T, Repr] with TraversableLike[T, ViewColl], That]
                (base: Exp[ViewColl with TraversableViewLike[T, Repr, ViewColl]])
                (implicit protected[this] val bf: CanBuildFrom[Repr, T, That]) extends UnaryOpExp[ViewColl, That, Force[T, Repr, ViewColl, That]](base) {
  override def interpret() = base.interpret().force
  override def copy(base: Exp[ViewColl]) = Force[T, Repr, ViewColl, That](base)
}

case class Union[T, Repr <: TraversableLike[T, Repr], That](base: Exp[Repr], that: Exp[Traversable[T]])
                                   (implicit protected[this] val c: CanBuildFrom[Repr, T, That]) extends BinaryOpExp[Repr, Traversable[T], That, Union[T, Repr, That]](base, that) {
  override def interpret() = base.interpret() ++ that.interpret()
  override def copy(base: Exp[Repr], that: Exp[Traversable[T]]) = Union[T, Repr, That](base, that)
}

case class Diff[T, Repr <: collection.Set[T] with SetLike[T, Repr]](base: Exp[Repr], that: Exp[GenTraversableOnce[T]]) extends BinaryOpExp[Repr, GenTraversableOnce[T], Repr, Diff[T, Repr]](base, that) {
  override def interpret() = base.interpret() -- that.interpret()
  override def copy(base: Exp[Repr], that: Exp[GenTraversableOnce[T]]) = Diff[T, Repr](base, that)
}

case class Size[T, Repr <: Traversable[T]](t: Exp[Repr with Traversable[T]]) extends UnaryOpExp[Repr, Int, Size[T, Repr]](t) {
  def interpret() = t.interpret().size
  def copy(t: Exp[Repr]) = Size(t)
}

case class TypeFilter[T, C[+X] <: TraversableLike[X, C[X]], D[+_], S /* is this too strict? <: T */](base: Exp[C[D[T]]], f: Exp[D[T] => T])
                                 (implicit cS: ClassManifest[S])
                                  extends BinaryOp[Exp[C[D[T]]], Exp[D[T] => T], C[D[S]], TypeFilter[T, C, D, S]](base, f) {
  private[this] val classS = IfInstanceOf.getErasure(cS)

  override def interpret() = {
    val b: C[D[T]] = base.interpret()
    b.filter(x => classS.isInstance(f.interpret()(x))).asInstanceOf[C[D[S]]]
  }
  override def copy(base: Exp[C[D[T]]], f: Exp[D[T] => T]) = TypeFilter[T, C, D, S](base, f)
}

// XXX: It is not clear whether the cast from Repr to That is always valid. OTOH, this could express typeFilter on Map,
// though not necessarily with a desirable interface.
case class TypeFilter2[T, D[+_], Repr <: TraversableLike[D[T], Repr], S, That](base: Exp[Repr],
                                                                               f: Exp[D[T] => T])(implicit cS: ClassManifest[S],
                                                                                                  cb: CanBuildFrom[Repr, S, That])
  extends BinaryOp[Exp[Repr], Exp[D[T] => T], That, TypeFilter2[T, D, Repr, S, That]](base, f)
{
  private[this] val classS = IfInstanceOf.getErasure(cS)
  def interpret() = {
    val b: Repr = base.interpret()
    b.filter(x => classS.isInstance(f.interpret()(x))).asInstanceOf[That]
  }
  def copy(base: Exp[Repr], f: Exp[D[T] => T]) = TypeFilter2[T, D, Repr, S, That](base, f)
}

//Note: this class also handles IVM, though in an incomplete way
case class Forall[T](coll: Exp[Traversable[T]], f: FuncExp[T, Boolean])
  extends UnaryOpExp[Traversable[T], Boolean, Forall[T]](coll) with EvtTransformerEl[Traversable[T], Boolean, Traversable[T]]
  with ExpWithCache[Boolean]
{
  var countFalse: Int = 0
  override def interpret() = {
    //XXX: we should get the initial status otherwise.
    countFalse = coll.interpret().count(x => !f.interpret()(x))
    expResult()
  }

  override def copy(coll: Exp[Traversable[T]]) = Forall(coll, f)

  override def cache = Some(expResult()) //We probably need to make the cache _field_ optional.
  override def expResult() = countFalse == 0 //The result is always valid here.

  override def notifyEv(pub: Traversable[T], evt: Message[Traversable[T]]) {
    evt match {
      case Include(v) =>
        countFalse += (if (!f.interpret()(v)) 1 else 0)
      case Remove(v) =>
        countFalse -= (if (!f.interpret()(v)) 1 else 0)
      case Update(oldV, newV) =>
        notifyEv(pub, Remove(oldV))
        notifyEv(pub, Include(newV))
      case Reset =>
        countFalse = 0
    }
  }
}
