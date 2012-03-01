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
                            (implicit protected[this] val c: CanBuildFrom[Repr, U, That]) extends Arity2Op[Exp[Repr], FuncExp[T, TraversableOnce[U]], That, FlatMap[T, Repr, U, That]](base, f) {
  override def interpret() = base.interpret() flatMap f.interpret()
  override def copy(base: Exp[Repr], f: FuncExp[T, TraversableOnce[U]]) = FlatMap[T, Repr, U, That](base, f)
}

case class MapOp[T, Repr <: FilterMonadic[T, Repr],
                 U, That](base: Exp[Repr], f: FuncExp[T, U])
                          (implicit protected[this] val c: CanBuildFrom[Repr, U, That]) extends Arity2Op[Exp[Repr], FuncExp[T, U], That, MapOp[T, Repr, U, That]](base, f) {
  override def interpret() = base.interpret() map f.interpret()
  override def copy(base: Exp[Repr], f: FuncExp[T, U]) = MapOp[T, Repr, U, That](base, f)
}

case class Filter[T, Repr <: TraversableLike[T, Repr]](base: Exp[Repr],
                                                      f: FuncExp[T, Boolean]) extends Arity2Op[Exp[Repr], FuncExp[T, Boolean], Repr, Filter[T, Repr]](base, f) {
  override def interpret() = base.interpret() filter f.interpret()
  override def copy(base: Exp[Repr], f: FuncExp[T, Boolean]) = Filter(base, f)
 }

case class WithFilter[T, Repr <: TraversableLike[T, Repr]](base: Exp[Repr],
                                                      f: FuncExp[T, Boolean])
                                                      extends Arity2Op[Exp[Repr], FuncExp[T, Boolean], TraversableView[T, Repr], WithFilter[T, Repr]](base, f) {
  override def interpret() = base.interpret().view filter f.interpret()
  override def copy(base: Exp[Repr], f: FuncExp[T, Boolean]) = WithFilter[T, Repr](base, f)
}

case class View[T, Repr <: TraversableLike[T, Repr]](base: Exp[Repr with TraversableLike[T, Repr]]) extends Arity1OpExp[Repr, TraversableView[T, Repr], View[T, Repr]](base) {
  override def interpret() = base.interpret().view
  override def copy(base: Exp[Repr]) = View(base)
}

case class Force[T, Repr <: Traversable[T] with TraversableLike[T, Repr],
                 ViewColl <: TraversableViewLike[T, Repr, ViewColl] with TraversableView[T, Repr] with TraversableLike[T, ViewColl], That]
                (base: Exp[ViewColl with TraversableViewLike[T, Repr, ViewColl]])
                (implicit protected[this] val bf: CanBuildFrom[Repr, T, That]) extends Arity1OpExp[ViewColl, That, Force[T, Repr, ViewColl, That]](base) {
  override def interpret() = base.interpret().force
  override def copy(base: Exp[ViewColl]) = Force[T, Repr, ViewColl, That](base)
}

//A bit of a hack, since it does not return the most precise type possible.
case class ForceIfPossible[T, Repr <: Traversable[T] with TraversableLike[T, Repr]](base: Exp[Repr with TraversableLike[T, Repr]]) extends Arity1OpExp[Repr, Traversable[T], ForceIfPossible[T, Repr]](base) {
  override def interpret() = {
    Lifting.TraversableForceable.force(base.interpret())
  }
  override def copy(base: Exp[Repr]) = ForceIfPossible(base)
}

case class Union[T, Repr <: TraversableLike[T, Repr], That](base: Exp[Repr], that: Exp[Traversable[T]])
                                   (implicit protected[this] val c: CanBuildFrom[Repr, T, That]) extends Arity2OpExp[Repr, Traversable[T], That, Union[T, Repr, That]](base, that) {
  override def interpret() = base.interpret() ++ that.interpret()
  override def copy(base: Exp[Repr], that: Exp[Traversable[T]]) = Union[T, Repr, That](base, that)
}

case class Diff[T, Repr <: collection.Set[T] with SetLike[T, Repr]](base: Exp[Repr], that: Exp[GenTraversableOnce[T]]) extends Arity2OpExp[Repr, GenTraversableOnce[T], Repr, Diff[T, Repr]](base, that) {
  override def interpret() = base.interpret() -- that.interpret()
  override def copy(base: Exp[Repr], that: Exp[GenTraversableOnce[T]]) = Diff[T, Repr](base, that)
}

case class Size[T, Repr <: Traversable[T]](t: Exp[Repr with Traversable[T]]) extends Arity1OpExp[Repr, Int, Size[T, Repr]](t) {
  def interpret() = t.interpret().size
  def copy(t: Exp[Repr]) = Size(t)
}

case class IsEmpty[T, Repr <: Traversable[T]](t: Exp[Repr with Traversable[T]]) extends Arity1OpExp[Repr, Boolean, IsEmpty[T, Repr]](t) {
  def interpret() = t.interpret().isEmpty
  def copy(t: Exp[Repr]) = IsEmpty(t)
}

object TypeFilter {
  def apply[T, C[+X] <: TraversableLike[X, C[X]], D[+_], S /* is this too strict? <: T */](base: Exp[C[D[T]]], f: Exp[D[T] => T], cS: ClassManifest[S]) =
    apply[T, C, D, S](base, f, IfInstanceOf.getErasure(cS))
}

//Just like for IfInstanceOf, equality comparison must consider also classS. Therefore, classS must be a class parameter.
case class TypeFilter[T, C[+X] <: TraversableLike[X, C[X]], D[+_], S /* is this too strict? <: T */](base: Exp[C[D[T]]], f: Exp[D[T] => T], classS: Class[_])
                                  extends Arity2Op[Exp[C[D[T]]], Exp[D[T] => T], C[D[S]], TypeFilter[T, C, D, S]](base, f) {
  override def interpret() = {
    val b: C[D[T]] = base.interpret()
    b.filter(x => classS.isInstance(f.interpret()(x))).asInstanceOf[C[D[S]]]
  }
  override def copy(base: Exp[C[D[T]]], f: Exp[D[T] => T]) = TypeFilter[T, C, D, S](base, f, classS)
}

// XXX: It is not clear whether the cast from Repr to That is always valid. OTOH, this could express typeFilter on Map,
// though not necessarily with a desirable interface.
case class TypeFilter2[T, D[+_], Repr <: TraversableLike[D[T], Repr], S, That](base: Exp[Repr],
                                                                               f: Exp[D[T] => T])(implicit cS: ClassManifest[S],
                                                                                                  cb: CanBuildFrom[Repr, S, That])
  extends Arity2Op[Exp[Repr], Exp[D[T] => T], That, TypeFilter2[T, D, Repr, S, That]](base, f)
{
  private[this] val classS = IfInstanceOf.getErasure(cS)
  def interpret() = {
    val b: Repr = base.interpret()
    b.filter(x => classS.isInstance(f.interpret()(x))).asInstanceOf[That]
  }
  def copy(base: Exp[Repr], f: Exp[D[T] => T]) = TypeFilter2[T, D, Repr, S, That](base, f)
}

case class TypeCase[Case, Res](classS: Class[Case], f: FuncExp[Case, Res])

//The implementation of this function relies on details of erasure for performance:
//- We use null instead of relying on Option, but we filter null values away. In theory this is only allowed if Res >: Null
//that is Res <: AnyRef; this is valid for all types but Res <: AnyVal, i.e. for primitive types, but since Res is a type
//parameter, it will be erased to java.lang.Object and even primitive types will be passed boxed.
//Hence in practice v: Res can be casted to AnyRef and compared against null.
case class TypeCaseExp[BaseT, Repr <: TraversableLike[BaseT, Repr], Res, That <: TraversableLike[Res, That]](e: Exp[Repr with TraversableLike[BaseT, Repr]], cases: Seq[TypeCase[_ /*Case_i*/, Res]])(implicit protected[this] val c: CanBuildFrom[Repr, Res, That]) extends Exp[TraversableView[Res, That]] {
  override def nodeArity = cases.length + 1
  override def children = e +: (cases map (_.f))
  override def checkedGenericConstructor: Seq[Exp[_]] => Exp[TraversableView[Res, That]] = v => TypeCaseExp(v.head.asInstanceOf[Exp[Repr]], (cases, v.tail).zipped map ((tc, f) => TypeCase(tc.classS.asInstanceOf[Class[Any]], f.asInstanceOf[FuncExp[Any, Res]])))
  private def checkF(v: BaseT): Res = {
    for (TypeCase(classS, f: FuncExp[s, _/*Res*/]) <- cases) {
      if (classS.isInstance(v))
        return f.interpret()(v.asInstanceOf[s]).asInstanceOf[Res]
    }
    null.asInstanceOf[Res]
  }
  override def interpret() = {
    (e.interpret() map checkF).view filter (_.asInstanceOf[AnyRef] ne null)
  }
  //cases map { case TypeCase(classS, f) => (v: Base) => if (v == null || !classS.isInstance(v)) Util.ifInstanceOfBody(v, classS)}
}


//Note: this class also handles IVM, though in an incomplete way
case class Forall[T](coll: Exp[Traversable[T]], f: FuncExp[T, Boolean])
  extends Arity1OpExp[Traversable[T], Boolean, Forall[T]](coll) with EvtTransformerEl[Traversable[T], Boolean, Traversable[T]]
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
