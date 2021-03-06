package ivm.expressiontree

import collection.{immutable, TraversableLike}
import collection.generic.CanBuildFrom

/**
 * User: pgiarrusso
 * Date: 18/7/2012
 */


object TypeFilter {
  def apply[T, C[+X] <: TraversableLike[X, C[X]], D[+_], S: ClassTag /* is this too strict? <: T */](base: Exp[C[D[T]]], f: Exp[D[T] => T])(implicit cdsTTag: TypeTag[C[D[S]]]): TypeFilter[T, C, D, S] =
    apply[T, C, D, S](base, f, ClassUtil.boxedErasure(classTag[S]))
}

//Just like for IfInstanceOf, equality comparison must consider also classS. Therefore, classS must be a class parameter.
case class TypeFilter[T, C[+X] <: TraversableLike[X, C[X]], D[+_], S /* is this too strict? <: T */](base: Exp[C[D[T]]], f: Exp[D[T] => T], classS: Class[_])(implicit cdsTTag: TypeTag[C[D[S]]])
  extends Arity2Op[Exp[C[D[T]]], Exp[D[T] => T], C[D[S]], TypeFilter[T, C, D, S]](base, f) with PersistClassS[C[D[S]]] {
  override def interpret() = {
    val b: C[D[T]] = base.interpret()
    b.filter(x => classS.isInstance(f.interpret()(x))).asInstanceOf[C[D[S]]]
  }
  //XXX: the cast '.asInstanceOf[C[D[S]]]' is missing. How can we encode it? With an extra manifest?
  //XXX: nameCDS, at times, is the result of erasure; hence we get java.lang.Integer instead of Int.
  override def toCode = "%s.filter(el => %s.isInstance(%s)).asInstanceOf[%s]" format (
    base.toCode, persistedValue, Lifting.app(f)(NamedVar("el")).toCode, Compile.manifestToString(typeTag[C[D[S]]]))
  override def copy(base: Exp[C[D[T]]], f: Exp[D[T] => T]) = TypeFilter[T, C, D, S](base, f, classS)
}

// XXX: It is not clear whether the cast from Repr to That is always valid. OTOH, this could express typeFilter on Map,
// though not necessarily with a desirable interface.
case class TypeFilter2[T, D[+_], Repr <: TraversableLike[D[T], Repr], S, That](base: Exp[Repr],
                                                                               f: Exp[D[T] => T])(implicit cS: ClassTag[S],
                                                                                                  cb: CanBuildFrom[Repr, S, That])
  extends Arity2Op[Exp[Repr], Exp[D[T] => T], That, TypeFilter2[T, D, Repr, S, That]](base, f)
{
  private[this] val classS = ClassUtil.boxedErasure(cS)
  def interpret() = {
    val b: Repr = base.interpret()
    b.filter(x => classS.isInstance(f.interpret()(x))).asInstanceOf[That]
  }
  def copy(base: Exp[Repr], f: Exp[D[T] => T]) = TypeFilter2[T, D, Repr, S, That](base, f)
}

case class TypeCase[Case, +Res](classS: Class[_], guard: FunSym[Case, Boolean], f: FunSym[Case, Res]) {
  //The setters for this field use Res in contravariant position, hence it is important to make them private.
  private[this] var _guardInt: Case => Boolean = _
  private[this] var _fInt: Case => Res = _
  private[expressiontree] def guardInt: Case => Boolean = _guardInt
  private[expressiontree] def fInt: Case => Res = _fInt
  private[expressiontree] def preInterpret() {
    _guardInt = guard.interpret()
    _fInt = f.interpret()
  }
}

//The implementation of this function relies on details of erasure for performance:
//- We use null instead of relying on Option, but we filter null values away. In theory this is only allowed if Res >: Null
//that is Res <: AnyRef; this is valid for all types but Res <: AnyVal, i.e. for primitive types, but since Res is a type
//parameter, it will be erased to java.lang.Object and even primitive types will be passed boxed.
//Hence in practice v: Res can be casted to AnyRef and compared against null.
//- What happens if the underlying function returns null? Argh. You shouldn't do that!
case class TypeCaseExp[BaseT, Repr <: Traversable[BaseT] with TraversableLike[BaseT, Repr], Res: TypeTag, +That /*XXX to drop*/](e: Exp[Repr with TraversableLike[BaseT, Repr]], cases: Seq[TypeCase[_ /*Case_i*/, Res]])/*(implicit protected[this] val c: CanBuildFrom[TraversableView[BaseT, Repr], Res, That])*/ extends Def[immutable.Set[Res]] {
  override def nodeArity = 2 * cases.length + 1
  override def children = e +: (cases.toList.flatMap /*[Exp[_], Seq[Exp[_]]] */(c => Seq[Exp[_]](c.guard, c.f)))
  override protected def checkedGenericConstructor(v: List[Exp[Any]]): Def[immutable.Set[Res]] =
    TypeCaseExp(
    v.head.asInstanceOf[Exp[Repr]],
      (cases, v.tail.grouped(2).toSeq).zipped map {case (tc, Seq(guard, f)) => TypeCase(tc.classS, guard.asInstanceOf[FunSym[Any, Boolean]], f.asInstanceOf[FunSym[Any, Res]])})

  private def checkF(v: BaseT): Res = {
    for (t <- cases.asInstanceOf[Seq[TypeCase[Any, Res]]]) {
      if (t.classS.isInstance(v) && t.guardInt(v))
        return t.fInt(v)
    }
    null.asInstanceOf[Res]
  }
  override def interpret() = {
    //Since cases can contain open terms, preInterpret() must be called at each call of interpret() - the environment might be different and we might thus get different
    //results. We needn't call them once per element of e, since TypeCaseExp binds no variable iterating over e.
    cases foreach (_ preInterpret())
    (e.interpret().view map checkF filter (_.asInstanceOf[AnyRef] ne null)).toSet
  }
  private def externalInterpret(e: Repr with TraversableLike[BaseT, Repr], cases: Seq[(Any => Boolean, Any => Res, Class[_])]): immutable.Set[Res] = {
    (for {
      v <- e
      (guard, f, classS) <- cases
      if classS.isInstance(v) && guard(v)
    } yield f(v)).toSet
  }
  override def toCode = {
    val caseCode =
      for {
        t <- cases.asInstanceOf[Seq[TypeCase[Any, Res]]]
        className = t.classS.getName /*XXX Won't work in general, we'd need TypeTags to do this right.*/
        namedVar = NamedVar("el.asInstanceOf[%s]" format className) //Use instead an AsInstanceOf node, to add!
      }
      yield
        s"""  case el: ${className} if ivm.expressiontree.Util.let(el)(${t.guard.toCode}) =>
           |    Seq(ivm.expressiontree.Util.let(el)(${t.f.toCode}))""".stripMargin
    //Alternative: fix substitution in Attach and write:
    //t.guard.defNode.f(namedVar).toCode, t.f.defNode.f(namedVar).toCode)
    """(%s flatMap {
      |%s
      |  case _ => Seq.empty
      |}).toSet[%s]""".stripMargin.format(e.toCode, caseCode.mkString("\n"), Compile.manifestToString(typeTag[Res]))
  }
  //cases map { case TypeCase(classS, f) => (v: Base) => if (v == null || !classS.isInstance(v)) Util.ifInstanceOfBody(v, classS)}
}

