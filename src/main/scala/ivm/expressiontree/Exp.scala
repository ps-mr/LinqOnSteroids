package ivm.expressiontree

trait Exp[+T] extends MsgSeqPublisher[T, Exp[T]] {
  /*
  type RootType
  private[ivm] def activateIVM() {}

  //XXX: does this really belong here?
  private[ivm] def pullAndPropagateContent() {}
  /*private[ivm]*/ def isRoot = roots.isEmpty

  /*private[ivm]*/ def roots: Seq[Exp[RootType]] = Nil
  /*private[ivm]*/ def visitPreorderRoots(visitor: Exp[_] => Unit) = visitPreorder(visitor, _.roots)
   */

  //This method recomputes the contained value
  def interpret(): T

  //This method returns the cached value (if any) or invokes interpret().
  def value(): T = interpret()

  def nodeArity: Int

  /*private[ivm]*/ def children: List[Exp[_]]
  protected def checkedGenericConstructor(v: List[Exp[_]]): Exp[T]

  /*private[ivm]*/ def genericConstructor(v: List[Exp[_]]): Exp[T] =
    if (v.length == nodeArity)
      checkedGenericConstructor(v)
    else
      throw new IllegalArgumentException()

  // some child management auxiliary functions

  /*private[ivm]*/ def visitPreorder(visitor: Exp[_] => Unit, childSelector: Exp[_] => Seq[Exp[_]]) {
    visitor(this)
    for (c <- childSelector(this)) {
      c.visitPreorder(visitor, childSelector)
    }
  }

  /*private[ivm]*/ def transform(transformer: Exp[_] => Exp[_]): Exp[T] = {
    val transformedChildren = children mapConserve (_ transform transformer)
    val newself =
      if (transformedChildren eq children)
        this
      else
        genericConstructor(transformedChildren)
    transformer(newself).asInstanceOf[Exp[T]]
  }
  //This could use as interface some Foldable-like stuff (or Haskell's Traversable, IIRC).
  /*private[ivm]*/ def treeMap[S](mapper: (Exp[_], Seq[S]) => S): S = {
    val mappedChilds = for (c <- children) yield c.treeMap(mapper)
    mapper(this, mappedChilds)
  }

  /* I renamed this method to avoid conflicts with Matcher.find. XXX test if
   * just making it private also achieves the same result.
   */
  /*private[ivm]*/ def __find(filter: PartialFunction[Exp[_], Boolean]): Seq[Exp[_]] = {
    val baseSeq =
      if (PartialFunction.cond(this)(filter))
        Seq(this)
      else
        Seq.empty
    children.map(_ __find filter).fold(baseSeq)(_ ++ _)
  }

  // This overload is not called find because that would confuse type inference - it would fail to infer that filter's
  // domain type is Exp[_].
  /*private[ivm]*/ def findTotFun(filter: Exp[_] => Boolean): Seq[Exp[_]] = __find(filter.asPartial)

  /*private[ivm]*/ def isOrContains(e: Exp[_]): Boolean =
    (this findTotFun (_ == e)).nonEmpty

  /*private[ivm]*/ def substSubTerm[S](SubTerm: Exp[_], e: Exp[S]) =
    transform {
      case SubTerm => e
      case exp => exp
    }

  /*private[ivm]*/ def freeVars: Set[Var] = {
    def mapper(e: Exp[_], c: Seq[Set[Var]]): Set[Var] = e match {
      case v@Var(_) => Set(v)
      case fe@Fun(_) => c.fold(Set.empty)(_ union _).filter(!_.equals(fe.x))
      case _ => c.fold(Set.empty)(_ union _)
    }
    treeMap(mapper)
  }
  def toCode: String = ""
  def persistValues() { children foreach (_.persistValues()) }

  //Methods for the clients of the library, rather than for the implementations.
  //They simply produce the appropriate expression tree nodes.
  final def ==#[S >: T](that: Exp[S]): Exp[Boolean] = Eq(this, that)
  // This variant is needed because null <: S but also null <: Exp[S], so the needed call to pure won't be inserted
  // manually when that is statically known to be null.
  final def ==#(that: Null): Exp[Boolean] = Eq(this, Const(null))

  final def !=#[S >: T](that: Exp[S]): Exp[Boolean] = Not(this ==# that)
  final def !=#(that: Null): Exp[Boolean] = Not(this ==# that)

  final def isInstanceOf_#[S: ClassTag: TypeTag]: Exp[Boolean] = IsInstanceOf[T, S](this)
  final def asInstanceOf_#[S: ClassTag: TypeTag]: Exp[S] = AsInstanceOf[T, S](this)
}

/*private[ivm]*/ object Exp {
  private def ordering[T] = new Ordering[Exp[T]] {
    override def compare(a: Exp[T], b: Exp[T]): Int = a.toString compareTo b.toString
  }
  def min[T](a: Exp[T], b: Exp[T]): Exp[T] = ordering.min(a,b)
  def max[T](a: Exp[T], b: Exp[T]): Exp[T] = ordering.max(a,b)
}
