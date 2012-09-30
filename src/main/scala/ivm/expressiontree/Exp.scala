package ivm.expressiontree

trait Exp[+T] /*extends MsgSeqPublisher[T, Exp[T]]*/ {
  /*
  type RootType
  private[ivm] def activateIVM() {}

  //XXX: does this really belong here?
  private[ivm] def pullAndPropagateContent() {}
  def isRoot = roots.isEmpty

  def roots: Seq[Exp[RootType]] = Nil
  def visitPreorderRoots(visitor: Exp[_] => Unit) = visitPreorder(visitor, _.roots)

  def visitPreorder(visitor: Exp[_] => Unit, childSelector: Exp[_] => Seq[Exp[_]]) {
    visitor(this)
    for (c <- childSelector(this)) {
      c.visitPreorder(visitor, childSelector)
    }
  }
   */

  //This method recomputes the contained value
  def interpret(): T

  //This method returns the cached value (if any) or invokes interpret().
  def value(): T = interpret()

  def nodeArity: Int

  def children: List[Exp[_]]
  protected def checkedGenericConstructor(v: List[Exp[_]]): Exp[T]

  def genericConstructor(v: List[Exp[_]]): Exp[T] =
    if (v.length == nodeArity)
      checkedGenericConstructor(v)
    else
      throw new IllegalArgumentException()

  // some child management auxiliary functions

  def transform(transformer: Exp[_] => Exp[_]): Exp[T] = {
    val transformedChildren = children mapConserve (_ transform transformer)
    val newself =
      if (transformedChildren eq children)
        this
      else
        genericConstructor(transformedChildren)
    transformer(newself).asInstanceOf[Exp[T]]
  }
  //This could use as interface some Foldable-like stuff (or Haskell's Traversable, IIRC).
  def treeMap[S](mapper: (Exp[_], Seq[S]) => S): S = {
    val mappedChilds = for (c <- children) yield c.treeMap(mapper)
    mapper(this, mappedChilds)
  }

  /* I renamed this method to avoid conflicts with Matcher.find. XXX test if
   * just making it private also achieves the same result.
   */
  def __find(filter: PartialFunction[Exp[_], Boolean]): Seq[Exp[_]] = {
    val baseSeq =
      if (PartialFunction.cond(this)(filter))
        Seq(this)
      else
        Seq.empty
    children.map(_ __find filter).fold(baseSeq)(_ ++ _)
  }

  // This overload is not called find because that would confuse type inference - it would fail to infer that filter's
  // domain type is Exp[_].
  def findTotFun(filter: Exp[_] => Boolean): Seq[Exp[_]] = __find(filter.asPartial)

  def isOrContains(e: Exp[_]): Boolean =
    (this findTotFun (_ == e)).nonEmpty

  def substSubTerm[S](SubTerm: Exp[_], e: Exp[S]) =
    transform {
      case SubTerm => e
      case exp => exp
    }

  def freeVars: Set[Var] = {
    def mapper(e: Exp[_], c: Seq[Set[Var]]): Set[Var] = e match {
      case v@Var(_) => Set(v)
      case fe@Fun(_) => c.fold(Set.empty)(_ union _).filter(!_.equals(fe.x))
      case _ => c.fold(Set.empty)(_ union _)
    }
    treeMap(mapper)
  }
  def toCode: String = ""
  def persistValues() { children foreach (_.persistValues()) }
}

object Exp {
  private def ordering[T] = new Ordering[Exp[T]] {
    override def compare(a: Exp[T], b: Exp[T]): Int = a.toString compareTo b.toString
  }
  def min[T](a: Exp[T], b: Exp[T]): Exp[T] = ordering.min(a,b)
  def max[T](a: Exp[T], b: Exp[T]): Exp[T] = ordering.max(a,b)
}
