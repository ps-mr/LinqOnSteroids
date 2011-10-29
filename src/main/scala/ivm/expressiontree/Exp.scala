package ivm.expressiontree

trait Exp[+T] extends MsgSeqPublisher[T] {
  type Pub <: Exp[T]
  type RootType
  private[ivm] def activateIVM() {}

  //XXX: does this really belong here?
  private[ivm] def pullAndPropagateContent() {}
  private[ivm] def isRoot = roots.isEmpty

  def interpret(): T
  private[ivm] def children: Seq[Exp[_]]
//  private[ivm] def closedTermChildren: Seq[Exp[_]] = children
  private[ivm] def roots: Seq[Exp[RootType]] = Seq.empty
  //The arity is not specified.
  private[ivm] def genericConstructor: Seq[Exp[_]] => Exp[T]
  // some child management auxiliary functions

  private[ivm] def visitPreorder(visitor: Exp[_] => Unit, childSelector: Exp[_] => Seq[Exp[_]]) {
    visitor(this)
    for (c <- childSelector(this)) {
      c.visitPreorder(visitor, childSelector)
    }
  }
  private[ivm] def visitPreorderRoots(visitor: Exp[_] => Unit) = visitPreorder(visitor, _.roots)

  private[ivm] def transform(transformer: Exp[_] => Exp[_]): Exp[T] = {
    val transformedChilds = for (c <- children) yield c.transform(transformer)
    val newself = genericConstructor(transformedChilds)
    transformer(newself).asInstanceOf[Exp[T]]
  }
  private[ivm] def treeMap[S](mapper: (Exp[_], Seq[S]) => S): S = {
    val mappedChilds = for (c <- children) yield c.treeMap(mapper)
    mapper(this, mappedChilds)
  }
  private[ivm] def containsExp[S](e: Exp[S]): Boolean = {
    var ac = allChildren //XXX slow, allChildren computes an eager sequence!
    ac.contains(e)
  }
  //Alternative implementation not using allChildren - and thus probably faster:
  //def containsExp[S](e: Exp[S]): Boolean = children.map(_.isOrContains(e)).foldRight(false)(_ || _)

  private[ivm] def isOrContains(e: Exp[_]): Boolean = if (this.equals(e)) true else containsExp(e)
  private[ivm] def allChildren: Seq[Exp[_]] = children ++ (for (c <- children; a <- c.allChildren) yield a)

  private[ivm] def substVar[S](v: Int, e: Exp[S]) =
    transform((exp) => exp match {
      case Var(x) => if (x.equals(v)) e else exp
      case _ => exp
    })

  private[ivm] def freeVars: Set[Var] = {
    val mapper : (Exp[_], Seq[Set[Var]]) => Set[Var] = (e,c) => e match {
      case v@Var(_) => Set(v)
      case fe@FuncExp(_) => c.fold(Set.empty)(_ union _).filter(!_.equals(fe.x))
      case _ => c.fold(Set.empty)(_ union _)
    }
    treeMap(mapper)
  }

  //Methods for the clients of the library, rather than for the implementations.
  //They simply produce the appropriate expression tree nodes.
  final def is[S >: T](that: Exp[S]): Exp[Boolean] = Eq(this, that)
  final def is(that: Null): Exp[Boolean] = Eq(this, Const(null))

  final def ===[S >: T](that: Exp[S]): Exp[Boolean] = Eq(this, that)
  final def ===(that: Null): Exp[Boolean] = Eq(this, Const(null))

  final def !==[S >: T](that: Exp[S]): Exp[Boolean] = Not(this === that)
  final def !==(that: Null): Exp[Boolean] = Not(this === that)

  def ifInstanceOf[S:ClassManifest] : Exp[Option[S]] = IfInstanceOf(this)

}

private[ivm] object Exp {
  private def ordering[T] = new Ordering[Exp[T]] {
    override def compare(a: Exp[T], b: Exp[T]): Int = a.toString compareTo b.toString
  }
  def min[T](a: Exp[T], b: Exp[T]) : Exp[T] = ordering.min(a,b)
  def max[T](a: Exp[T], b: Exp[T]) : Exp[T] = ordering.max(a,b)
}
