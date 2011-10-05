package ivm.expressiontree

trait Exp[+T] {
  def interpret(): T
  private[ivm] def children: Seq[Exp[_]]
  private[ivm] def closedTermChildren: Seq[Exp[_]] = children
  //The arity is not specified.
  private[ivm] def genericConstructor: Seq[Exp[_]] => Exp[T]
  // some child management auxiliary functions

  //Many visitors might not want to visit childrens of FuncExp, i.e. function bodies, because they are open terms.
  private[ivm] def visitPreorderClosedChildren(visitor: Exp[_] => Unit) {
    visitor(this)
    for (c <- closedTermChildren) {
      c.visitPreorderClosedChildren(visitor)
    }
  }
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

  private[ivm] def substVar[S](v: String, e: Exp[S]) =
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
  def is[S >: T](that: Exp[S]): Exp[Boolean] = Eq(this, that)
  //def is(that: Exp[T]): Exp[Boolean] = Eq(this, that)

}

private[ivm] object Exp {
  private def ordering[T] = new Ordering[Exp[T]] {
    override def compare(a: Exp[T], b: Exp[T]): Int = a.toString compareTo b.toString
  }
  def min[T](a: Exp[T], b: Exp[T]) : Exp[T] = ordering.min(a,b)
  def max[T](a: Exp[T], b: Exp[T]) : Exp[T] = ordering.max(a,b)
}
