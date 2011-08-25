package ivm.expressiontree

trait Exp[+T] {

  def interpret(): T
  private[ivm] def children: Seq[Exp[_]]
  //The arity is not specified.
  private[ivm] def genericConstructor: Seq[Exp[_]] => Exp[T]
  // some child management auxiliary functions
  private[ivm] def transform(transformer: Exp[_] => Exp[_]): Exp[T] = {
    val transformedChilds = for (c <- children) yield c.transform(transformer)
    val newself = genericConstructor(transformedChilds)
    transformer(newself).asInstanceOf[Exp[T]]
  }
  private[ivm] def map[S](mapper: (Exp[_], Seq[S]) => S): S = {
    val mappedChilds = for (c <- children) yield c.map(mapper)
    mapper(this, mappedChilds)
  }
  private[ivm] def containsExp[S](e: Exp[S]): Boolean = {
    var ac = allChildren
    ac.contains(e)
  }
  private[ivm] def isOrContains[S](e: Exp[S]): Boolean = if (this.equals(e)) true else containsExp(e)
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
    map(mapper)
  }

  //Methods for the clients of the library, rather than for the implementations.
  //They simply produce the appropriate expression tree nodes.
  //The implicits which are passed allow restricting the availability of these
  //methods only to appropriate T.
  //XXX: a problem is that in this encoding they all have to be defined within
  //Exp, if infix syntax is desired. An alternative encoding addressing this
  //problem is proposed in OpenEncoding.scala - PG
  def +[S >: T, Sum](that: Exp[S])(implicit sumT: Summable[S, Sum]) = sumT.plusNode(this, that)
  def <=[S >: T](that: Exp[S])(implicit ord: Ordering[S]) = LEq(this, that)
  def is[S >: T](that: Exp[S]): Exp[Boolean] = Eq(this, that)

  def &&[S >: T](that: Exp[S])(implicit ab : AsBool[S]) = And(ab(this),ab(that))
  def ||[S >: T](that: Exp[S])(implicit ab : AsBool[S]) = Or(ab(this),ab(that))
  def unary_![S >: T]()(implicit ab : AsBool[S]) = Not(ab(this))


  //def is(that: Exp[Int]): Exp[Boolean] = Eq(this, that)

  // method is used for testing. It is overriden in CallN to treat all calls as potentially equal
  private[ivm] def potentiallyEquals[S](other: Exp[S]) : Boolean = {
    val c  = children.zip(other.children)
    this.getClass().equals(other.getClass()) &&
      c.map({ case (a, b) => a.potentiallyEquals(b)}).forall(identity)
  }
}

private[ivm] object Exp {
  private def ordering[T] = new Ordering[Exp[T]] {
    override def compare(a: Exp[T], b: Exp[T]): Int = a.toString compareTo b.toString
  }
  def min[T](a: Exp[T], b: Exp[T]) : Exp[T] = ordering.min(a,b)
  def max[T](a: Exp[T], b: Exp[T]) : Exp[T] = ordering.max(a,b)
}

trait ChildlessExp[T] extends Exp[T] {
  def genericConstructor = _ => this
  def children = Seq()
}
