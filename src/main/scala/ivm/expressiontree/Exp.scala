package ivm.expressiontree

/*
trait Exp[+T] /*extends MsgSeqPublisher[T, Exp[T]]*/ {
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
*/
trait ExpTransformer {
  def apply[T](e: Exp[T]): Exp[T]
}
object ExpTransformer {
  def apply(f: Exp[_] => Exp[_]) = new ExpTransformer {
    def apply[T](e: Exp[T]): Exp[T] = f(e).asInstanceOf[Exp[T]]
  }
}

trait TreeNode[+T] {
  //This method computes the contained value
  def interpret(): T
  def children: List[Exp[_]]
  def persistValues()
}

sealed trait Exp[+T] extends TreeNode[T] /*with MsgSeqPublisher[T, Exp[T]]*/ {
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

  def toCode: String
  def transformImpl(transformer: ExpTransformer): Exp[T]
  def transform(f: Exp[_] => Exp[_]) = transformImpl(ExpTransformer(f))

  //This method returns the cached value (if any) or invokes interpret().
  def value(): T = interpret()

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
    this transform {
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
}

object Exp {
  private def ordering[T] = new Ordering[Exp[T]] {
    override def compare(a: Exp[T], b: Exp[T]): Int = a.toString compareTo b.toString
  }
  def min[T](a: Exp[T], b: Exp[T]): Exp[T] = ordering.min(a,b)
  def max[T](a: Exp[T], b: Exp[T]): Exp[T] = ordering.max(a,b)
}

trait Def[+T] extends TreeNode[T] {
  def nodeArity: Int

  def persistValues() { children foreach (_.persistValues()) }
  def children: List[Exp[_]]
  protected def checkedGenericConstructor(v: List[Exp[_]]): Def[T]

  def genericConstructor(v: List[Exp[_]]): Def[T] =
    if (v.length == nodeArity)
      checkedGenericConstructor(v)
    else
      throw new IllegalArgumentException()
  def toCode: String = ""
}

object Sym {
  private val gensymId: () => Int = new Util.ThreadLocalIDGenerator
}
object SymWithId {
  def unapply[T](s: Sym[T]): Some[(Def[T], Int)] = {
    Some((s.d, s.id))
  }
}
case class Sym[+T](d: Def[T]) extends Exp[T] {
  val id: Int = Sym.gensymId()
  def toCode: String = d.toCode
  def interpret() = d.interpret()
  def children = d.children
  def persistValues() { d persistValues () }
  def transformImpl(transformer: ExpTransformer): Exp[T] = {
    val transformedChildren = children mapConserve (_ transformImpl transformer)
    val newSelf: Exp[T] =
      if (transformedChildren eq children)
        this
      else
        d genericConstructor transformedChildren
    transformer(newSelf)
    //transformer(d.genericConstructor(children mapConserve (_ transformImpl transformer)))
  }
}
class FunSym[-S, +T](override val d: Fun[S, T]) extends Sym[S => T](d) {
  def x = d.x
  def body = d.body
  def f = d.f
}
object FunSym {
  def apply[S, T](d: Fun[S, T]) = new FunSym(d)
  def unapply[S, T](s: FunSym[S, T]): Some[Fun[S, T]] = Some(s.d)
}

case class Const[T](x: T)(implicit val cTag: ClassTag[T], val tTag: TypeTag[T]) extends Exp[T] {
  import Const._

  def interpret() = x
  def children: List[Exp[_]] = Nil
  def persistValues() {}
  def transformImpl(transformer: ExpTransformer): Exp[T] = transformer(this)
  override def toString = Const toString (x, productPrefix)
  def toCode = throw new RuntimeException("Const.toCode should never be called")
}

object Const {
  private val maxInlineStringLength = 10
  val allowInlineInEval = false

  def toString[T](x: T, productPrefix: String): String = {
    val s =
      x match {
        //Printing all elements and then cutting the output is horribly expensive for huge collections, so try to avoid it.
        //Of course, this does not work when x is not a collection but e.g. contains one, or when for any reason toString()
        //takes a lot of time for any reason. Still, better than nothing.
        case coll: Traversable[_] =>
          coll.take(3).toString() + (if (coll.size > 3) "..." else "")
        case s: String if s.length < maxInlineStringLength =>
          "\"%s\"" format s
        case c: Char =>
          "'%c'" format c
        case _ =>
          String valueOf x
      }
    val shortened =
      if (s.length() > 100) {
        val begin = s take 100
        val quoteCloser = if (begin.contains('"')) "\"" else if (begin.contains('\'')) "'" else ""
        begin + "..." + quoteCloser + ")" * (begin.count(_  == '(') - begin.count(_ == ')'))
      }
      else
        s
    productPrefix + "(" + shortened + ")"
  }
}
