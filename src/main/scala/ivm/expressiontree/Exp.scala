package ivm.expressiontree

import annotation.unchecked.uncheckedVariance

trait ExpTransformer {
  def apply[T](e: Exp[T]): Exp[T]
}

object ExpTransformer {
  def apply(f: Exp[_] => Exp[_]) = new ExpTransformer {
    def apply[T](e: Exp[T]): Exp[T] = f(e).asInstanceOf[Exp[T]]
  }
}

sealed trait TreeNode[+T, +MyType >: Exp[_]] {
  this: MyType =>
  //This method computes the contained value
  def interpret(): T
  def children: List[Exp[_]]
  def persistValues()

  /* I renamed this method to avoid conflicts with Matcher.find. XXX test if
  * just making it private also achieves the same result.
  */
  def __findGen(filter: PartialFunction[MyType, Boolean]): Seq[MyType] = {
    val baseSeq =
      if (PartialFunction.cond(this)(filter))
        Seq(this)
      else
        Seq.empty
    baseSeq ++ (children.flatMap(_ __find filter.asInstanceOf[PartialFunction[Exp[_], Boolean]]): Seq[MyType])
  }

  // This overload is not called find because that would confuse type inference - it would fail to infer that filter's
  // domain type is Exp[_].
  def findTotFunGen(filter: MyType => Boolean): Seq[MyType] = __findGen(filter.asPartial)

  //Using @uncheckedVariance here is safe: a subclass with a more specific MyType might be upcast and get an unexpected
  //value, but the code will work anyway since it is only passing e to Any's ==(Any) method.
  def isOrContainsGen(e: MyType @uncheckedVariance): Boolean =
    (this findTotFunGen (_ == e)).nonEmpty
}

sealed trait Exp[+T] extends TreeNode[T, Exp[_]] /*with MsgSeqPublisher[T, Exp[T]]*/ {
  def __find(filter: PartialFunction[Exp[_], Boolean]): Seq[Exp[_]] = __findGen(filter)
  def findTotFun(filter: Exp[_] => Boolean): Seq[Exp[_]] = findTotFunGen(filter)
  def isOrContains(e: Exp[_]): Boolean = isOrContainsGen(e)

  def toCode: String
  def transformImpl(transformer: ExpTransformer): Exp[T]
  def transform(f: Exp[_] => Exp[_]) = transformImpl(ExpTransformer(f))

  //This method returns the cached value (if any) or invokes interpret().
  def value(): T = interpret()

  //This could use as interface some Foldable-like stuff (or Haskell's Traversable, IIRC).
  //Write it as a fold on the tree!!!
  def treeMap[S](mapper: (Exp[_], Seq[S]) => S): S = {
    //Worked in 2.10.0, not in 2.10.1 or 2.10.2 - now it does again!
    //val mappedChilds = for (c <- children) yield c.treeMap(mapper)
    // Help a bit type inference:
    //val mappedChilds = for (c <- children) yield c.treeMap[S](mapper)
    // Doesn't give additional help.
    //val mappedChilds: List[S] = for (c <- children) yield c.treeMap[S](mapper)
    // This tells that no implicit is found:
    //val mappedChilds: List[S] = children.map[S, List[S]](c => c.treeMap[S](mapper))
    //Also doesn't work:
    //val mappedChilds = for (c <- children) yield (c: Exp[_]).asInstanceOf[Exp[Any]].treeMap[S](mapper)

    //Doesn't work.
    //val mappedChilds: List[S] = children.asInstanceOf[List[Exp[Any]]].map[S, List[S]](c => c.asInstanceOf[Exp[Any]].treeMap[S](mapper))(List.canBuildFrom)
    //Works in 2.10.1! (Maybe in 2.10.2).
    val mappedChilds: List[S] = children.asInstanceOf[List[Exp[Any]]].map[S, List[S]](c => c.asInstanceOf[Exp[Any]].treeMap[S](mapper))(List.canBuildFrom[S])

    //After compiling with that line, we can use pretty much any definition and
    //it will work. That is, we can comment out the working definition, comment
    //back in the one which worked in 2.10.0 (or many other ones, but I didn't
    //test each of them), and that one will compile. At least in 2.10.1.
    //
    //Yeah, that's crazy, yet reproducible on this computer. And compiling a
    //file separate from its codebase is known to work differently. Usually not
    //up to this point though.

    // This is another of the many examples of code which works after the first compilation.
    //val mappedChilds = children.asInstanceOf[List[Exp[Any]]].map(c => c.treeMap(mapper))
    mapper(this, mappedChilds)
  }

  def substSubTerm[S](SubTerm: Exp[_], e: Exp[S]) =
    this transform {
      case SubTerm => e
      case exp => exp
    }

  def freeVars: Set[Var] = {
    def mapper(e: Exp[_], c: Seq[Set[Var]]): Set[Var] = e match {
      case Sym(v@Var(_)) => Set(v)
      case FunSym(fe@Fun(_)) => c.fold(Set.empty)(_ union _).filter(!_.equals(fe.x))
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

trait Def[+T] extends TreeNode[T, TreeNode[_, _]] {
  def nodeArity: Int

  def persistValues() { children foreach (_.persistValues()) }
  def children: List[Exp[_]]
  protected def checkedGenericConstructor(v: List[Exp[_]]): Def[T]

  def genericConstructor(v: List[Exp[_]]): Def[T] =
    if (v.length == nodeArity)
      checkedGenericConstructor(v)
    else
      throw new IllegalArgumentException()
  def toCode: String = ???
}

object Sym {
  val gensymId = new Util.ThreadLocalIDGenerator
}
object SymWithId {
  def unapply[T](s: Sym[T]): Some[(Def[T], Int)] = {
    Some((s.defNode, s.id))
  }
}
case class Sym[+T](defNode: Def[T]) extends Exp[T] {
  val id: Int = Sym.gensymId()
  def toCode: String = defNode.toCode
  def interpret() = defNode.interpret()
  def children = defNode.children
  def persistValues() { defNode persistValues () }
  def transformImpl(transformer: ExpTransformer): Exp[T] = {
    val transformedChildren = children mapConserve (_ transformImpl transformer)
    val newSelf: Exp[T] =
      if (transformedChildren eq children)
        this
      else
        defNode genericConstructor transformedChildren
    transformer(newSelf)
    //transformer(d.genericConstructor(children mapConserve (_ transformImpl transformer)))
  }
}
class FunSym[-S, +T](override val defNode: Fun[S, T]) extends Sym[S => T](defNode) {
  def x = defNode.x
  def body = defNode.body
  def f = defNode.f
  override def productPrefix = "FunSym"
}
object FunSym {
  def apply[S, T](d: Fun[S, T]) = new FunSym(d)
  def unapply[S, T](s: FunSym[S, T]): Some[Fun[S, T]] = Some(s.defNode)
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
          s"Instance of ${coll getClass ()}, ID = ${System.identityHashCode(coll)}"
          //coll.take(3).toString() + (if (coll.size > 3) "..." else "")
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
