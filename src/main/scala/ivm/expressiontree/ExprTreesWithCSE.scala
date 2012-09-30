package ivm
package expressiontree

import collection.mutable

/**
 * User: pgiarrusso
 * Date: 28/9/2012
 */
object ExprTreesWithCSE {
  trait ExpTransformer {
    def apply[T](e: Exp[T]): Exp[T]
  }
  object ExpTransformer {
    implicit def apply(f: Exp[_] => Exp[_]) = new ExpTransformer {
      def apply[T](e: Exp[T]): Exp[T] = f(e).asInstanceOf[Exp[T]]
    }
  }

  trait TreeNode[+T] {
    //This method computes the contained value
    def interpret(): T
    def children: List[Exp[_]]
    def persistValues() { children foreach (_.persistValues()) }
  }

  sealed trait Exp[+T] extends TreeNode[T] {
    def transform(transformer: ExpTransformer): Exp[T]

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
      this transform ExpTransformer{{
        case SubTerm => e
        case exp => exp
      }}

    def freeVars: Set[Var] = {
      def mapper(e: Exp[_], c: Seq[Set[Var]]): Set[Var] = e match {
        case v@Var(_) => Set(v)
        case fe@Fun(_) => c.fold(Set.empty)(_ union _).filter(!_.equals(fe.x))
        case _ => c.fold(Set.empty)(_ union _)
      }
      treeMap(mapper)
    }
  }
  def definitions: mutable.Map[Def[_], Sym[_]] = new mutable.HashMap()
  implicit def toAtom[T](d: Def[T]): Exp[T] =
    definitions.asInstanceOf[mutable.Map[Def[T], Sym[T]]].getOrElseUpdate(d, Sym[T](d))//.asInstanceOf[Sym[T]]

  object Sym {
    private val gensymId: () => Int = new Util.GlobalIDGenerator
  }
  case class Sym[+T] private[ExprTreesWithCSE](d: Def[T], id: Int = Sym.gensymId()) extends Exp[T] {
    def interpret() = d.interpret()
    def children = d.children
    def transform(transformer: ExpTransformer): Exp[T] = {
      val transformedChildren = children mapConserve (_ transform transformer)
      val newSelf: Exp[T] =
        if (transformedChildren eq children)
          this
        else
          d genericConstructor transformedChildren
      transformer(newSelf)
      //transformer(d.genericConstructor(children mapConserve (_ transform transformer)))
    }
  }
  case class Const[T](t: T)(implicit val cTag: ClassTag[T], val tTag: TypeTag[T]) extends Exp[T] {
    def interpret() = t
    def children: List[Exp[_]] = Nil
    def transform(transformer: ExpTransformer): Exp[T] = this
    override def toString = expressiontree.Const toString (t, productPrefix)
  }

  trait Def[+T] extends TreeNode[T] {
    def nodeArity: Int

    def children: List[Exp[_]]
    protected def checkedGenericConstructor(v: List[Exp[_]]): Def[T]

    def genericConstructor(v: List[Exp[_]]): Def[T] =
      if (v.length == nodeArity)
        checkedGenericConstructor(v)
      else
        throw new IllegalArgumentException()
    def toCode: String = ""
  }
  val transf: Exp[_] => Exp[_] = {
    case e => e
  }
  //Test that this code compiles:
  def tr[T](e: Exp[T]) = e transform transf
  def compile[T](e: Exp[T]): String = {
    val symDecls = e __find {
      case Sym(_, _) => true
    } map {
      case Sym(defNode, id) => s"val s${id} = ${defNode.toCode}"
      case _ => throw new Throwable()
    } mkString ("\n")
    val constlessE = e transform {
      case c: Const[_] =>
        CrossStagePersistence.persist(c.x)(c.cTag, c.tTag)
      case Sym(_, id) => NamedVar("s" + id)
    }

    val body = ""
    s"""{
    |  ${symDecls}
    |  ${body}
    |}""".stripMargin
  }
}
