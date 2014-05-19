package ivm
package optimization

import expressiontree._


/**
 * User: pgiarrusso
 * Date: 22/5/2012
 */

object OptimizationUtil {
  //Pattern-matchers for simplifying writing patterns
  object FuncExpBody {
    def unapply[S, T](f: Fun[S, T]): Option[Exp[T]] = Some(f.body)
  }

  object FuncExpBodyUntyped {
    def unapply(f: Fun[_, _]): Option[Exp[_]] = Some(f.body)
  }

  object FuncExpIdentity {
    def unapply[S, T](f: FunSym[S, T]): Boolean = f.body == Sym(f.x)
  }

  //Pattern match to connect two conditions
  object & { def unapply[A](a: A) = Some((a, a)) }

  object AnyMapBinding {
    def unapply(e: Exp[_]): Option[(Exp[_], FunSym[_, _])] = e match {
      case Sym(FlatMap(base, f)) => Some((base, f))
      case Sym(MapNode(base, f)) => Some((base, f))
      case _ => None
    }
  }

  object BaseBindingRaw {
    def unapply(e: Sym[Traversable[Any]]): Option[(Exp[Traversable[Any]], FunSym[_, _])] = e match {
      case Sym(FlatMap(base, f)) => Some((base, f))
      case Sym(Filter(base, f)) => Some((base, f))
      case _ => None
    }
  }

  object BaseBinding {
    def unapply(e: Sym[Any]): Option[(Exp[Traversable[Any]], FunSym[_, _])] =
      BaseBindingRaw.unapply(e.asInstanceOf[Sym[Traversable[Any]]])
  }

  object BindingRaw {
    def unapply(e: Sym[Traversable[Any]]): Option[(Exp[Traversable[Any]], FunSym[_, _])] =
      BaseBinding.unapply(e) orElse (e match {
        case Sym(MapNode(base, f)) => Some((base, f))
        case _ => None
      })
  }

  object Binding {
    def unapply(e: Sym[_]): Option[(Exp[Traversable[Any]], FunSym[_, _])] =
      BindingRaw.unapply(e.asInstanceOf[Sym[Traversable[Any]]])
  }

  val FlatMapId = 0
  val FilterId = 1
  val MapNodeId = 2

  object BaseBindingWithT {
    def unapply(e: Exp[_]): Option[(Exp[_], FunSym[_, _], Int)] = e match {
      case Sym(FlatMap(base, f)) => Some((base, f, FlatMapId))
      case Sym(Filter(base, f)) => Some((base, f, FilterId))
      case _ => None
    }
  }
  object BindingWithT {
    def unapply(e: Exp[_]): Option[(Exp[_], FunSym[_, _], Int)] =
      BaseBindingWithT.unapply(e) orElse (e match {
        case Sym(MapNode(base, f)) => Some((base, f, MapNodeId))
        case _ => None
      })
  }
  def stripView[T](coll: Exp[Traversable[T]]) = stripViewUntyped(coll)

  //This type is incorrect whenever T is a view type. Be careful!
  def stripViewUntyped[T](coll: Exp[T]): Exp[T] =
    coll match {
      case Sym(View(coll2)) => coll2.asInstanceOf[Exp[T]]
      case _ => coll
    }

  def Transformer(f: PartialFunction[Exp[_], Exp[_]]) = f

  trait Extractor[From, To] {
    def unapply(from: From): Option[To]
  }

  /*
   * Altered from https://github.com/Blaisorblade/scrap-expr-boilerplate/blob/master/src/traversal/Extractor.scala.
   * This collapses the cost of writing small extractors.
   * TODO: add combinators!
   */
  def extractor[A, B](f: A => Option[B]) = new Extractor[A, B] { def unapply(x: A): Option[B] = f(x) }

  /**
   * Find children matching a certain pattern. Without zippers, we cannot return the context, but we have our nice and expensive substSubTerm for that.
   * If there are no such children, fail the match.
   * Inspired by use cases for higher-order unification.
   */
  //def finder[B](a: Extractor[Exp[_], B]) = extractor[Exp[_], B](_ findTotFunGen (x => (a unapply x).nonEmpty))
  def finder[B](subExtractor: Extractor[Exp[_], B]): Extractor[Exp[_], Seq[B]] = extractor[Exp[_], Seq[B]] {
    _.__foldr(Seq.empty[B])((cand, seq) => ((subExtractor unapply cand).toList ++ seq)) match {
      case Seq() => None
      case elems => Some(elems)
    }
  }

  def uniqueFinder[B](subExtractor: Extractor[Exp[_], B]): Extractor[Exp[_], Set[B]] = {
    val tfinder = finder(subExtractor)
    extractor { x => tfinder unapply x map (_.toSet) }
  }
}
