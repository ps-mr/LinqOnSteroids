package ivm
package optimization

import expressiontree._

/**
 * User: pgiarrusso
 * Date: 22/5/2012
 */

/**
 * Represent a first-class extractor.
 *
 * Altered from https://github.com/Blaisorblade/scrap-expr-boilerplate/blob/master/src/traversal/Extractor.scala.
 * TODO: allow for some polymorphism (like natural transformations)?
 */
trait Extractor[-From, +To] {
  val fun: From => Option[To]
  def unapply(from: From): Option[To] = fun(from)
}

case class ConcreteExtractor[-From, +To](fun: From => Option[To])
  extends Extractor[From, To]

/**
 * An extractor binding no variables. This type is isomorphic to a predicate
 * From => Boolean; ConcreteBooleanExtractor witnesses one half of the isomorphism.
 */
trait BoolExtractor[-From] {
  val fun: From => Boolean
  def unapply(from: From): Boolean = fun(from)
}

case class ConcreteBoolExtractor[-From](fun: From => Boolean)
  extends BoolExtractor[From]

trait ExtractorCombinators {
  implicit def toExtractorOps[From, To](t: Extractor[From, To]) = new ExtractorOps(t)
  implicit def toBoolExtractorOps[From](t: BoolExtractor[From]) = new BoolExtractorOps(t)

  /**
   * Ease writing a small extractor.
   */
  def extractor[From, To](f: From => Option[To]): Extractor[From, To] = ConcreteExtractor(f)
  def boolExtractor[From](f: From => Boolean): BoolExtractor[From] = ConcreteBoolExtractor(f)

  def pfExtractor[From, To](pf: PartialFunction[From, To]): Extractor[From, To] = extractor(pf.lift)

  def toBoolExtractor[From, To](ext: Extractor[From, To]): BoolExtractor[From] = ConcreteBoolExtractor(toFilter(ext))

  def Success[T] = extractor[T, T](Some(_))
  def Fail[T] = extractor[T, T](_ => None)
  def BoolSuccess[T] = boolExtractor[T](_ => true)
  def BoolFail[T] = boolExtractor[T](_ => false)

  //Turn extractor into a predicate
  def toFilter[From, To](ext: Extractor[From, To]): From => Boolean = { x => (ext fun x).nonEmpty }

  def restrictedId[From, To](ext: Extractor[From, To]): Extractor[From, From] =
    Success[From] filter toFilter(ext)
}

object OptimizationUtil extends ExtractorCombinators {
  //TODO: simplify using pattern combinators, if possible.

  //Pattern-matchers for simplifying writing patterns
  object FuncExpBody {
    def unapply[S, T](f: Fun[S, T]): Option[Exp[T]] = Some(f.body)
  }

  //Reimplementation. Note: the type parameters move externally.
  def FuncExpBody2[S, T]: Extractor[Fun[S, T], Exp[T]] =
    Success[Fun[S, T]] map (_.body)

  object FuncExpBodyUntyped {
    def unapply(f: Fun[_, _]): Option[Exp[_]] = Some(f.body)
  }

  def FuncExpIdentity2[S, T](f: FunSym[S, T]) = toBoolExtractor(FuncExpBody2[S, T] filter (Sym(f.x) == _))

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

  /**
   * Find children matching a certain pattern. Without zippers, we cannot return the context, but we have our nice and expensive substSubTerm for that.
   * If there are no such children, fail the match.
   * Inspired by use cases for higher-order unification.
   */
  //def finder[B](a: Extractor[Exp[_], B]) = extractor[Exp[_], B](_ findTotFunGen (x => (a unapply x).nonEmpty))

  /*
   * (Incomplete XXX) Usage example. Assume:
  Exp[T]
  Eq[T](a, b: Exp[T]) <: Exp[Boolean]

  then you can write sth. like (untested):

  val A = finder(extractor(Eq.unapply _))
  e match {
    case A(eqs) =>
    for {
      (a, b) <- eqs
    } {}
  }
  */
  def finder[B](subExtractor: Extractor[Exp[Any], B]): Extractor[Exp[Any], Seq[B]] = extractor[Exp[Any], Seq[B]] {
    _.__foldr(Seq.empty[B])((cand, seq) => ((subExtractor unapply cand).toList ++ seq)) match {
      case Seq() => None
      case elems => Some(elems)
    }
  }

  def uniqueFinder[B](subExtractor: Extractor[Exp[Any], B]): Extractor[Exp[Any], Set[B]] = {
    val tfinder = finder(subExtractor)
    tfinder map (_.toSet)
    //extractor { x => tfinder unapply x map (_.toSet) }
  }

  //Instead of only returning unique subterms, also return the corresponding holes.
  // The implementation uses substitution, so it is probably horribly slow.
  // However, the interface is better (for me) than higher-order matching (not unification!)
  // It's better because it is guaranteed to find "maximal substitution",
  // that is, substitute away all the occurrences of a subterm.
  // But so, why does higher-order pattern unification not achieve what I want?

  def addContexts(tfinder: Extractor[Exp[Any], Set[Exp[Any]]]): Extractor[Exp[Any], Set[(Exp[Any], Exp[Any] => Exp[Any])]] =
    extractor { exp =>
      tfinder unapply exp map { matches =>
        matches map { subTerm =>
          (subTerm, { (replacement: Exp[Any]) =>
            exp substSubTerm (subTerm, replacement)
          })
        }
      }
    }

  //A closer approximation to the "perfect" type. Of course that's not correct because all needs to be more polytypic.
  //def uniqueFinderAndContexts[A, B](subExtractor: Extractor[Exp[A], Exp[B]]): Extractor[Exp[A], Set[(Exp[B], Exp[B] => Exp[A])]]
  def uniqueFinderAndContexts(subExtractor: Extractor[Exp[Any], Exp[Any]]) =
    addContexts(uniqueFinder(subExtractor))

  def guard(t: Boolean) = {
    assert(t)
    true
  }

  def uniqueMatcherAndContexts(v: Exp[Any]) = {
    val vFinder = uniqueFinder(matcher(v))
    //XXX This should arguably also be reusable.
    addContexts(vFinder) map { matches =>
      //Matches cannot be distinct, so the number of distinct matches must be one
      assert(matches.size == 1)
      matches.head
    }
  }

  def matcher[T](t: Exp[T]) = Success[Exp[T]] filter { _ == t }

  def expExtractor[To](f: Exp[Any] => Option[To]) = extractor(f)
  def pfExpExtractor[To](f: PartialFunction[Exp[Any], To]) = pfExtractor(f)
}

import OptimizationUtil._

class ExtractorOps[T, U](val t: Extractor[T, U]) extends AnyVal {
  //Extend combinators for Option[U] to Extractor[T, U] == (T =>) Option[U] = Reader T Option[U].
  def filter(p: U => Boolean) =
    extractor(t.fun andThen (_ filter p))
  def map[V](f: U => V) =
    extractor(t.fun andThen (_ map f))

  def flatMap[V](u: U => Option[V]): Extractor[T, V] =
    extractor(t.fun andThen (_ flatMap u))
  //In fact, flatMap lifts the composition for from Kleisli arrows T => Option[U] (known as >=>, among other names) to Extractor[T, U].
  def >=>[V](u: Extractor[U, V]): Extractor[T, V] = flatMap(u.fun)
}

class BoolExtractorOps[From](val ext: BoolExtractor[From]) extends AnyVal {
  //Replaces filter
  def intersect(p: From => Boolean) =
    boolExtractor[From](x => ext.fun(x) && p(x))
}

