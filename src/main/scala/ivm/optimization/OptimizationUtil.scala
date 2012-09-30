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
    def unapply[S, T](f: FunSym[S, T]): Boolean = f.body == f.x
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

  object BaseBinding {
    def unapply(e: Exp[_]): Option[(Exp[_], FunSym[_, _])] = e match {
      case Sym(FlatMap(base, f)) => Some((base, f))
      case Sym(Filter(base, f)) => Some((base, f))
      case _ => None
    }
  }
  object Binding {
    def unapply(e: Exp[_]): Option[(Exp[_], FunSym[_, _])] =
      BaseBinding.unapply(e) orElse (e match {
        case Sym(MapNode(base, f)) => Some((base, f))
        case _ => None
      })
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
}
