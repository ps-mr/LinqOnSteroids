package ivm.optimization

import ivm.expressiontree._


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
    def unapply[S, T](f: Fun[S, T]): Boolean = f.body == f.x
  }

  //Pattern match to connect two conditions
  object & { def unapply[A](a: A) = Some(a, a) }

  object BaseBinding {
    def unapply(e: Exp[_]): Option[(Exp[_], Fun[_, _])] = e match {
      case FlatMap(base, f) => Some((base, f))
      case Filter(base, f) => Some((base, f))
      case _ => None
    }
  }
  object Binding {
    def unapply(e: Exp[_]): Option[(Exp[_], Fun[_, _])] =
      BaseBinding.unapply(e) orElse (e match {
        case MapNode(base, f) => Some((base, f))
        case _ => None
      })
  }
}
