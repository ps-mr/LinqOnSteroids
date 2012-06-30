package ivm
package optimization

import expressiontree._
import Lifting._
import annotation.tailrec
import OptimizationUtil._
import Subst.subst

/**
 * User: pgiarrusso
 * Date: 30/6/2012
 */

trait SimplificationsOptimTransforms {
  val removeTrivialFilters: Exp[_] => Exp[_] = {
    case Filter(coll, FuncExpBody(Const(true))) =>
      stripViewUntyped(coll)
    case e => e
  }

  //Move constants on the right-side of a boolean connective. Also, move nested connectives of the same type to the
  //left: since these operators are left-associative, that's the natural order.
  private val reassociateBoolOps: Exp[_] => Exp[_] = {
    case And(l@Const(_), r) =>
      r && l
    case And(l, r@And(ra, rb)) =>
      (l && ra) && rb
    case Or(l@Const(_), r) =>
      r || l
    case Or(l, r@Or(ra, rb)) =>
      (l || ra) || rb
    case e => e
  }

  //Some cases of constant folding for booleans.
  //The code could be optimized to save repeated matches on And and Or in the different functions, but that seems premature.
  val simplifyConditions: Exp[_] => Exp[_] =
    reassociateBoolOps andThen {
      case And(x, Const(true)) => x
      case And(x, c@Const(false)) => c
      case Or(x, Const(false)) => x
      case Or(x, c@Const(true)) => c
      case Not(Not(c)) => c
      case e => e
    }

  val removeIdentityMaps: Exp[_] => Exp[_] = {
    case MapNode(col, FuncExpIdentity()) =>
      col
    case e => e
  }

  /*
  private def removeIdentityMaps[T](e: Exp[T]): Exp[T] =
    e match {
      //Alternative 1 - a cast is required:
      case MapNode(col: Exp[_ /*T*/], FuncExpIdentity()) =>
        col.asInstanceOf[Exp[T]]
      //Alternative 2 - causes a warning, but works and is more elegant:
      case MapNode(col: Exp[T], FuncExpIdentity()) =>
        col
      //Possibility 2 is what is used in the Scala-virtualized tutorial.
      case e => e
    }
  */

  /*
   * Thanks for the idea to Tillmann Rendel - this optimization was one of his insightful side remarks.
   * His actual comment was that this optimization (which he assumed to be done) introduces the possibility of
   * non-termination if interpret() is ever non-terminating.
   * Notice that constant-folding on one level produces a new constant which might enable constant folding on the
   * next level, and this is handled automatically since the visit is bottom-up.
   */
  val constantFolding: Exp[_] => Exp[_] = {
    //Exclude from optimization Fun nodes: they can't be replaced by equivalent Const nodes containing opaque functions,
    //because that removes information, and because many parent nodes require specifically Fun nodes. Any other node
    //type T <: Exp[U] can be replaced by an equivalent instance of Exp[U].
    case f: Fun[_, _] => f
    case e =>
      //Avoid constant-folding for nullary expressions - if they are not Const nodes, there must be a good reason!
      //For instance, this catches Var, ConstByIdentity, but also incremental collections.
      //XXX: there might be other classes where constant-folding is a bad idea. Keep that in mind.
      if (e.children.nonEmpty && e.children.forall(_ match {
        case Const(_) => true
        case _ => false
      }))
        Const(e.interpret())
      else
        e
  }

  //This is used only at the end, to convert the remaining Let()s into function applications; they are however
  //_not_ beta-reduced, so that computation is not duplicated.
  val letTransformer: Exp[_] => Exp[_] = {
    case FlatMap(ExpSeq(Seq(v)), f) => letExp(v)(f)
    case e => e
  }

  val deltaReductionTuple: PartialFunction[Exp[_], Exp[_]] = {
    case ExpSelection(arity, selected, e: ExpProduct) =>
      e.metaProductElement(selected - 1)
  }

  val betaReduction: PartialFunction[Exp[_], Exp[_]] = {
    //case a: App[s, t] => a.f(a.t) //causes a warning
    //To ensure termination, this must only apply if this rule changes behavior, that is, if App contains a Fun!
    //Otherwise fToFunOps will recreate a new App node.
    case appNode@App(fun: Fun[_, _], arg)
      //if ((body findTotFun (_ == fun.x)).length == 1) //Inlining side conditions. Damn, we need to use unrestricted inlining as here, simplify, and then use CSE again,
      //to have a robust solution.
    =>
      subst(fun)(arg)
  }

  //This is the shortest way of writing identity.
  val emptyTransform: PartialFunction[Exp[_], Exp[_]] = {
    case e => e
  }

  // Fixpoint iteration for beta-delta reduction :-). This works very well because:
  //   (f andThen g).isDefinedAt = f.isDefinedAt. (and g is supposed to be total - this code won't work so well if g is
  // in fact partial, another example of PartialFunction <: Function1 being totally confusing).

  // The recursion scheme here is remarkably similar to the one used with parser combinators:
  //   statements = (statement ~ statements) | empty
  // which can be shortened (IIRC) to:
  //   statements = statement*
  // Remember that Scala parser combinators are applicative parsers (i.e. Applicative and Alternative). Also note that
  // Alternative is described as "A monoid on applicative functors". Which nowadays is even obvious to me, oh my God!
  // Are my transformers an instance of the same structure?
  //Note: there's a reason parser combinators take their arguments by value; partial function combinators don't, hence we
  //suffer a lot
  //Note: recursive vals don't work - a recursive definition like this requires def.
  //val betaDeltaReducer: Exp[_] => Exp[_] = (deltaReductionTuple orElse betaReduction) andThen betaDeltaReducer orElse {case e => e} //that's the shortest way of writing identity.
  //This recursive def does not work either - upon a call it tries to unfold itself.
  //def betaDeltaReducer: Exp[_] => Exp[_] = (deltaReductionTuple orElse betaReduction) andThen betaDeltaReducer orElse {case e => e} //that's the shortest way of writing identity.
  //This one always terminates, as long as the involved PartialFunctions are only defined when they do transform their input.
  //TODO: write a Kleene star to encapsulate this pattern; then lookup under what name is this technique already known.
  def betaDeltaReducer(exp: Exp[_]): Exp[_] = ((deltaReductionTuple orElse betaReduction) andThen betaDeltaReducer orElse emptyTransform)(exp)
}
