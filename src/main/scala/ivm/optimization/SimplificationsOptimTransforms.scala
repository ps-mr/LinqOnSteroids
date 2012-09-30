package ivm
package optimization

import expressiontree._
import Lifting._
import OptimizationUtil._
import Subst.subst

/**
 * User: pgiarrusso
 * Date: 30/6/2012
 */

trait SimplificationsOptimTransforms {
  this: Inlining =>
  val ifSimplify: Exp[_] => Exp[_] = {
    case Sym(IfThenElse(x @ Var(_), thenBranch, elseBranch)) if (thenBranch isOrContains x) || (elseBranch isOrContains x) =>
      val thenBranchSub = thenBranch substSubTerm (x, true)
      val elseBranchSub = elseBranch substSubTerm (x, false)
      if_#(x) { thenBranchSub } else_# { elseBranchSub }
    case e => e
  }

  //Valid but unneeded
  val foldRedundantIf: Exp[_] => Exp[_] = {
    case Sym(IfThenElse(x, Const(true), Const(false))) => x
    case e => e
  }

  val simplifyForceView: Exp[_] => Exp[_] = Transformer {
    case Sym(View(Sym(Force(coll)))) => coll
    case Sym(Force(Sym(View(coll)))) => coll
  } orElse (Transformer {
    //Fusion
    case Sym(Force(Sym(coll @ Force(_)))) => coll
    case Sym(View(Sym(coll @ View(_)))) => coll

    case e => e
  })

  val removeTrivialFilters: Exp[_] => Exp[_] = {
    case Sym(Filter(coll, FunSym(FuncExpBody(Const(true))))) =>
      stripViewUntyped(coll)
    case e => e
  }

  //Move constants on the right-side of a boolean connective. Also, move nested connectives of the same type to the
  //left: since these operators are left-associative, that's the natural order.
  val reassociateBoolOps: Exp[_] => Exp[_] = {
    case Sym(And(l@Const(_), r)) =>
      r && l
    case Sym(And(l, r@Sym(And(ra, rb)))) =>
      (l && ra) && rb
    case Sym(Or(l@Const(_), r)) =>
      r || l
    case Sym(Or(l, r@Sym(Or(ra, rb)))) =>
      (l || ra) || rb
    case e => e
  }

  //Some cases of constant folding for booleans.
  //The code could be optimized to save repeated matches on And and Or in the different functions, but that seems premature.
  val simplifyConditions: Exp[_] => Exp[_] =
    reassociateBoolOps andThen {
      case Sym(And(x, Const(true))) => x
      case Sym(And(x, c@Const(false))) => c
      case Sym(Or(x, Const(false))) => x
      case Sym(Or(x, c@Const(true))) => c
      case Sym(Not(Sym(Not(c)))) => c
      case Sym(Not(Const(b))) => !b
      case e => e
    }

  val removeIdentityMaps: Exp[_] => Exp[_] = {
    case Sym(MapNode(col, FuncExpIdentity())) =>
      col
    case e => e
  }

  /*
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
  */

  //This is used only at the end, to convert the remaining Let()s into function applications; they are however
  //_not_ beta-reduced, so that computation is not duplicated.
  val letTransformer: Exp[_] => Exp[_] = {
    case Sym(FlatMap(Sym(ExpSeq(Seq(v))), f)) => letExp(v)(f)
    case e => e
  }

  val deltaReductionTuple: PartialFunction[Exp[_], Exp[_]] = {
    case ExpSelection(arity, selected, e: ExpProduct) =>
      e.metaProductElement(selected - 1)
  }

  val deltaReductionIsEmpty: PartialFunction[Exp[_], Exp[_]] = {
    case Sym(IsEmpty(Sym(ExpSeq(seq)))) => seq.isEmpty
  }

  val betaReduction: PartialFunction[Exp[_], Exp[_]] = {
    //To ensure termination, this must only apply if this rule changes behavior, that is, if App contains a Fun!
    //Otherwise fToFunOps will recreate a new App node.
    case Sym(appNode@App(fun: FunSym[_, _], arg)) if isTrivial(arg) || usesArgAtMostOnce(fun)
      //if ((body findTotFun (_ == fun.x)).length == 1) //Inlining side conditions. Damn, we need to use unrestricted inlining as here, simplify, and then use CSE again,
      //to have a robust solution.
    =>
      subst(fun.d)(arg)
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
  //Note: there's a reason parser combinators take their arguments by name; partial function combinators don't, hence we
  //suffer a lot
  //Note: recursive vals don't work and cause NullPointerExceptions - a recursive definition like this requires def.
  //val betaDeltaReducer: Exp[_] => Exp[_] = (deltaReductionTuple orElse betaReduction) andThen betaDeltaReducer orElse {case e => e} //that's the shortest way of writing identity.
  //This recursive def does not work either - upon a call it tries to unfold itself.
  //def betaDeltaReducer: Exp[_] => Exp[_] = (deltaReductionTuple orElse betaReduction) andThen betaDeltaReducer orElse {case e => e} //that's the shortest way of writing identity.
  //This one always terminates, as long as the involved PartialFunctions are only defined when they do transform their input.
  //TODO: write a Kleene star to encapsulate this pattern; then lookup under what name is this technique already known.
  def betaDeltaReducer(exp: Exp[_]): Exp[_] = ((deltaReductionTuple orElse deltaReductionIsEmpty orElse betaReduction) andThen betaDeltaReducer orElse emptyTransform)(exp)
}
