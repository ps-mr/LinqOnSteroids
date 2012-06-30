package ivm
package optimization

import expressiontree._
import Lifting._
import annotation.tailrec
import OptimizationUtil._

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


  def isTrivial(v: Exp[_]): Boolean = v match {
    case ExpSelection(_, _, e) => isTrivial(e)
    case e: ExpProduct with Exp[_] => e.children forall isTrivial
    case _: TypedVar[_] => true
    case _ => false
  }

  def subst[S, T](fun: Fun[S, T])(arg: Exp[S]) = {
    //See TAPL page 75 - we need this transformation whenever we might duplicate terms. XXX check if other
    // transformations duplicate terms. Map fusion uses letExp, to allow for a smarter inliner - I hope I was
    // consistent in doing this.
    fun(arg) transform {
      case fun: Fun[_, _] => Fun.rename(fun)
      case e => e
    }
  }

  //XXX: also reduce lets where the variable is only used once. Papers on the GHC inliner explain how to do this and why
  //a linear type system is needed for that.
  val letTransformerTrivial: Exp[_] => Exp[_] = {
    case FlatMap(ExpSeq(Seq(v)), f) if isTrivial(v) => subst(f)(v)
    case e => e
  }

  //Bug-triggering variant of the code below.
  /*@tailrec private def usesArgAtMostOnce[S, T](f: Fun[S, T], v: Exp[_]): Boolean = {
    f match {
      case FuncExpBody(FlatMap(ExpSeq(Seq(v2)), g)) if !v2.isOrContains(v) =>
        usesArgAtMostOnce(g, v)
      case FuncExpBody(FlatMap(baseUsingV, g)) =>
        val occurrences = baseUsingV.findTotFun(_ == v)
        occurrences.length == 1 //gives spurious error
        //false
      case _ => false
    }
  }*/

  /*
   * Inlining: if you bind something and then use it immediately (1), it's used only once; the same if there are in between only other bindings, i.e. iterations over single-element sequences (try to make this into a complete criterion). Example:
for {
a <- Let(…)
b <- f(a)
…
}
In other words: FlatMap(Seq(a), x => FlatMap(f(a), g)) and combinations with Filter instead of FlatMap.
Let's assume that our binding of interest is bound in the head of a comprehension - otherwise it might be used more than once, but only
once per bound value. So let's consider
FlatMap(a, x => f(x)).
We want to characterize the subterms of f which are only evaluated once; in addition, we need that our variable appears in only one of them.
Theorem: if and only if a variable bound in a for-comprehension (using only FlatMap) is used at most once, then and only then it is used either in the
   */

  private def usesArgAtMostOnceNotUnderLambda(exp: Exp[_], v1: Exp[_]): Boolean =
    exp.findTotFun(_ == v1).length <= 1 &&
      (exp.find {
        case f: Fun[_, _] => true
      } forall (!_.isOrContains(v1)))

  //Problem here: this assumes that the variable does not appear under explicit lambdas
  @tailrec private def usesArgAtMostOnce(f: Fun[_, _], v1: Exp[_]): Boolean = {
    f match {
      case FuncExpBody(Binding(ExpSeq(Seq(exp2)), g)) if !exp2.isOrContains(v1) =>
        //This allows to inline id1 in cases like:
        //v1 <- Let(exp1)
        //v2 <- Let(exp2) //v1 not free in exp2, that is, !v2.isOrContains(v1)
        //v3 <- Let(exp3) //v1 free in exp3, but not used any more; or v1 not free in exp3 but free in exp4; and so on.
        //v4 <- Let(exp4)
        //Note that it is crucial that all bindings act on single-element collections; we check that by looking for when this is statically known, i.e. we look for ExpSeq(Seq(_)), but we might want to extend that to
        //Const(seq) if seq has "at runtime" just one element.
        usesArgAtMostOnce(g, v1)
      case FuncExpBody(baseUsingV) =>
        //Let's assume that unnesting has already happened, hence all functions we find are just function definitions and not nested FlatMaps to further analyze.
        //Since functions might be applied multiple times, we just make sure that nested function definitions do not refer to g.
        usesArgAtMostOnceNotUnderLambda(baseUsingV, v1)
      case _ => false //XXX add corresponding cases for Filter. Or add a common pattern-matcher encompassing both FlatMap and Filter, or more in general all available binding constructs!
    }
  }

  //Cannot be written as default argument - this is a dependent default argument, and it can't be part of the same
  //parameter list; using a separate parameter list with only a default argument means that the caller must provide
  // a second parameter list, possibly empty - producing call syntaxes like "usesArgAtMostOnce(f)()".
  private def usesArgAtMostOnce[S, T](f: Fun[S, T]): Boolean = usesArgAtMostOnce(f, f.x)

  //This allows to inline definitions if they are used at most once.
  private[optimization] val letTransformerUsedAtMostOnce: Exp[_] => Exp[_] = {
    case FlatMap(ExpSeq(Seq(exp1)), f) if usesArgAtMostOnce(f) => subst(f)(exp1)
    // XXX: The following case is also a valid optimization, but I expect code to which it applies to never be created,
    // for now (say by inlining Lets). Hence for now disable it. Reenable later.
    //case App(f: Fun[_, _], exp1) if usesArgAtMostOnce(f) => subst(f)(exp1)
    case e => e
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
