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

trait Inlining {
  def isTrivial(v: Exp[_]): Boolean = v match {
    case ExpSelection(_, _, e) => isTrivial(e)
    case e: ExpProduct with Exp[_] => e.children forall isTrivial
    case _: TypedVar[_] => true
    case _ => false
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
}
