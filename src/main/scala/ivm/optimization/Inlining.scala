package ivm
package optimization

import expressiontree._
import Lifting._
import annotation.tailrec
import OptimizationUtil._
import Subst.subst

/**
 * This code should implement non-work-duplicating inliner.
 * We don't use the linear type system used by GHC's inliner; from the papers about it I gathered a simpler
 * sufficient condition, which is in general very restrictive but fine for our purposes.
 * User: pgiarrusso
 * Date: 30/6/2012
 */

trait InliningDefs {
  def isTrivial(v: Exp[_]): Boolean = v match {
    case ExpSelection(_, _, e) => isTrivial(e)
    case Sym(e: ExpProduct with Def[_]) => e.children forall isTrivial
    case Sym(_: TypedVar[_]) => true
    case _ => false
  }

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
      (exp.__find {
        case f: FunSym[_, _] => true
      } forall (!_.isOrContains(v1)))

  //Problem here: this assumes that the variable does not appear under explicit lambdas
  @tailrec private def usesArgAtMostOnce(f: FunSym[_, _], v1: Exp[_]): Boolean = {
    f match {
      case FunSym(FuncExpBody(Sym(Binding(Sym(ExpSeq(Seq(exp2))), g)))) if !exp2.isOrContains(v1) =>
        //This allows to inline id1 in cases like:
        //v1 <- Let(exp1)
        //v2 <- Let(exp2) //v1 not free in exp2, that is, !v2.isOrContains(v1)
        //v3 <- Let(exp3) //v1 free in exp3, but not used any more; or v1 not free in exp3 but free in exp4; and so on.
        //v4 <- Let(exp4)
        //Note that it is crucial that all bindings act on single-element collections; we check that by looking for when this is statically known, i.e. we look for ExpSeq(Seq(_)), but we might want to extend that to
        //Const(seq) if seq has "at runtime" just one element.
        usesArgAtMostOnce(g, v1)
      case FunSym(FuncExpBody(baseUsingV)) =>
        //Let's assume that unnesting has already happened, hence all functions we find are just function definitions and not nested FlatMaps to further analyze.
        //Since functions might be applied multiple times, we just make sure that nested function definitions do not refer to g.
        usesArgAtMostOnceNotUnderLambda(baseUsingV, v1)
      case _ => false //XXX add corresponding cases for Filter. Or add a common pattern-matcher encompassing both FlatMap and Filter, or more in general all available binding constructs!
    }
  }

  //Cannot be written as default argument - this is a dependent default argument, and it can't be part of the same
  //parameter list; using a separate parameter list with only a default argument means that the caller must provide
  // a second parameter list, possibly empty - producing call syntaxes like "usesArgAtMostOnce(f)()".
  def usesArgAtMostOnce[S, T](f: FunSym[S, T]): Boolean = usesArgAtMostOnce(f, f.x)

}

trait Inlining extends InliningDefs {
  /*
  val letTransformerTrivial: Exp[_] => Exp[_] = {
    //XXX having a separate case here is probably a bad idea, we should try to unify this somehow.
    //constantFoldSequences is probably a more powerful and robust replacement.
    case Sym(FlatMap(Sym(Filter(seq @ Sym(ExpSeq(Seq(v))), pred)), FunSym(f))) if isTrivial(v) =>
      seq filter pred flatMap Fun.makefun(subst(f)(v), f.x).f
  }
  */

  //This allows to inline definitions if they are used at most once or they are trivial enough.
  //We also reduce lets where the variable is only used once. Papers on the GHC inliner explain how to do this and why
  //a linear type system is needed for that. See letTransformerUsedAtMostOnce.
  val selectiveLetInliner: Exp[_] => Exp[_] = {
    case Sym(FlatMap(Sym(ExpSeq(Seq(v))), fs @ FunSym(f))) if isTrivial(v) || usesArgAtMostOnce(fs)  =>
      subst(f)(v)
    // XXX: The following case is also a valid optimization, but I expect code to which it applies to never be created,
    // for now (say by inlining Lets). Hence for now disable it. Reenable later.
    //case App(f: Fun[_, _], exp1) if usesArgAtMostOnce(f) => subst(f)(exp1)
    case e => e
  }

  //XXX to test
  //This implements rules from "How to Comprehend Queries Functionally", page 14, rules (12a, b).
  //XXX: But maybe filter for whether this enables inlining!
  val constantFoldSequences: Exp[_] => Exp[_] = {
    //XXX having a separate case here is probably a bad idea, we should try to unify this somehow.
    //constantFoldSequences is probably a more powerful and robust replacement.

    //XXX: abstract over Filter/FlatMap with a single matcher having a rebuild method (close to what Klaus suggested). The
    //type signature of rebuild would depend on type parameters specified as a field of the match result.
    case Sym(Filter(Sym(ExpSeq(seq)), pred)) if seq forall isTrivial =>
      (seq map (x => asExp(Seq(x)) filter pred)).fold[Exp[Traversable[Any]]](Seq.empty)(_ ++ _)
      //We need to do more: we need to implement here filter inlining. But that's done in a transformation
      //turning filters into ifs... isn't it there? XXX

      //seq filter pred flatMap Fun.makefun(subst(f)(v), f.x).f
    case Sym(FlatMap(Sym(ExpSeq(seq)), f)) =>
      //Rewrite this as a sequence of expressions.
      (seq map (x => asExp(Seq(x)) flatMap f)).  //map is at the meta-level, flatMap at the object level.
      //(seq map (letExp(_)(f))).
        fold[Exp[Traversable[Any]]] (Seq.empty) (_ ++ _)
      //We need also a case for filters!
    case e => e
  }
}
