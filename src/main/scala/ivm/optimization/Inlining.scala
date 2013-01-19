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

  case class UsageCount private(n: Option[Int]) {
    def +(that: UsageCount): UsageCount = {
      UsageCount build (for {
        a <- this.n
        b <- that.n
      } yield a + b)
    }
  }
  object UsageCount {
    val zero = new UsageCount(Some(0))
    val one = new UsageCount(Some(1))
    val more = new UsageCount(None)
    def build(n: Option[Int]) = {
      n match {
        case Some(0) => zero
        case Some(1) => one
        case _ => more
      }
    }
  }

  private def usesArgAtMostOnceNotUnderLambda(exp: Exp[_], v1: Exp[_]): UsageCount = {
    import UsageCount._
    if (exp.__find {
        case f: FunSym[_, _] => true
      } forall (!_.isOrContains(v1)))
      exp.findTotFun(_ == v1).length match {
        case 0 => zero
        case 1 => one
        case _ => more
      }
    else
      more
  }


  //Problem here: this assumes that the variable does not appear under explicit lambdas
  @tailrec private def usesArgAtMostOnce(f: FunSym[_, _], v1: Exp[_]): UsageCount = {
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
      case _ => UsageCount.more
    }
  }

  //Cannot be written as default argument - this is a dependent default argument, and it can't be part of the same
  //parameter list; using a separate parameter list with only a default argument means that the caller must provide
  // a second parameter list, possibly empty - producing call syntaxes like "usesArgAtMostOnce(f)()".
  def usesArgAtMostOnce[S, T](f: FunSym[S, T]): UsageCount = usesArgAtMostOnce(f, f.x)

  //This is the shortest way of writing identity.
  val emptyTransform: PartialFunction[Exp[_], Exp[_]] = {
    case e => e
  }
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
  val selectiveLetFlatMapInliner: PartialFunction[Exp[_], Exp[_]] = {
    case Sym(FlatMap(Sym(ExpSeq(Seq(v))), fs @ FunSym(f))) if isTrivial(v) || usesArgAtMostOnce(fs) != UsageCount.more =>
      subst(f)(v)
    // XXX: The following case is also a valid optimization, but I expect code to which it applies to never be created,
    // for now (say by inlining Lets). Hence for now disable it. Reenable later.
    //case App(f: Fun[_, _], exp1) if usesArgAtMostOnce(f) => subst(f)(exp1)

    //XXX having a separate case here is probably a bad idea, we should try to unify this somehow.
    //constantFoldSequences is probably a more powerful and robust replacement.
    //However, we would still need to lift the if upward for this to be useful.
    case Sym(FlatMap(Sym(Filter(seq @ Sym(ExpSeq(Seq(v))), predSym @ FunSym(pred))), fs @ FunSym(f)))
      if isTrivial(v) || usesArgAtMostOnce(fs) + usesArgAtMostOnce(predSym) != UsageCount.more
    =>
      if_# (subst(pred)(v)) { subst(f)(v) } else_# { Seq.empty }

  }

  val selectiveLetFilterInliner: PartialFunction[Exp[_], Exp[_]] = {
    //This case breaks indexing when part of selectiveLetInliner
    case Sym(Filter(Sym(ExpSeq(Seq(v))), predSym @ FunSym(pred))) if isTrivial(v) || usesArgAtMostOnce(predSym) != UsageCount.more =>
      if_# (subst(pred)(v)) { Seq(v) } else_# { Seq.empty }
  }
  val selectiveLetInliner: Exp[_] => Exp[_] = selectiveLetFlatMapInliner orElse emptyTransform
  val selectiveLetCompleteInliner: Exp[_] => Exp[_] = selectiveLetFlatMapInliner orElse selectiveLetFilterInliner orElse emptyTransform

  //This implements rules from "How to Comprehend Queries Functionally", page 14, rules (12a, b).
  val selectiveConstantSeqInliner: Exp[_] => Exp[_] = {
    case binder @ BaseBinding(Sym(ExpSeq(seq)), fun) if (seq exists (isTrivial _)) || usesArgAtMostOnce(fun) != UsageCount.more =>
      //FoldRight means that the zero element is at the left and will set the result type to Seq, which is correct since
      //the original mapping is on a sequence.
      (seq map (seqEl => ExpTransformer(selectiveLetCompleteInliner) apply
                      Sym(binder.defNode genericConstructor List(asExp(Seq(seqEl)), fun)))).
        foldLeft[Exp[Traversable[Any]]](Seq.empty)(_ ++ _)
    case e => e
//    case Sym(FlatMap(Sym(ExpSeq(Seq(v))), fs @ FunSym(f))) if v exists (isTrivial(v) || usesArgAtMostOnce(fs)  =>
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
      //turning filters into ifs... isn't it there? XXX I was thinking of filterToTransformedFilter,
      //but it's not optimized for constant-shaped sequences. The new code in selectiveLetInliner works better.

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
