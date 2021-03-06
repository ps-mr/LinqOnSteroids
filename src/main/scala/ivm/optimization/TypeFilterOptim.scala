package ivm
package optimization

import expressiontree._
import Lifting._
import OptimizationUtil._

/**
 * User: pgiarrusso
 * Date: 30/6/2012
 */

trait TypeFilterOptim {
  private def buildTypeFilter[S: TypeTag, T, U](coll: Exp[Traversable[T]], classS: Class[S], f: FunSym[S, Traversable[U]], origFmFun: FunSym[T, Traversable[U]]): Exp[Traversable[U]] = {
    val res = coll.typeFilterClass(classS).flatMap(f.f)
    //Check that the transformed expression has overall the same type as the original one:
    Util.checkSameTypeAndRet(coll flatMap origFmFun)(res)
  }

  private def tryBuildTypeFilter[T, U](coll: Exp[Traversable[T]],
                                       fmFun: FunSym[T, Traversable[U]],
                                       e: Exp[Traversable[U]]): Exp[Traversable[U]] = {
    val X: Exp[Any] = fmFun.x //Trigger implicit conversion to Sym.
    //Correct safety condition for this optimization: The variable of fmFun must appear always wrapped in the same
    //IfInstanceOf node (with the same type manifest...)
    //implicit def noPureForTreeNode[T](t: TreeNode[T]): Exp[TreeNode[T]] = ???
    val containingXParent: Seq[Exp[_]] = fmFun.body.findTotFun(_.children.flatMap(_.children).contains(X))
    val containingX = fmFun.body.findTotFun(_.children.contains(X))
    containingX.headOption match {
      case Some(instanceOfNodeSym @ Sym(instanceOfNode: IfInstanceOf[_, s])) if instanceOfNode.x == X && containingX.forall(_ == instanceOfNodeSym) =>
        val typeTagS: TypeTag[s] = instanceOfNode.tS
        val classS = instanceOfNode.classS.asInstanceOf[Class[s]]
        val v = Fun.gensym[s]()
        if (containingXParent.forall(_ == (Sym[Option[s]](instanceOfNode): Exp[Iterable[s]]))) {
          val transformed = fmFun.body.substSubTerm(containingXParent.head, Seq(asExp(v)))
          buildTypeFilter(coll, classS, Fun.makefun(transformed.asInstanceOf[Exp[Traversable[U]]], v), fmFun)(typeTagS)
        } else {
          val transformed = fmFun.body.substSubTerm(instanceOfNode, Some(asExp(v)))
          //Note: on the result we would really like to drop all the 'Option'-ness, but that's a separate step.
          //Also, if we are in this branch, it means the client code is really using the 'Option'-ness of the value, say
          //via orElse or getOrElse, so we can't drop it.
          buildTypeFilter(coll, classS, Fun.makefun(transformed.asInstanceOf[Exp[Traversable[U]]], v), fmFun)(typeTagS)
        }
      case _ =>
        e
    }
  }

  /*
  fmFun match {
    /*case FuncExpBody(Call1(`optionToIterableId`, _, Call2(`optionMapId`, _, instanceOf@IfInstanceOf(x), f: Fun[Any, _]))) =>
  buildTypeFilter(coll, instanceOf.cS, f)*/
   */

  val toTypeFilter: Exp[_] => Exp[_] = {
    case e @ Sym(FlatMap(coll, fmFun: FunSym[t, u])) =>
      tryBuildTypeFilter(coll, fmFun, e.asInstanceOf[Exp[Traversable[u]]])
    case e => e
  }

  //removeRedundantLet is supposed to eliminate redundant lets from code like:
  //for (i <- base.typeFilter[Int]; j <- Let(i) if j % 2 ==# 0) yield j
  //which is for instance produced by toTypeFilter.
  //The transformation can be described as FlatMap(coll, x => f(Seq(x))) => f(coll), under the condition that
  //f is a sequence homomorphism, i.e. if it distributes over list concatenation so that
  // coll flatMap (x => f(Seq(x))) = {flatMap identity}
  // coll map (x => f(Seq(x))) flatten = {undo map fusion}
  // coll map (x => Seq(x)) map f flatten = {distributivity of f}
  // f(coll map (x => Seq(x)) flatten = {compose map and flatten}
  // f(coll)
  // However, we use in practice a much more restrictive condition.
  //This optimization does not extend to normal Let bindings as used in FindBugsAnalyses. There we need to produce the usual
  //desugaring of Let - i.e. to use letExp; that's done in letTransformer.
  private def tryRemoveRedundantLet[T, U](coll: Exp[Traversable[T]],
                                          fmFun: FunSym[T, Traversable[U]],
                                          e: Exp[Traversable[U]]): Exp[Traversable[U]] = {
    val insideConv = fmFun.body
    // The safety condition for this optimization is two-fold:
    // 1. The variable of fmFun must appear always wrapped in the same
    //    Let node
    // 2. Only supported Traversable operations must appear.

    val X: Exp[Any] = fmFun.x

    //Check safety condition, part 2.
    //@tailrec
    def isSupported(insideConv: Exp[_], LetNode: Exp[_]): Boolean =
      insideConv match {
        case Sym(MapNode(subColl, f)) => isSupported(subColl, LetNode) && !f.body.isOrContains(X)
        case Sym(Filter(subColl, f)) => isSupported(stripViewUntyped(subColl), LetNode) && !f.body.isOrContains(X)
        case Sym(FlatMap(subColl, f)) => isSupported(subColl, LetNode) && !f.body.isOrContains(X)
        case LetNode => true
        case _ => false
      }

    val containingX = insideConv.findTotFun(_.children.contains(X))
    containingX.headOption match {
      case Some(letNode@Sym(ExpSeq(Seq(X)))) if containingX.forall(_ == letNode) && isSupported(insideConv, letNode) =>
        insideConv.substSubTerm(letNode, coll)
      case _ =>
        e
    }
  }

  val removeRedundantOption: Exp[_] => Exp[_] = {
    case e@Sym(FlatMap(coll, (fmFun: FunSym[_, Traversable[u]] @unchecked))) =>
      tryRemoveRedundantLet(coll, fmFun, e.asInstanceOf[Exp[Traversable[u]]])
    case e => e
  }
}
