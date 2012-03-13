package ivm
package expressiontree

import org.scalatest.FunSuite
import org.scalatest.matchers.{HavePropertyMatchResult, HavePropertyMatcher, ShouldMatchers}
import java.io.{Closeable, File}
import java.nio.channels.FileChannel
import performancetests.Benchmarking
import collection.mutable.{ArrayBuffer, Map => MutMap}
import optimization.FuncExpBody
import collection.TraversableLike

trait TypeMatchers {
  def typ[ExpectedT: ClassManifest] = new HavePropertyMatcher[Any, OptManifest[_]] {
    def apply(obj: Any): HavePropertyMatchResult[OptManifest[_]] = {
      val actual = ClassManifest.fromClass(obj.getClass)
      val expected = classManifest[ExpectedT]
      HavePropertyMatchResult(
        //expected.erasure.isInstance(obj), //Natural and wrong way to write this
        ClassUtil.boxedErasure(expected).isInstance(obj),
        "type",
        expected,
        actual
      )
    }
  }
}
/**
 * User: pgiarrusso
 * Date: 5/3/2012
 */
class TypeTests extends FunSuite with ShouldMatchers with TypeMatchers with Benchmarking {
  import java.{lang => jl}
  val seenTypesEx: Set[Class[_]] = Set(classOf[jl.Integer], classOf[Null], classOf[AnyRef], classOf[String], classOf[File], classOf[jl.Long], classOf[FileChannel])

  test("check subtype relationship") {
    import TypeHierarchyUtils._
    val rel = benchMark("subtype relationship")(computeSubTypeRel[Void](seenTypesEx))
    /*
    println("Rel:")
    rel foreach println
    println()
    */
    val res = transitiveQuery(rel, classOf[Any])
    assert(res(classOf[Number]))
    //println(res)
    val res2 = transitiveQuery(rel, classOf[Closeable])
    /*
    println(res2)
    println(res2(classOf[Channel]))
    println(res2(classOf[ByteChannel]))
    */
    assert(res2(classOf[FileChannel]))
    //XXX: the code below does not work because of weird issues
    //type C = Class[_]
    //val res: Traversable[C] = transitiveQuery(rel, classOf[Any])
    //res should contain (classOf[Number].asInstanceOf[C])
  }
  test("MaybeSub") {
    def f[A, B](implicit p: MaybeSub[A, B]) = p
    f[String, AnyRef] should have (typ[YesSub[String, AnyRef]])
    f[String, AnyRef] should have (typ[YesSub[_, _]])
    f[Int, AnyVal] should have (typ[YesSub[Int, AnyVal]])
    f[AnyVal, Int] should have (typ[NoSub.type])
    f[FileChannel, String] should have (typ[NoSub.type])
    1 should have (typ[Int])
  }

  /*
   * TODO: to close over open terms, build an environment binding them to the FlatMap expression where they're bound (or
   * the collection in it). It would be complex to also consider bindings in Filter expressions, but those are something
   * we can ignore for now since they don't nest anwyay (we only need the last expression, but we're gonna match on that).
   * And maybe it wouldn't even be that complex.
   *
   * Once we do that, we can also travel down unexpected nodes - for instance indexing would then work also across IsEmpty
   * nodes, which are used in the internal representation of exists.
   * This is simply a different traversal strategy - but we needn't integrate it in Exp (we have enough access).
   */
  type EnvEntry = (Var, Exp[Traversable[_]])
  def transformWithEnv[T](e: Exp[T], env: List[EnvEntry], transformer: (Exp[_], Seq[EnvEntry]) => Exp[_]): Exp[T] = {
    val transformedChilds = e match {
      case FlatMap(coll: Exp[Traversable[_]], fmFun) =>
        val newEnv: List[EnvEntry] = (fmFun.x, coll) :: env
        Seq(transformWithEnv(coll, env, transformer),
          // The new binding is only in scope in the _body_ of the function, not in the whole of it,
          // but it won't matter.
          transformWithEnv(fmFun, newEnv, transformer))
      case _ => for (c <- e.children) yield transformWithEnv(c, env, transformer)
    }
    val newself = e.genericConstructor(transformedChilds)
    transformer(newself, env).asInstanceOf[Exp[T]]
  }

  import Lifting._
  import CollectionUtils.collectFirst
  import Util.assertType
  import optimization.{Optimization, OptimizationTransforms}
  import optimization.SubquerySharing._
  import Optimization.subqueries

  case class Equality[U](varEqSide: Exp[U], constantEqSide: Exp[U], orig: Eq[U])

  sealed abstract class FoundNode[T, Repr <: Traversable[T] with TraversableLike[T, Repr]](val c: Exp[Repr with Traversable[T]]) {
    def optimize[TupleT, U, That <: Traversable[U]](indexBaseToLookup: Exp[Traversable[TupleWith[T]]],
                           parentNode: FlatMap[T, Repr, U, That],
                           allFVSeq: Seq[Var]): Option[Exp[Traversable[T]]]
    type TupleWith[T]
    def buildTuple[T](allFVSeq: Seq[Var])(x: Exp[T]): Exp[TupleWith[T]]
  }
  case class FoundFilter[T, Repr <: Traversable[T] with TraversableLike[T, Repr]](override val c: Exp[Repr],
                                                              f: FuncExp[T, Boolean],
                                                              conds: Set[Exp[Boolean]],
                                                              foundEqs: Set[Equality[_]]) extends FoundNode[T, Repr](c) {
    type TupleWith[T] = Any
    override def buildTuple[T](allFVSeq: Seq[Var])(x: Exp[T]): Exp[Any] = TupleSupport2.toTuple(allFVSeq :+ x)
    override def optimize[TupleT, U, That <: Traversable[U]](indexBaseToLookup: Exp[Traversable[TupleWith[T]]],
                                   parentNode: FlatMap[T, Repr, U, That],
                                   allFVSeq: Seq[Var]) = collectFirst(foundEqs)(tryGroupByNested(indexBaseToLookup, conds, f.x, allFVSeq, parentNode, this)(_))
  }
  case class FoundTypeCase[BaseT,
  Repr <: Traversable[BaseT] with TraversableLike[BaseT, Repr],
  Res,
  That <: TraversableLike[Res, That]](t: TypeCaseExp[BaseT, Repr, Res, That]) extends FoundNode[BaseT, Repr](t.e) {
    type TupleWith[T] = (Any, T)
    override def buildTuple[T](allFVSeq: Seq[Var])(x: Exp[T]): Exp[(_, T)] = (TupleSupport2.toTuple(allFVSeq), x)
    override def optimize[TupleT, U, That <: Traversable[U]](indexBaseToLookup: Exp[Traversable[TupleWith[BaseT]]],
                                   parentNode: FlatMap[BaseT, Repr, U, That],
                                   allFVSeq: Seq[Var]) = {
      val res: Exp[Traversable[Res]] = (for (branch1 <- t.cases) yield {
        branch1 match {
          case branch2: TypeCase[t, _/*Res*/] =>
            val branch = branch2.asInstanceOf[TypeCase[t, Res]] //This cast should really not be needed, but Scalac can't infer it for some reason.
            val guard = branch.guard
            val conds = BooleanOperators.cnf(guard.body)
            tryGroupByNested(indexBaseToLookup, conds, guard.x, allFVSeq, parentNode, this)(null).get.asInstanceOf[Exp[Traversable[t]]] filter guard.f map branch.f
          //collectFirst(conds)(tryGroupByNested(indexBaseToLookup, conds, guard.x, allFVSeq, parentNode, this)(_)).get
        }
      }) reduce (_ ++ _)
      None
    }
  }
  //case object FoundNothing extends FoundNode
  //case class FoundFlatMap[T, U](c: Either[Exp[Option[T]], Exp[Traversable[T]]], f: FuncExp[T, Traversable[U]], isOption: Boolean = false) extends FoundNode

  def defUseFVars(fvContains: Var => Boolean)(e: Exp[_]) = e.findTotFun { case v: Var => fvContains(v); case _ => false }.nonEmpty

  def localLookupIndexableExps[T, Repr <: Traversable[T] with TraversableLike[T, Repr], U, That <: Traversable[U]](e: FlatMap[T, Repr, U, That],
                          freeVars: Set[Var] = Set.empty,
                          fvSeq: Seq[Var] = Seq.empty): Seq[(FlatMap[T, Repr, U, That], FoundNode[T, Repr], Seq[Var])] = {
    e.base match {
      //case Filter(c: Exp[Traversable[T /*t*/]], f: FuncExp[_, _ /*Boolean*/]) =>
      case Filter(c, f) =>
        //case ff @ FoundFilter(_ /*: Exp[Traversable[_ /*t*/]]*/, f: FuncExp[t, _ /*Boolean*/])
        val conds: Set[Exp[Boolean]] = BooleanOperators.cnf(f.body)
        val allFreeVars: Set[Var] = freeVars + f.x
        val usesFVars = defUseFVars(allFreeVars) _

        //Using Sets here directly is very difficult, due to the number of wildcards and the invariance of Set.
        //I managed, but it was beyond the abilities of type inference.
        val foundEqs =
          conds.map {
            case eq @ Eq(l, r) if (eq find {case Var(_) => true}).nonEmpty =>
              if (usesFVars(l) && !usesFVars(r))
                Seq(Equality(l, r, eq))
              else if (usesFVars(r) && !usesFVars(l))
                Seq(Equality(r, l, eq))
              else
                Seq.empty
            case _ => Seq.empty
          }.fold(Seq.empty)(_ union _).toSet[Equality[_]]
        //Seq((e, (ff, conds, foundEqs), fvSeq /*allFVSeq*/))
        Seq((e, FoundFilter[T, Repr](c, f, conds, foundEqs), fvSeq /*allFVSeq*/))
      case t1: TypeCaseExp[baseT, repr, res, that] =>
        val t = t1.asInstanceOf[TypeCaseExp[T, Repr, res, that]]
        Seq((e, FoundTypeCase(t), fvSeq))
      /*case t: TypeFilter[t, c, d, s] /*TypeFilter(base, f, classS)*/ =>
        //XXX: this builds expression nodes instead of using concrete syntax.
        Some(FoundTypeCase(TypeCaseExp(t.base.asInstanceOf[Exp[Traversable[d[t]]]],
          Seq(TypeCase[Any, Any](t.classS.asInstanceOf[Class[Any]], FuncExp((_: Any) => true), FuncExp(identity))))))
        //when[s](identity))))*/
      case _ => Seq.empty
    }
  }

  def lookupIndexableExps(e: Exp[_],
               freeVars: Set[Var] = Set.empty,
               fvSeq: Seq[Var] = Seq.empty): Seq[(FlatMap[_, _, _, _], FoundNode[_, _], Seq[Var])] = {
    require (fvSeq.toSet == freeVars)

    val otherRes = e match {
      case f: FuncExp[_, _] =>
        lookupIndexableExps(f.body, freeVars + f.x, fvSeq :+ f.x)
      case exp => exp.children flatMap {
        lookupIndexableExps(_, freeVars, fvSeq)
      }
    }
    otherRes ++ (e match {
      case f: FlatMap[t, repr, u, that] =>
      //case FlatMap(c: Exp[Traversable[t]], _: FuncExp[_ /*t*/, Traversable[u]]) =>
        localLookupIndexableExps(f, freeVars, fvSeq)
      case _ => Seq.empty
    })
  }

  /*
   * When looking up an index, a filter in the original query will be preserved in the lookup key, and it might make the
   * lookup fail. If an index exists without a filter, we should try to use it.
   * Some filters might be required to prevent subsequent operations from failing, hence they might be required also in
   * the index; stripping the filter will thus create a query which would fail at runtime.
   * That is not a problem, since we don't execute the queries we build, just look them up; if the lookup is successful,
   * it means we constructed a query which the user added to the precomputed query repository, hence it better be safe.
   * An actual concern is that we might need to strip only some indexes but not others from the query in order to find a
   * matching index.
   */
  private def withoutFilters[TupleT, T, U](indexBaseToLookup: Exp[Traversable[TupleT]], tuplingTransform: (Exp[U], TypedVar[Seq[T]]) => Exp[U], fx: TypedVar[Seq[T]]): (Exp[Traversable[TupleT]], Exp[Boolean]) = {
    val v = ArrayBuffer[Exp[Boolean]]()
    //We are removing too many filters, and not keeping track of their bindings :-(.
    val res = indexBaseToLookup transform { //We only want to recognize filtering in the collection branch of FlatMap nodes. Easy!
      e => e match {
        case FlatMap(Filter(base: Exp[Traversable[_]], f: FuncExp[_, _ /*Boolean*/]), fmFun) =>
          val alphaRenamed = f.body.substSubTerm(f.x, fmFun.x) //tuplingTransform acts on var from flatMap functions, not filter functions.
          v += tuplingTransform(alphaRenamed.asInstanceOf[Exp[U]], fx).asInstanceOf[Exp[Boolean]] //XXX fix typing here
          OptimizationTransforms.stripView(base) flatMap fmFun
        case _ => e
      }
    }
    (res, v.fold(Const(true))(And))
  }

  private def groupByShareBodyNested[TupleT, T, U](indexBaseToLookup: Exp[Traversable[TupleT]],
                                      fx: TypedVar[Seq[T]],
                                      fEqBody: Equality[U],
                                      allFVSeq: Seq[Var],
                                      tuplingTransform: (Exp[U], TypedVar[Seq[T]]) => Exp[U]): Option[Exp[Traversable[TupleT]]] = {
    import fEqBody.{varEqSide, constantEqSide}
    val varEqSideTransf = tuplingTransform(varEqSide, fx)

    //println("groupByShareBodyNested on " + indexBaseToLookup)
    val (baseNoFilter, filterCond) = withoutFilters(indexBaseToLookup, tuplingTransform, fx)
    val tries = Seq((indexBaseToLookup, Const(true)), (baseNoFilter, filterCond))
    collectFirst(tries) {
      case (base, cond) =>
        val groupedBy = base.groupBy[U](FuncExp.makefun[TupleT, U](varEqSideTransf, fx))

        assertType[Exp[U => Traversable[TupleT]]](groupedBy) //Just for documentation.
        val toLookup = Optimization.normalize(groupedBy)
        subqueries.get(toLookup) match {
          case Some(t) =>
            println("Found nested index of form " + toLookup)
            Some(asExp(t.asInstanceOf[Map[U, Traversable[TupleT]]]) get constantEqSide flatMap identity withFilter FuncExp.makefun(cond, fx))
          case None =>
            println("Found no nested index of form " + toLookup)
            None
        }
    }
  }

  /**
   *
   * @param indexBaseToLookup
   * @param allConds
   * @param fx
   * @param FVSeq
   * @param parentNode
   * @param eq one of allConds
   * @tparam TupleT
   * @tparam T
   * @return
   */
  private def tryGroupByNested[TupleT, U, FmT, FmRepr <: Traversable[FmT] with TraversableLike[FmT, FmRepr], FmU, FmThat <: Traversable[FmU]](indexBaseToLookup: Exp[Traversable[TupleT]],
                            allConds: Set[Exp[Boolean]],
                            fx: Var, FVSeq: Seq[Var],
                            parentNode: FlatMap[FmT, FmRepr, FmU, FmThat], fn: FoundNode[FmT, FmRepr])
                           (eq: Equality[U]): Option[Exp[Traversable[FmT]]] = {
    val allFVSeq = FVSeq :+ fx
    val allFVMap = allFVSeq.zipWithIndex.toMap
    val usesFVars = defUseFVars(allFVMap contains _) _
    //Filter-specific
    assert(usesFVars(eq.varEqSide) && !usesFVars(eq.constantEqSide))
    def tuplingTransform[T, U](e: Exp[T], tupleVar: TypedVar[Seq[U]]) = e.transform(
      exp => exp match {
        case v: Var if allFVMap contains v =>
          TupleSupport2.projectionTo(tupleVar, allFVSeq.length, allFVMap(v))
        case _ =>
          exp
      })

    //We never want to duplicate variables and take care of scoping.
    //However, we can reuse fx here while still getting a free variable in the end.
    val newVar = fx.asInstanceOf[TypedVar[Seq[FmT]]]
    //Filter-specific
    val step1Opt: Option[Exp[Traversable[TupleT]]] =
      groupByShareBodyNested[TupleT, FmT, U](indexBaseToLookup, newVar, eq, allFVSeq, tuplingTransform)
    //We need to apply tuplingTransform both to allConds and to parentF.
    //About parentF, note that fx is _not_ in scope in its body, which uses another variable, which
    //iterates over the same collection as fx, so it should be considered equivalent to fx from the point of view of
    //tuplingTransform. Hence let's perform the desired alpha-conversion.
    val alphaRenamedParentF = parentNode.f.body.substSubTerm(parentNode.f.x, newVar)
    //residualQuery is also filter-specific, in this form. TypeCase however has guards.
    val step2Opt = step1Opt.map(e => residualQuery(e, (allConds - eq.orig).map(tuplingTransform(_, newVar)), newVar))

    //Note that here, map/flatMap will ensure to use a fresh variable for the FuncExp to build, since it builds a FuncExp
    // instance from the HOAS representation produced.
    parentNode match {
      case FlatMap(_, _) =>
        step2Opt.map(e => e flatMap FuncExp.makefun[TupleT, Traversable[FmT]](
          tuplingTransform(alphaRenamedParentF.asInstanceOf[Exp[Traversable[FmT]]], newVar), newVar))
    }
  }

  val groupByShareNested: Exp[_] => Exp[_] =
    e => {
      (for {
        ((parentNode: FlatMap[t, repr, u, that/*T, Repr, U, That*/]), fn1: FoundNode[_, _], allFVSeq) <- lookupIndexableExps(e)
      } yield {
        //buildTuple produces an open term, because vars in allFVSeq are not bound...
        val fn = fn1.asInstanceOf[FoundNode[t, repr]]
        val indexQuery = OptimizationTransforms.stripView(fn.c) map fn.buildTuple(allFVSeq)
        //... but here we replace parentNode with the open term we just constructed, so that vars in allFVSeq are
        //now bound. Note: e might well be an open term - we don't check it explicitly anywhere, even if we should.
        //However, index lookup is going to fail when open terms are passed - the query repository contains only
        //closed queries.
        //
        //Note: this means that we built the index we search by substitution in the original query; an alternative
        //approach would be to rebuild the index by completing indexQuery with the definitions of the open variables.
        val indexBaseToLookup = e.substSubTerm(parentNode, indexQuery).asInstanceOf[Exp[Traversable[fn.TupleWith[t]]]]

        fn.optimize(indexBaseToLookup, parentNode, allFVSeq)
        /*fn match {
          case FoundFilter(c: Exp[Traversable[Any]], f: FuncExp[_/*t*/, _ /*Boolean*/], conds, foundEqs) =>
            collectFirst(foundEqs)(tryGroupByNested(indexBaseToLookup, conds, f.x, allFVSeq, parentNode)(_))
          case _ => None
        }*/
      }).headOption getOrElse e
    }
}
