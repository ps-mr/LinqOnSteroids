package ivm
package expressiontree

import org.scalatest.FunSuite
import org.scalatest.matchers.{HavePropertyMatchResult, HavePropertyMatcher, ShouldMatchers}
import java.io.{Closeable, File}
import java.nio.channels.FileChannel
import performancetests.Benchmarking
import collection.mutable.{ArrayBuffer, Map => MutMap}
import optimization.FuncExpBody

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
  val subqueries: MutMap[Exp[_], Any] = Optimization.subqueries

  sealed trait FoundNode
  case class FoundFilter[T](c: Either[Exp[Option[T]], Exp[Traversable[T]]], f: FuncExp[T, Boolean], isOption: Boolean = false) extends FoundNode
  case class FoundFlatMap[T, U](c: Either[Exp[Option[T]], Exp[Traversable[T]]], f: FuncExp[T, Traversable[U]], isOption: Boolean = false) extends FoundNode

  def defUseFVars(fvContains: Var => Boolean)(e: Exp[_]) = e.findTotFun { case v: Var => fvContains(v); case _ => false }.nonEmpty

  def lookupEq2[T, U](e: Exp[Traversable[U]],
               freeVars: Set[Var] = Set.empty,
               fvSeq: Seq[Var] = Seq.empty): Seq[(ivm.expressiontree.Exp[Traversable[U]], FoundFilter[T], Set[Exp[Boolean]], Set[Exp[_]], Seq[Var])] = {
    require (fvSeq.toSet == freeVars)
    import OptionOps._

    ((e match {
      case FlatMap(c: Exp[Traversable[T]], f: FuncExp[_ /*t*/, Traversable[U]]) =>
        Some(FoundFlatMap[T /*t*/, U](Right(c), f))
      case Call2(OptionFlatMapId, _, subColl: Exp[Option[T]], f: FuncExp[_ /*t*/, Traversable[U]]) =>
        Some(FoundFlatMap[T /*t*/, U](Left(subColl), f.asInstanceOf[FuncExp[T /*t*/, Traversable[U]]], true))
      case _ => None
    }) flatMap {
      case FoundFlatMap(cEither, f, isOption) =>
        (cEither match {
          case Right(Filter(c: Exp[Traversable[_ /*t*/]], f: FuncExp[t, _ /*Boolean*/])) =>
            Some(FoundFilter(Right(c), f))
          case Left(Call2(OptionFilterId, _, subColl: Exp[Option[t]], f: FuncExp[_, _])) =>
            //XXX not so sure we want to optimize such a one-element filter.
            //But it can be a one-element filter on top of various navigation operations, so it can still make sense.
            Some(FoundFilter(Left(subColl), f.asInstanceOf[FuncExp[t, Boolean]], true))
          case other => None
        }).asInstanceOf[Option[FoundFilter[T]]]
      case _ => None
    }).toList flatMap {
      case ff @ FoundFilter(_ /*: Exp[Traversable[_ /*t*/]]*/, f: FuncExp[t, _ /*Boolean*/], _) =>
        val conds: Set[Exp[Boolean]] = BooleanOperators.cnf(f.body)
        val allFreeVars: Set[Var] = freeVars + f.x
        val usesFVars = defUseFVars(allFreeVars) _

        //Using Sets here directly is very difficult, due to the number of wildcards and the invariance of Set.
        //I managed, but it was beyond the abilities of type inference.
        val foundEqs =
          conds.map {
            case eq @ Eq(l, r) if (eq find {case Var(_) => true}).nonEmpty && (usesFVars(l) && !usesFVars(r) || usesFVars(r) && !usesFVars(l)) =>
              Seq(eq)
            case _ => Seq.empty
          }.fold(Seq.empty)(_ union _).toSet[Exp[_]]
        Seq((e, ff, conds, foundEqs, fvSeq /*allFVSeq*/))
      case _ => Seq.empty
    }
  }
  //Preconditions:
  //Postconditions: when extracting Some((parentNode, parentF)), filterExp, conds, foundEqs, allFVSeq, parentNode is a FlatMap node which
  // contains a filter expression as base collection (and not through a FuncExp node); the filter expression is returned, after being transformed
  // to a FoundFilter instance, inside filterExp. filterExp.isOption tells whether both filterExp and parentNode are of type Exp[Option[_]] instead of
  //Exp[Traversable[_]] - they cannot differ in this regard (currently). If we allow for conversion nodes later, this assumption will not hold
  //any more - we should normalize away such conversions if possible.
  def lookupEq(parent: Option[(Exp[Traversable[_]], FuncExp[_, _])],
               e: Exp[_],
               freeVars: Set[Var] = Set.empty,
               fvSeq: Seq[Var] = Seq.empty): Seq[((Exp[Traversable[_]], FuncExp[_, _]), FoundFilter[_], Set[Exp[Boolean]], Set[Exp[_]], Seq[Var])] = {
    require (fvSeq.toSet == freeVars)
    import OptionOps._

    val matchResult: Either[Exp[_], FoundNode] = e match {
      case Filter(c: Exp[Traversable[_ /*t*/]], f: FuncExp[t, _ /*Boolean*/]) if parent.isDefined =>
        Right(FoundFilter(Right(c), f))
      case Call2(OptionFilterId, _, subColl: Exp[Option[t]], f: FuncExp[_, _]) if parent.isDefined =>
        //XXX not so sure we want to optimize such a one-element filter.
        //But it can be a one-element filter on top of various navigation operations, so it can still make sense.
        Right(FoundFilter(Left(subColl), f.asInstanceOf[FuncExp[t, Boolean]], true))
      case FlatMap(c: Exp[Traversable[_]], f: FuncExp[t, Traversable[u]]) =>
        Right(FoundFlatMap(Right(c), f))
      case Call2(OptionFlatMapId, _, subColl: Exp[Option[_]], f: FuncExp[t, Traversable[u]]) =>
        Right(FoundFlatMap(Left(subColl.asInstanceOf[Exp[Option[t]]]), f, true))
      case _ =>
        Left(e)
    }

    matchResult.fold(
      _ match {
        case f: FuncExp[_, _] =>
          lookupEq(None, f.body, freeVars + f.x, fvSeq :+ f.x)
        case exp => exp.children flatMap {
          lookupEq(None, _, freeVars, fvSeq)
        }
      },
      _ match {
        case ff @ FoundFilter(_ /*: Exp[Traversable[_ /*t*/]]*/, f: FuncExp[t, _ /*Boolean*/], _) =>
          assert (parent.isDefined)
          val conds: Set[Exp[Boolean]] = BooleanOperators.cnf(f.body)
          val allFreeVars: Set[Var] = freeVars + f.x
          val usesFVars = defUseFVars(allFreeVars) _

          //Using Sets here directly is very difficult, due to the number of wildcards and the invariance of Set.
          //I managed, but it was beyond the abilities of type inference.
          val foundEqs =
            conds.map {
              case eq @ Eq(l, r) if (eq find {case Var(_) => true}).nonEmpty && (usesFVars(l) && !usesFVars(r) || usesFVars(r) && !usesFVars(l)) =>
                Seq(eq)
              case _ => Seq.empty
            }.fold(Seq.empty)(_ union _).toSet[Exp[_]]
          Seq((parent.get, ff, conds, foundEqs, fvSeq /*allFVSeq*/)) //Don't include the variable of the filter, which is going to be dropped anyway - hence fvSeq, not allFVSeq
        //case FlatMap(c, f: FuncExp[t, Traversable[u]]) =>
        //lookupEq(Some((e.asInstanceOf[Exp[Traversable[u]]], f)), c, freeVars, fvSeq) union lookupEq(None, f.body, freeVars + f.x, fvSeq :+ f.x)
        case FoundFlatMap(c, f: FuncExp[t, Traversable[u]], _) =>
          //Add all free variables bound in the location.
          lookupEq(Some((e.asInstanceOf[Exp[Traversable[u]]], f)), c.fold(identity, identity), freeVars, fvSeq) union lookupEq(None, f.body, freeVars + f.x, fvSeq :+ f.x)
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
                                      fEqBody: Eq[U],
                                      constantEqSide: Exp[U],
                                      varEqSide: Exp[U],
                                      allFVSeq: Seq[Var],
                                      tuplingTransform: (Exp[U], TypedVar[Seq[T]]) => Exp[U]): Option[Exp[Traversable[TupleT]]] = {
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
   * @param parentF
   * @param isOption whether parentNode and the contained node have type Exp[ Option[_] ] rather than Exp[ Traversable[_] ]
   * @param cond one of allConds
   * @tparam TupleT
   * @tparam T
   * @return
   */
  private def tryGroupByNested[TupleT, T /*, U, V*/](indexBaseToLookup: Exp[Traversable[TupleT]],
                            allConds: Set[Exp[Boolean]],
                            fx: Var, FVSeq: Seq[Var],
                            parentNode: Exp[Traversable[T]],
                            parentF: FuncExp[_ /*T*/, _/*U*/],
                            isOption: Boolean)
                           (cond: Exp[Boolean]): Option[Exp[Traversable[T]]] =
    cond match {
      case eq: Eq[u] =>
        val allFVSeq = FVSeq :+ fx
        val allFVMap = allFVSeq.zipWithIndex.toMap
        val usesFVars = defUseFVars(allFVMap contains _) _
        def tuplingTransform[T, U](e: Exp[T], tupleVar: TypedVar[Seq[U]]) = e.transform(
          exp => exp match {
            case v: Var if allFVMap contains v =>
              TupleSupport2.projectionTo(tupleVar, allFVSeq.length, allFVMap(v))
            case _ =>
              exp
          })

        //We never want to duplicate variables and take care of scoping.
        //However, we can reuse fx here while still getting a free variable in the end.
        val newVar = fx.asInstanceOf[TypedVar[Seq[T]]]
        //XXX probably this should get Exp[Traversable] or Exp[Option], depending on isOption.
        val step1Opt: Option[Exp[Traversable[TupleT]]] =
          if (usesFVars(eq.t1) && !usesFVars(eq.t2))
            groupByShareBodyNested[TupleT, T, u](indexBaseToLookup, newVar, eq, eq.t2, eq.t1, allFVSeq, tuplingTransform)
          else if (usesFVars(eq.t2) && !usesFVars(eq.t1))
            groupByShareBodyNested[TupleT, T, u](indexBaseToLookup, newVar, eq, eq.t1, eq.t2, allFVSeq, tuplingTransform)
          else None
        //We need to apply tuplingTransform both to allConds and to parentF.
        //About parentF, note that fx is _not_ in scope in its body, which uses another variable, which
        //iterates over the same collection as fx, so it should be considered equivalent to fx from the point of view of
        //tuplingTransform. Hence let's perform the desired alpha-conversion.
        val alphaRenamedParentF = parentF.body.substSubTerm(parentF.x, newVar)
        val step2Opt = step1Opt.map(e => residualQuery(e, (allConds - eq).map(tuplingTransform(_, newVar)), newVar))

        //Note that here, map/flatMap will ensure to use a fresh variable for the FuncExp to build, since it builds a FuncExp
        // instance from the HOAS representation produced.
        import OptionOps._
        parentNode match {
          case Call2(OptionFlatMapId, _, _, _) | FlatMap(_, _) =>
            step2Opt.map(e => e flatMap FuncExp.makefun[TupleT, Traversable[T]](
              tuplingTransform(alphaRenamedParentF.asInstanceOf[Exp[Traversable[T]]], newVar), newVar))
        }
      case _ => None
    }

  val groupByShareNested: Exp[_] => Exp[_] =
    e => {
      (for {
        ((parentNode: Exp[Traversable[t]], parentF), FoundFilter(c, f: FuncExp[_/*t*/, _ /*Boolean*/], isOption), conds, foundEqs, allFVSeq) <- lookupEq(None, e)
        optim <- {
            //Here we produce an open term, because vars in allFVSeq are not bound...
            def buildTuple(x: Exp[_]): Exp[_] = TupleSupport2.toTuple(allFVSeq :+ x)
            val indexQuery = c fold (
              cOpt  => (cOpt map buildTuple): Exp[Iterable[Any /*t*/]],
              cTrav => OptimizationTransforms.stripView(cTrav) map buildTuple)

            //... but here we replace parentNode with the open term we just constructed, so that vars in allFVSeq are
            //now bound. Note: e might well be an open term - we don't check it explicitly anywhere, even if we should.
            //However, index lookup is going to fail when open terms are passed - the query repository contains only
            //closed queries.
            //
            //Note: this means that we built the index we search by substitution in the original query; an alternative
            //approach would be to rebuild the index by completing indexQuery with the definitions of the open variables.
            val indexBaseToLookup = e.substSubTerm(parentNode, indexQuery).asInstanceOf[Exp[Traversable[Any]]]
            val optimized: Option[Exp[_]] = collectFirst(conds)(tryGroupByNested(indexBaseToLookup, conds, f.x, allFVSeq, parentNode, parentF, isOption)(_))
            optimized
        }
      } yield optim).headOption.getOrElse(e)
    }
  
  /*val groupByShareNested2: (Exp[_], Seq[EnvEntry]) => Exp[_] =
    (e, env) => {
      e match {
        case fm: FlatMap[t, _, u, _] =>
          fm.base match {
            case ff: Filter[t, _] =>
              val f = ff.f
              val (fvSeq, _) = env.unzip
              val conds: Set[Exp[Boolean]] = BooleanOperators.cnf(f.body)
              val allFreeVars: Set[Var] = fvSeq.toSet + f.x
              val usesFVars = defUseFVars(allFreeVars) _

              //Using Sets here directly is very difficult, due to the number of wildcards and the invariance of Set.
              //I managed, but it was beyond the abilities of type inference.
              val foundEqs =
                conds.map {
                  case eq @ Eq(l, r) if (eq find {case Var(_) => true}).nonEmpty && (usesFVars(l) && !usesFVars(r) || usesFVars(r) && !usesFVars(l)) =>
                    Seq(eq)
                  case _ => Seq.empty
                }.fold(Seq.empty)(_ union _).toSet[Exp[_]]
              val ret = Seq((fm, ff, conds, foundEqs, fvSeq /*allFVSeq*/)) //Don't include the variable of the filter, which is going to be dropped anyway - hence fvSeq, not allFVSeq
              //val (parentNode, FoundFilter(c, f: FuncExp[_/*t*/, _ /*Boolean*/], isOption), _, _, allFVSeq) = ret

              val Seq((parentNode, Filter(c, filterFun: FuncExp[_/*t*/, _ /*Boolean*/]), _, _, allFVSeq)) = ret
              val parentF = parentNode.f
              def buildTuple(x: Exp[_]): Exp[_] = TupleSupport2.toTuple(allFVSeq :+ x)

              val indexQuery = OptimizationTransforms.stripView(c) map buildTuple

              e
          }
      }
    }
    */
}
