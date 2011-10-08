package ivm.expressiontree

import collection.generic.{FilterMonadic, CanBuildFrom}
import collection.{TraversableView, GenTraversableOnce, TraversableViewLike, IterableView, TraversableLike, mutable}

/**
 * Here I show yet another encoding of expression trees, where methods
 * like +, &lt;= and so on can be added by other classes, rather than having to
 * be inserted in the original object.
 *
 * The original encoding of Klaus had this property but relied on an implicit
 * conversion from T to Exp[T] and then on additional ones from Exp[Double] to
 * DoubleExp, Exp[String] to StringExp, and so on. Quite a few times this did
 * not work because Scala never applies two implicit conversions on top of one
 * another.
 * This can be solved by making their composition available as another implicit conversion, and that's the solution
 * we show here.
 */
object SimpleOpenEncoding {
  trait ConversionDisabler {
    //We forbid implicit conversion from Unit to Exp[Unit] by making it ambiguous. To this end we declare noToExpForUnit.
    //It is more specific than toExp[Unit] because it's not generic, but is declared in a superclass, hence
    //has less priority. Ambiguity follows.
    implicit def noToExpForUnit(t: Unit): Exp[Unit] = null
    //Ditto. Creating Const nodes for mutable collection is a contradiction; moreover, those nodes would send no
    //notification for updates to the underlying collection.
    //To test, edit testNoMutableConst below to see that the currently commented-out code does not compile.
    implicit def noConstForMutableColl[T](t: mutable.Traversable[T]): Exp[mutable.Traversable[T]] = null
  }

  trait OpsExpressionTreeTrait extends ConversionDisabler {
    implicit def toExp[T](t: T): Exp[T] = Const(t)
    /*implicit def liftOrd[T: Ordering](x: T) = Const(x)
    implicit def liftNum[T: Numeric](x: T) = Const(x)

    implicit def liftBool(x: Boolean) : Exp[Boolean] = Const(x)
    implicit def liftString(x: String) : Exp[String] = Const(x)*/

    implicit def pairToPairExp[A, B](pair: (Exp[A], Exp[B])): Pair[A, B] = Pair[A,B](pair._1, pair._2)

    //To "unlift" a pair, here's my first solution:
    /*implicit*/ def unliftPair[A, B](pair: Exp[(A, B)]): (Exp[A], Exp[B]) = (Proj1(pair), Proj2(pair))
    /*
    //Unfortunately this conversion is not redundant; we may want to have a special node to support this, or to
    //remove Pair constructors applied on top of other pair constructors.
    implicit def expPairToPairExp[A, B](pair: Exp[(A, B)]): Pair[A, B] =
      (Pair[A,B] _).tupled(unliftPair(pair))
    */

    //Here's the second one, adapted from Klaus code. It represents but does not build a tuple (once one adds lazy vals).
    //However, one cannot do pattern matching against the result, not with the existing pattern.
    //Lesson: Scala does not allow to define additional extractors for a given pattern type, and syntax shortcuts such
    //as tuples or => are simply built-in in the language.
    case class PairHelper[A, B](p: Exp[(A, B)]) {
      lazy val _1 = Proj1(p)
      lazy val _2 = Proj2(p)
    }

    implicit def toPairHelper[A, B](e: Exp[(A, B)]): PairHelper[A, B] = PairHelper(e)

    implicit def fToFunOps[A, B](f: Exp[A => B]): Exp[A] => Exp[B] =
      x => f match {
        case FuncExp(fe) => fe(x) //This line should be dropped, but then we'll need to introduce a beta-reducer.
                                  // KO: Why do we need a beta-reducer? Since we use HOAS this is just Scala function application
                                  // and already available in App.interpret
                                  // But it may still make sense to evaluate such applications right away
                                  // PG: I believe we need a beta-reducer before any optimization, to ensure that beta-equivalent
                                  // operations optimize to the same thing. Otherwise the optimizer might not find a pattern
                                  // because it would show up only after reduction.
                                  // I believe we want to have App for the same exact reason not all function calls are
                                  // inlined: preventing code size explosion.
        case _ => App(f, x)
      }
  }
  object OpsExpressionTree extends OpsExpressionTreeTrait

  /**
   * In comparison to the other encoding, we don't use CanBuildExp to get most specific types as result types, as
   * that implies that the type is obtained through type inference.
   * Instead, we use conversions from T => Exp[T], and
   * then specific ones Pi T: Numeric. Exp[T] => NumExp[T]; Pi T. Exp[Traversable[T]] => TraversableExp[T]
   */

  object NumOpsExps {
    import OpsExpressionTree._
    class NumOps[T: Numeric](val t: Exp[T]) {
      def +(that: Exp[T]): Exp[T] = Plus(this.t, that)
    }
    class OrderingOps[T: Ordering](t: Exp[T]) {
      def <=(that: Exp[T]) = LEq(t, that)
    }

    implicit def expToNumOps[T : Numeric](t: Exp[T]): NumOps[T] = new NumOps(t)
    implicit def tToNumOps[T: Numeric](t: T): NumOps[T] = expToNumOps(t)
    implicit def expToOrderingOps[T: Ordering](t: Exp[T]) = new OrderingOps(t)
    implicit def tToOrderingOps[T: Ordering](t: T) = expToOrderingOps(t)
  }

  trait TraversableOps {
    import OpsExpressionTree._
    def newWithFilter[T, Repr <: TraversableLike[T, Repr] with Traversable[T]](base: Exp[Repr],
                                                        f: FuncExp[T, Boolean]) =
      new Filter(View[T, Repr](base), f)
    def newMapOp[T, Repr <: FilterMonadic[T, Repr], U, That](base: Exp[Repr],
                                                             f: FuncExp[T, U])
                                                            (implicit c: CanBuildFrom[Repr, U, That]) =
      new MapOp[T, Repr, U, That](base, f)

    /* Lift faithfully the FilterMonadic trait except foreach and withFilter, since we have a special lifting for it.
     * This trait is used both for concrete collections of type Repr <: FilterMonadic[T, Repr].
     */
    trait FilterMonadicOpsLike[T, Repr <: FilterMonadic[T, Repr]] {
      val t: Exp[Repr]
      def map[U, That](f: Exp[T] => Exp[U])(implicit c: CanBuildFrom[Repr, U, That]): Exp[That] =
        MapOp[T, Repr, U, That](this.t, FuncExp(f))
      def flatMap[U, That](f: Exp[T] => Exp[GenTraversableOnce[U]])
                          (implicit c: CanBuildFrom[Repr, U, That]): Exp[That] =
        FlatMap[T, Repr, U, That](this.t, FuncExp(f))
    }

    case class Union[T, Repr <: TraversableLike[T,Repr], U >: T, That](base: Exp[Repr], that: Exp[Traversable[U]])
                                       (implicit c: CanBuildFrom[Repr, U, That]) extends BinaryOpExp[Repr, Traversable[U], That](base, that) {
      override def interpret = base.interpret ++ that.interpret
      override def copy(base: Exp[Repr], that: Exp[Traversable[U]]) = Union[T,Repr,U,That](base, that)
    }
    case class Filter[T, Repr <: TraversableLike[T,Repr]](base: Exp[Repr], f: FuncExp[T, Boolean]) extends BinaryOp[Exp[Repr], FuncExp[T, Boolean], Repr](base, f) {
      override def interpret = base.interpret filter f.interpret()
      override def copy(base: Exp[Repr], f: FuncExp[T, Boolean]) = Filter(base, f)
   }

    case class GroupBy[T, Repr <: TraversableLike[T,Repr],K](base: Exp[Repr], f: Exp[T => K]) extends BinaryOpExp[Repr,
      T => K, Map[K, Repr]](base, f) {
      override def interpret = base.interpret groupBy f.interpret()
      override def copy(base: Exp[Repr], f: Exp[T => K]) = GroupBy(base, f)
    }

    case class Join[T, Repr <: TraversableLike[T,Repr],S, TKey, TResult, That](colouter: Exp[Repr],
                                         colinner: Exp[Traversable[S]],
                                         outerKeySelector: FuncExp[T, TKey],
                                         innerKeySelector: FuncExp[S, TKey],
                                         resultSelector: FuncExp[(T, S), TResult])
                                           (implicit cbf: CanBuildFrom[Repr, TResult, That]) extends
                                         QuinaryOp[Exp[Repr],
                                           Exp[Traversable[S]],
                                           FuncExp[T, TKey], FuncExp[S, TKey], FuncExp[(T, S), TResult],
                                           That](colouter, colinner, outerKeySelector, innerKeySelector, resultSelector) {
      override def copy(colouter: Exp[Repr],
                                         colinner: Exp[Traversable[S]],
                                         outerKeySelector: FuncExp[T, TKey],
                                         innerKeySelector: FuncExp[S, TKey],
                                         resultSelector: FuncExp[(T, S), TResult]) = Join(colouter, colinner, outerKeySelector, innerKeySelector, resultSelector)

      override def interpret() = {
        // naive hash join algorithm
        val ci: Traversable[S] = colinner.interpret()
        val co: Repr = colouter.interpret()
        val builder = cbf(co)
        if (ci.size > co.size) {
          val map  = ci.groupBy(innerKeySelector.interpret())
          for (c <- co; d <- map(outerKeySelector.interpret()(c)))
            builder += resultSelector.interpret()(c,d)
        } else {
          val map  = co.groupBy(outerKeySelector.interpret())
          for (c <- ci; d <- map(innerKeySelector.interpret()(c)))
            builder += resultSelector.interpret()(d,c)
        }
        builder.result()
      }
    }

    //This is just an interface for documentation purposes.
    trait WithFilterable[T, Repr] {
      def withFilter(f: Exp[T] => Exp[Boolean]): Exp[TraversableView[T, Repr]]
    }

    trait WithFilterImpl[T, This <: Traversable[T] with TraversableLike[T, Repr], Repr <: Traversable[T] with TraversableLike[T, Repr]] extends WithFilterable[T, Repr] {
      self: FilterMonadicOpsLike[T, Repr] =>
      def withFilter(f: Exp[T] => Exp[Boolean]): Exp[TraversableView[T, Repr]] =
        newWithFilter(this.t, FuncExp(f))
    }

    trait TraversableLikeOps[T, Repr <: TraversableLike[T, Repr] with Traversable[T]] extends FilterMonadicOpsLike[T, Repr] {
      def filter(f: Exp[T] => Exp[Boolean]): Exp[Repr] =
        Filter(this.t, FuncExp(f))

      def union[U >: T, That](that: Exp[Traversable[U]])(implicit c: CanBuildFrom[Repr, U, That]): Exp[That] =
        Union[T,Repr,U,That](this.t, that)

      def view: Exp[TraversableView[T, Repr]] = View[T, Repr](this.t)

      def groupBy[K](f: Exp[T] => Exp[K]): Exp[Map[K, Repr]] =
        GroupBy(this.t, FuncExp(f))

      def join[S, TKey, TResult, That](outercol: Exp[Traversable[S]],
                                       outerKeySelector: Exp[T] => Exp[TKey],
                                       innerKeySelector: Exp[S] => Exp[TKey],
                                       resultSelector: Exp[(T, S)] => Exp[TResult])
                                      (implicit cbf: CanBuildFrom[Repr, TResult, That]): Exp[That]
      = Join(this.t, outercol, FuncExp(outerKeySelector), FuncExp(innerKeySelector), FuncExp(resultSelector))
    }

    trait TraversableViewLikeOps[
        T,
        Repr <: TraversableLike[T, Repr] with Traversable[T],
        ViewColl <: TraversableViewLike[T, Repr, ViewColl] with TraversableView[T, Repr] with TraversableLike[T, ViewColl]]
      extends TraversableLikeOps[T, ViewColl] with WithFilterable[T, Repr]
    {
      def force[That](implicit bf: CanBuildFrom[Repr, T, That]) = Force[T, Repr, ViewColl, That](this.t)

      def withFilter(f: Exp[T] => Exp[Boolean]): Exp[ViewColl] =
        new Filter(this.t, FuncExp(f))
    }

    class TraversableOps[T](val t: Exp[Traversable[T]]) extends TraversableLikeOps[T, Traversable[T]] with WithFilterImpl[T, Traversable[T], Traversable[T]]

    class TraversableViewOps[T, Repr <: Traversable[T] with TraversableLike[T, Repr]](val t: Exp[TraversableView[T, Repr]])
      extends TraversableViewLikeOps[T, Repr, TraversableView[T, Repr]]

    implicit def expToTravExp[T](t: Exp[Traversable[T]]): TraversableOps[T] = new TraversableOps(t)
    implicit def tToTravExp[T](t: Traversable[T]): TraversableOps[T] = {
      //toExp(t)
      expToTravExp(t)
    }

    implicit def expToTravViewExp[T, Repr <: Traversable[T] with TraversableLike[T, Repr]](t: Exp[TraversableView[T, Repr]]): TraversableViewOps[T, Repr] = new TraversableViewOps(t)
    implicit def tToTravViewExp[T, Repr <: Traversable[T] with TraversableLike[T, Repr]](t: TraversableView[T, Repr]): TraversableViewOps[T, Repr] = expToTravViewExp(t)

    implicit def expToTravViewExp2[T](t: Exp[TraversableView[T, Traversable[_]]]): TraversableViewOps[T, Traversable[T]] = expToTravViewExp(
      t.asInstanceOf[Exp[TraversableView[T, Traversable[T]]]])
    //XXX
    implicit def tToTravViewExp2[T](t: TraversableView[T, Traversable[_]]): TraversableViewOps[T, Traversable[T]] = expToTravViewExp2(t)
  }

  /**
   * A goal of this new encoding is to be able to build expression trees (in particular, query trees) producing
   * different collections; once we can represent query trees producing maps and maintain them incrementally, view
   * maintenance can subsume index update.
   */

  trait MapOps extends TraversableOps {
    import OpsExpressionTree._
    class MapOps[K, V](val t: Exp[Map[K, V]]) extends TraversableLikeOps[(K, V), Map[K, V]] with WithFilterImpl[(K, V), Map[K, V], Map[K, V]] {
      /*
      //IterableView[(K, V), Map[K, V]] is not a subclass of Map; therefore we cannot simply return Exp[Map[K, V]].
      case class WithFilter(base: Exp[Map[K, V]], f: Exp[((K, V)) => Boolean]) extends Exp[IterableView[(K, V), Map[K, V]]] {
        override def interpret = base.interpret.view filter f.interpret
      }
      */
    }

    implicit def expToMapExp[K, V](t: Exp[Map[K, V]]): MapOps[K, V] = new MapOps(t)
    implicit def tToMapExp[K, V](t: Map[K, V]): MapOps[K, V] =
      expToMapExp(t)
  }

  object SimpleOpenEncoding extends MapOps {
    import OpsExpressionTree._
    import NumOpsExps._

    class ToQueryable[T](t: Traversable[T]) {
      def asQueryable: Exp[Traversable[T]] = Const(t)
    }
    implicit def toQueryable[T](t: Traversable[T]) = new ToQueryable(t)

    def show(name: String, v: Any) {
      print(name + ": ")
      println(v)
    }

    def showInterp(name: String, v: Exp[_]) {
      show(name, v)
      show(name + ".interpret", v.interpret)
    }

    def moreTests() {
      println("testBug:")

      val i: Exp[Int] = 1
      show("i", i)
      val i1: Exp[Int] = 1
      show("i1", i1)
      //One of the syntaxes we want to support - both ones just fail, with "could not find implicit value for parameter cTTE: ivm.expressiontree.OpenEncoding.CanBuildExp[Int,ExpT]"
      //val a0: Exp[Traversable[Int]] = Seq(1, 2, 3, 5)
      //val a0: Exp[Traversable[Int]] = Seq(1, 2, 3, 5).toTraversable
      val a0: Exp[Traversable[Int]] = Seq(1, 2, 3, 5)
      show("a0", a0)

      /*val a1 = toExpTempl(Seq(1, 2, 3, 5)) //Doesn't work well - canBuildExp[Seq[Int]]: CanBuildExp[Seq[Int], Exp[Seq[Int]]] is preferred to canBuildExpTrav[Int, NumExp[Int]]: CanBuildExp[Traversable[Int], TraversableExp[Int]].
      show("a1", a1)
      val a2 = toExpTempl(Seq(1, 2, 3, 5).toTraversable) //Doesn't work well either
      show("a2", a2)
      val a3: Any = toExpTempl(Seq(1, 2, 3, 5)) //Works better
      show("a3", a3)
      val a4: Exp[Seq[Int]] = toExpTempl(Seq(1, 2, 3, 5)) //Doesn't work well - that's equivalent to a1
      show("a4", a4)
      val a5: Exp[Traversable[Int]] = toExpTempl(Seq(1, 2, 3, 5)) //Works well apparently.
      show("a5", a5)
      val a6: TraversableExp[Int, NumExp[Int]] = Seq(1, 2, 3, 5) //This one obviously works.
      show("a6", a6)

      show("(like a3) toExpTempl(Seq(1, 2, 3, 5))", toExpTempl(Seq(1, 2, 3, 5)))
      //show("toExpTempl(Seq(1, 2, 3, 5))(canBuildExpTrav)", toExpTempl(Seq(1, 2, 3, 5))(canBuildExpTrav))
      show("toExpTempl(Seq(1, 2, 3, 5).toTraversable)(canBuildExpTrav)", toExpTempl(Seq(1, 2, 3, 5).toTraversable)(canBuildExpTrav))*/

      println()
    }

    import Util.assertType

    def testInadequate(c: Exp[Map[Int, Int]]) {
      val e: Exp[Map[Int, Int]] = c map (_ match {
        case Pair(a, b) => (a, b + 1) //Inadequate term, even if it's the first I wrote; it causes a crash
      })
      assertType[Exp[Map[Int, Int]]](e)
      showInterp("e", e)
    }

    def testTraversableView(exp: Exp[Traversable[Int]]) {
      val a = exp.view
      showInterp("a", a)
      assertType[Exp[Traversable[Int]]](a)
      assertType[Exp[TraversableView[Int, Traversable[Int]]]](a)
      val b = a.map(_ + 1)
      assertType[Exp[Traversable[Int]]](b)
      // The underscore is due to the type of TraversableView.canBuildFrom. However, it doesn't really matter - see the
      // type of forcedB
      assertType[Exp[TraversableView[Int, Traversable[_]]]](b)
      //assertType[Exp[TraversableView[Int, Traversable[Int]]]](b)
      showInterp("b", b)
      val forcedB = b.force
      showInterp("forcedB", forcedB)
      assertType[Exp[Traversable[Int]]](forcedB)

      val c = a.withFilter(_ <= 3)
      assertType[Exp[Traversable[Int]]](c)
      assertType[Exp[TraversableView[Int, Traversable[Int]]]](c)
      showInterp("c", c)

      val forcedC = c.force
      showInterp("forcedC", forcedC)
      assertType[Exp[Traversable[Int]]](forcedC)
    }

    def testNoMutableConst() {
      val mutableD = mutable.Seq(1)
      //XXX: As desired, thanks to noConstForMutableColl this code does not compile. It is unfortunate that we can't assert
      // that some code does not compile. Recheck when changing this code.
      //val mutExp: Exp[Traversable[Int]] = mutableD
    }

    def testTraversable() {
      moreTests()
      testNoMutableConst()

      val data = Seq(1, 2, 2, 3, 5, 5, 3)
      val a: Exp[Traversable[Int]] = data
      val a2 = data.asQueryable
      assertType[Exp[Traversable[Int]]](a2)
      val b1 = a.map(_ + 1)
      val b2 = a2.map(1 + _)
      val b3 = b1.map(2 + _)
      showInterp("b1", b1)
      showInterp("b2", b2)
      showInterp("b3", b3)
      val b4 = a groupBy identity
      assertType[Exp[Map[Int, Traversable[Int]]]](b4)
      showInterp("b4", b4)

      val c: Exp[Map[Int, Int]] = Map(1 -> 2, 2 -> 4, 3 -> 4)
      showInterp("c", c)
      // Type annotations on the results of map below are not needed to get the correct result, they just check that the
      // result has the correct type.
      //XXX: Implicit conversions are not used by the Scala compiler to fix pattern match errors.
      val d = c map (unliftPair(_) match {
        case (a, b) => (a, b + 1)
      })
      assertType[Exp[Map[Int, Int]]](d)
      showInterp("d", d)
      val d2 = c map (ab => (ab._1, ab._2 + 1))
      assertType[Exp[Map[Int, Int]]](d2)
      showInterp("d2", d2)
      val d3 = c map (ab => (ab._1 + ab._2))
      assertType[Exp[Iterable[Int]]](d3)
      showInterp("d3", d3)

      val d4 = c filter (ab => (ab._1 + ab._2 <= 4))
      assertType[Exp[Map[Int, Int]]](d4)
      showInterp("d4", d4)

      val d5 = c withFilter (ab => (ab._1 + ab._2 <= 4))
      assertType[Exp[TraversableView[(Int, Int), Map[Int, Int]]]](d5)
      showInterp("d5", d5)

      val d6 = d5 withFilter (ab => (ab._1 + ab._2 <= 4))
      assertType[Exp[TraversableView[(Int, Int), Map[Int, Int]]]](d6)
      showInterp("d6", d6)

      val forced = d6.force
      assertType[Exp[Map[Int, Int]]](forced)

      val d7 = c groupBy (ab => ab._2)
      assertType[Exp[Map[Int, Map[Int, Int]]]](d7)
      showInterp("d7", d7)

      val d8 = d7(4)
      assertType[Exp[Map[Int, Int]]](d8)
      showInterp("d8", d8)

      testTraversableView(a)
      testInadequate(c)
    }

    //Analogues of Exp.app. Given the different argument order, I needed to rename them to get a sensible name:
    def withExp[T, U](t: Exp[T])(f: T => U): Exp[U] = App(f, t)
    def withExpFunc[T, U](t: Exp[T])(f: Exp[T] => Exp[U]): Exp[U] = f(t)

    def main(args: Array[String]) {
      val a: Exp[Int] = 1
      val b = a + 2
      //With a smart signatures, type inference works:
      val c2 = withExp(1)(_ + 1)
      val c3 = withExpFunc(1)(_ + 1)

      println(a)
      println(b)
      println(c2)
      println(c3)
      testTraversable()
    }
  }

  def main(args: Array[String]) {
    SimpleOpenEncoding.main(args)
  }
}
