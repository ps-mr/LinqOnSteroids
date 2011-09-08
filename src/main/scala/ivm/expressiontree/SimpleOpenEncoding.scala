package ivm.expressiontree

import collection.generic.CanBuildFrom
import collection.{IterableView, TraversableLike, TraversableView}

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
  import OpenEncoding.{ExpModule, BaseExprTree}

  trait OpsExpressionTree extends BaseExprTree with ExpModule {
    case class Var(x: String) extends Exp[Nothing] {
      def interpret = throw new RuntimeException("interpret on var")
    }

    case class FuncExp[-T, +U](f: Exp[T] => Exp[U]) extends Exp[T => U] {
      override def interpret = t => f(Const(t)).interpret
      override def toString = {
        val name = "x" //XXX
        "%s => (%s)" format (name, f(Var(name)))
      }
    }

    case class Pair[A, B](a: Exp[A], b: Exp[B]) extends Exp[(A, B)] {
      def interpret = (a.interpret, b.interpret)
    }

    case class Proj1[A, B](p: Exp[(A, B)]) extends Exp[A] {
      def interpret = p.interpret._1
    }

    case class Proj2[A, B](p: Exp[(A, B)]) extends Exp[B] {
      def interpret = p.interpret._2
    }
  }

  /**
   * In comparison to the other encoding, we don't use CanBuildExp to get most specific types as result types, as
   * that implies that the type is obtained through type inference.
   * Instead, we use conversions from T => Exp[T], and
   * then specific ones Pi T: Numeric. Exp[T] => NumExp[T]; Pi T. Exp[Traversable[T]] => TraversableExp[T]
   */

  trait NumOpsExps {
    this: NumOpsExpressionTree with ExpModule =>
    class NumOps[T](val t: Exp[T])(implicit val isNum: Numeric[T]) {
      def +(that: Exp[T]): Exp[T] = Plus(this.t, that)
    }
  }

  trait NumOpsExpressionTree {
    this: NumOpsExps with ExpModule =>
    //Root node for all binary, associative and commutative operations. The
    //intuition is that many operations (including optimizations) might apply
    //for all of those - e.g. expression normalization.
    abstract class CommutativeOp[T](a: Exp[T], b: Exp[T]) extends Exp[T]

    case class Plus[T](a: Exp[T], b: Exp[T])(implicit val isNum: Numeric[T]) extends CommutativeOp(a, b) {
      def interpret =
        isNum.plus(a.interpret, b.interpret)
    }
  }

  trait TraversableOpsExpressionTree {
    this: OpsExpressionTree =>
    case class MapOp[T, +U](base: Exp[Traversable[T]], f: Exp[T => U]) extends Exp[Traversable[U]] {
      override def interpret = base.interpret map f.interpret
    }
    case class FlatMap[T, +U](base: Exp[Traversable[T]], f: Exp[T => Traversable[U]]) extends Exp[Traversable[U]] {
      override def interpret = base.interpret flatMap f.interpret
    }
    case class WithFilter[T](base: Exp[Traversable[T]], f: Exp[T => Boolean]) extends Exp[Traversable[T]] {
      //XXX: Again the same problem with filtering - we cannot call withFilter.
      override def interpret = base.interpret.view filter f.interpret
    }
    //Alternative approach:
    case class View[T](base: Exp[Traversable[T]]) extends Exp[TraversableView[T, Traversable[T]]] {
      override def interpret = base.interpret.view
    }

    case class WithFilter2[T](base: Exp[TraversableView[T, Traversable[T]]], f: Exp[T => Boolean]) extends Exp[Traversable[T]] {
      override def interpret = base.interpret filter f.interpret
    }

    case class Force[T](base: Exp[TraversableView[T, Traversable[T]]]) extends Exp[Traversable[T]] {
      override def interpret = base.interpret.force
    }

    case class Union[T](base: Exp[Traversable[T]], that: Exp[Traversable[T]]) extends Exp[Traversable[T]] {
      override def interpret = base.interpret ++ that.interpret
    }
  }

  // This is an attempt to abstract over TraversableOpsExpressionTree.MapOp and MapOpsExpressionTree.MapOpForMap.
  // However, it requires Coll[X] to be a total unary type function, while Map[K, V] is a binary type function.
  // Given a cooler version of Tuple, say TupleT, for X = TupleT[K, V] one could write a type function equivalent to
  // X => Map[X#Fst, X#Snd]; however, this function is not total! Hence this encoding cannot work.
  trait TraversableOpsExpressionTreeV2[
    Coll[X] <: TraversableLike[X, Coll[X]]
    //Coll[X] <: Traversable[X] //This bound is not correct - map would then require an instance of
    // CanBuildFrom[Traversable[A], B, That], not of the more specific CanBuildFrom[Coll[A], B, That]
    ] {
    this: OpsExpressionTree =>
    case class MapOpV2[A, +B, That](base: Exp[Coll[A]], f: Exp[A => B])(implicit c: CanBuildFrom[Coll[A], B, That]) extends Exp[That] {
      override def interpret = base.interpret.map(f.interpret)(c)
    }
  }

  /*trait Bla extends TraversableOpsExpressionTreeV2[Map] with OpsExpressionTree {
    
  }*/

  trait TraversableOpsExps {
    this: OpsExpressionTree with TraversableOpsExpressionTree =>
    class TraversableOps[T](val t: Exp[Traversable[T]]) {
      def map[U](f: Exp[T] => Exp[U]): Exp[Traversable[U]] =
        MapOp(this.t, FuncExp(f))
        //App((_: Traversable[T]) map (FuncExp(f).interpret), this.t)

      def flatMap[U](f: Exp[T] => Exp[Traversable[U]]): Exp[Traversable[U]] =
        FlatMap(this.t, FuncExp(f))
        //App((_: Traversable[T]) flatMap FuncExp(f).interpret, this.t)

      def withFilter(f: Exp[T] => Exp[Boolean]): Exp[Traversable[T]] =
        WithFilter2(View(this.t), FuncExp(f))
        //WithFilter(this.t, FuncExp(f))
        //App((_: Traversable[T]) filter FuncExp(f).interpret, this.t) //We can't use withFilter underneath.

      def union[U >: T](that: Exp[Traversable[U]]): Exp[Traversable[U]] =
        Union(this.t, that)
        //App((_: Traversable[T]) ++ u.interpret, this.t) //Should use an App node with two params.
    }
  }

  trait TraversableOps {
    this: OpsExpressionTree =>
    trait TraversableOpsLike[T, Repr <: TraversableLike[T, Repr] with Traversable[T]] {
      val t: Exp[Repr]
      case class MapOp[+U, That](base: Exp[Repr], f: Exp[T => U])(implicit c: CanBuildFrom[Repr, U, That]) extends Exp[That] {
        override def interpret = base.interpret.map(f.interpret)(c)
      }
      case class FlatMap[+U, That](base: Exp[Repr], f: Exp[T => Traversable[U]])
                                  (implicit c: CanBuildFrom[Repr, U, That]) extends Exp[That] {
        override def interpret = base.interpret flatMap f.interpret
      }

      /*
      //XXX: can't be fully defined here, this must be defined in children classes.
      case class WithFilter(base: Exp[Repr], f: Exp[T => Boolean]) extends Exp[Repr] {
        //XXX: Again the same problem with filtering - we cannot call withFilter.
        //override def interpret = base.interpret.view filter f.interpret
        override def interpret = base.interpret filter f.interpret
      }
      */

      case class Union[U >: T, That](base: Exp[Repr], that: Exp[Traversable[U]])
                                    (implicit c: CanBuildFrom[Repr, U, That]) extends Exp[That] {
        override def interpret = base.interpret ++ that.interpret
      }

      def map[U, That](f: Exp[T] => Exp[U])(implicit c: CanBuildFrom[Repr, U, That]): Exp[That] =
        MapOp(this.t, FuncExp(f))

      def flatMap[U, That](f: Exp[T] => Exp[Traversable[U]])(implicit c: CanBuildFrom[Repr, U, That]): Exp[That] =
        FlatMap(this.t, FuncExp(f))

      def withFilter(f: Exp[T] => Exp[Boolean]): Exp[Repr]

      def union[U >: T, That](that: Exp[Traversable[U]])(implicit c: CanBuildFrom[Repr, U, That]): Exp[That] =
        Union(this.t, that)
    }

    class TraversableOps[T](val t: Exp[Traversable[T]]) extends TraversableOpsLike[T, Traversable[T]] {
      def withFilter(f: Exp[T] => Exp[Boolean]): Exp[Traversable[T]] =
        WithFilter(this.t, FuncExp(f))

      case class WithFilter(base: Exp[Traversable[T]], f: Exp[T => Boolean]) extends Exp[Traversable[T]] {
        //XXX: Again the same problem with filtering - we cannot call withFilter.
        override def interpret = base.interpret.view filter f.interpret
      }

      //Do we need this specialization? No we don't, I think.
      /*def map[U](f: Exp[T] => Exp[U]): Exp[Traversable[U]] =
        super.map(f)*/
    }
  }
  trait MapOps {
    this: OpsExpressionTree with TraversableOps =>
    class MapOps[K, V](val t: Exp[Map[K, V]]) extends TraversableOpsLike[(K, V), Map[K, V]] {
      def withFilter(f: Exp[(K, V)] => Exp[Boolean]): Exp[Map[K, V]] =
        Filter(this.t, FuncExp(f))

      case class Filter(base: Exp[Map[K, V]], f: Exp[((K, V)) => Boolean]) extends Exp[Map[K, V]] {
        //XXX: We cannot call withFilter nor view.filter, because MapView does not exist; see below
        override def interpret = base.interpret filter f.interpret
      }
      case class WithFilterWeirdType(base: Exp[Map[K, V]], f: Exp[((K, V)) => Boolean]) extends Exp[IterableView[(K, V), Map[K, V]]] {
        override def interpret = base.interpret.view filter f.interpret
      }
    }
  }

  /**
   * A goal of this new encoding is to be able to build expression trees (in particular, query trees) producing
   * different collections; once we can represent query trees producing maps and maintain them incrementally, view
   * maintenance can subsume index update.
   */
  trait MapOpsExpressionTree {
    this: OpsExpressionTree =>

    // It's amazing that Scala accepts "extends Exp[That]", since it would not accept That; most probably that's thanks to erasure.
    case class MapOpForMap[K, V, U, That](base: Exp[Map[K, V]], f: Exp[((K, V)) => U])(implicit c: CanBuildFrom[Map[K, V], U, That]) extends Exp[That] {
      override def interpret = base.interpret map f.interpret
    }
  }

  trait MapOpsExps {
    this: OpsExpressionTree with MapOpsExpressionTree with TraversableOpsExpressionTree =>

    class MapOps[K, V](val t: Exp[Map[K, V]]) {
      def map[U, That](f: Exp[(K, V)] => Exp[U])(implicit c: CanBuildFrom[Map[K, V], U, That]): Exp[That] =
        MapOpForMap(this.t, FuncExp(f))
    }
  }


  trait SimpleOpenEncodingBase extends OpsExpressionTree with NumOpsExps with NumOpsExpressionTree with TraversableOps with MapOps /*with TraversableOpsExps with TraversableOpsExpressionTree*/ {
    implicit def toExp[T](t: T): Exp[T] = Const(t)

    implicit def expToNumExp[T : Numeric](t: Exp[T]): NumOps[T] = new NumOps(t)
    implicit def tToNumExp[T : Numeric](t: T): NumOps[T] = {
      //toExp(t)
      expToNumExp(t) //Doesn't work, unless we make NumOps not extends Exp. It should expand to:
      //expToNumExp(toExp(t)) //but it does not, because also the next expansion typechecks:
      //expToNumExp(tToNumExp(t))
    }

    implicit def expToTravExp[T](t: Exp[Traversable[T]]): TraversableOps[T] = new TraversableOps(t)
    implicit def tToTravExp[T](t: Traversable[T]): TraversableOps[T] = {
      //toExp(t)
      expToTravExp(t)
    }
    implicit def expToMapExp[K, V](t: Exp[Map[K, V]]): MapOps[K, V] = new MapOps(t)
    implicit def tToMapExp[K, V](t: Map[K, V]): MapOps[K, V] =
      expToMapExp(t)

    implicit def pairToPairExp[A, B](pair: (Exp[A], Exp[B])): Pair[A, B] = Pair[A,B](pair._1, pair._2)

    //To "unlift" a pair, here's my first solution:
    /*implicit*/ def unliftPair[A, B](pair: Exp[(A, B)]): (Exp[A], Exp[B]) = (Proj1(pair), Proj2(pair))
    /*
    //Unfortunately this conversion is not redundant; we may want to have a special node to support this.
    implicit def expPairToPairExp[A, B](pair: Exp[(A, B)]): Pair[A, B] =
      (Pair[A,B] _).tupled(unliftPair(pair))
    */

    //Here's the second one, adapted from Klaus code. It represents but does not build a tuple (once one adds lazy vals).
    //However, one cannot do pattern matching against the result, not with the existing pattern.
    //Lesson: Scala does not allow to define additional extractors for a given pattern type, and syntax shortcuts such
    //as tuples or => are simply built-in in the language.
    case class PairHelper[A,B](p: Exp[(A,B)]) {
      lazy val _1 = Proj1(p)
      lazy val _2 = Proj2(p)
    }

    implicit def toPairHelper[A, B](e: Exp[(A, B)]): PairHelper[A, B] = PairHelper(e)
  }

  object SimpleOpenEncoding extends SimpleOpenEncodingBase {
    class ToQueryable[T](t: Traversable[T]) {
      def asQueryable = Const(t)
    }
    implicit def toQueryable[T](t: Traversable[T]) = new ToQueryable(t)

    def show(name: String, v: Any) {
      print(name + ": ")
      println(v)
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

    def assertType[T](t: T) {}

    def testTraversable() {
      moreTests()

      val a: Exp[Traversable[Int]] = Seq(1, 2, 3, 5)
      val a2 = Seq(1, 2, 3, 5).asQueryable
      val b1 = a.map(_ + 1)
      val b2 = a2.map(1 + _)
      val b3 = b1.map(2 + _)
      show("b1", b1)
      show("b1.interpret", b1.interpret)
      show("b2", b2)
      show("b2.interpret", b2.interpret)
      show("b3", b3)
      show("b3.interpret", b3.interpret)

      val c: Exp[Map[Int, Int]] = Map(1 -> 2, 3 -> 4)
      show("c", c)
      show("c.interpret", c.interpret)
      //Type annotations on the results of map below are not needed to get the correct result, they just check that the
      // result has the correct type.
      //XXX: Implicit conversions are not used by the Scala compiler to fix pattern match errors.
      val d = c map (unliftPair(_) match {
        case (a, b) => (a, b + 1)
      })
      assertType[Exp[Map[Int, Int]]](d)
      show("d", d)
      show("d.interpret", d.interpret)
      val d2 = c map (ab => (ab._1, ab._2 + 1))
      assertType[Exp[Map[Int, Int]]](d2)
      show("d2", d2)
      show("d2.interpret", d2.interpret)
      val d3 = c map (ab => (ab._1 + ab._2))
      //assertType[Exp[Seq[Int]]](d3) //XXX broken
      assertType[Exp[Iterable[Int]]](d3)
      show("d3", d3)
      show("d3.interpret", d3.interpret)
      val e: Exp[Map[Int, Int]] = c map (_ match {
        case Pair(a, b) => (a, b + 1) //Inadequate term, even if it's the first I wrote; it causes a crash
      })
      assertType[Exp[Map[Int, Int]]](e)
      show("e", e)
      show("e.interpret", e.interpret)
    }

    //Analogues of Exp.app. Given the different argument order, I needed to rename them to get a sensible name:
    def withExp[T, U](t: Exp[T])(f: T => U): Exp[U] = App(f, t)
    def withExpFunc[T, U](t: Exp[T])(f: Exp[T] => Exp[U]): Exp[U] = f(t)

    def main(args: Array[String]) {
      val a: Exp[Int] = 1
      val b = a + 2
      //Here we type inference fails:
      val c = Exp.app((x: Int) => x + 1, 1)
      //With smarter signatures, it works:
      val c2 = withExp(1)(_ + 1)
      val c3 = withExpFunc(1)(_ + 1)

      println(a)
      println(b)
      println(c)
      println(c2)
      println(c3)
      testTraversable()
    }
  }

  def main(args: Array[String]) {
    SimpleOpenEncoding.main(args)
  }
}