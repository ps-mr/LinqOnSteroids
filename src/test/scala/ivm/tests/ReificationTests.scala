package ivm
package tests

import scala.collection.immutable.Vector
import expressiontree.Lifting._
import expressiontree._
import org.scalatest.junit.JUnitSuite
import org.scalatest.junit.ShouldMatchersForJUnit
import org.junit.Test
import optimization.Optimization
import Optimization.optimize

class ReificationTests extends JUnitSuite with ShouldMatchersForJUnit {
  def test(x: Int, y: Int): Boolean = x+y == 12
  def foo(x: Int): Int = x
  def bar(x: Int): Int = x
  val l: Exp[Traversable[Int]] = toExp(Vector.range(1,10))
  val j: Exp[Traversable[Int]] = toExp(Vector.range(1,10))
  val q = for (k  <- l if k <= 5) yield 3 + k
  val r = for (k  <- l; k2  <- j if k is k2) yield k + k2
  val r1 = for (k <- l; k2 <- j if k + k2 is k2 + k) yield k + k2
  val r2 = for (k <- l; k2 <- j if liftCall('test$Int$Int, test, k, k2)) yield k + k2
  val r3 = for (k <- l; k2 <- j if liftCall('foo$Int, foo, k) is liftCall('bar$Int, bar, k2)) yield (k, k2)
  
  @Test
  def testq() {
    optimize(q) should equal (q)
  }
  @Test
  def testr() {
    optimize(r) should equal (l.join(j)(x => x, y => y, p => (p._1 + p._2)))
  }

  @Test
  def testr1() {
    optimize(r1) should equal (r1)
  }
  
  @Test
  def testr2() {
    optimize(r2) should equal (r2)
  }
  
  @Test
  def testr3() {
    optimize(r3) should equal (l.join(j)(x => liftCall('foo$Int,foo,x), y => liftCall('bar$Int, bar, y), p => (p._1, p._2)))
  }
  
  @Test 
  def testfreeVars() {
    q.freeVars should be (Set.empty)
    newMapOp(l,FuncExp( (x: Exp[Int]) => x + Var(58))).freeVars should be (Set(Var(58)))
  }

  @Test
  def testReification() {
    q should equal (newMapOp[Int, Traversable[Int], Int, Traversable[Int]](newWithFilter(l, FuncExp((v2: Exp[Int]) => LEq(v2, 5))),FuncExp((v3: Exp[Int]) => Plus(3, v3))))
    /*println(q.exec())
    println(optimize(q).interpret().exec())
    //val r = l.flatMap( (k) => j.withFilter( (k2 ) => k is k2).map( (k2:Exp[Int]) => k+k2))
    println(r);
    println(optimize(r))
    println(r.exec())
    println(optimize(r).interpret().exec())
    println(r1);
    println(optimize(r1))
    println(r.exec())
    println(r2);
    println(optimize(r2))
    println(r.exec())
    println(r3);
    println(optimize(r3))
    println(r3.exec())
    println(optimize(r3).interpret().exec())*/
  }
  
  @Test
  def testAntiJoin() {
    val r = for (k <- l if j forall (k2 => k !== k2)) yield k
    val rAntiJoin = Optimization.cartProdToAntiJoin(r)
    //This was problematic because it transforms the newly-built FuncExp nodes into FuncExpInt ones, so we test that it works.
    val rOpt = optimize(rAntiJoin)
    println(r)
    println(rAntiJoin)
    println(rOpt)
    r.interpret() should equal (rAntiJoin.interpret())
    r.interpret() should equal (rOpt.interpret())
  }

  @Test
  def testTypeFilterPrimitive() {
    val base = asExp(Seq(1))
    val query1 = for (i <- base.typeFilter[Int] if i % 2 === 0) yield i
    val query2 = for (i <- base.typeFilter[Int] if i % 2 === 1) yield i
    query1.expResult() should be (Seq())
    query2.expResult() should be (Seq(1))
    val query21 = for (i <- base; iCast <- i.ifInstanceOf[Int] if iCast % 2 === 1) yield iCast
    query21.expResult() should be (Seq(1))
    val query21ExpectedOpt = for (i <- base.typeFilter[Int]; iCast <- asExp(Some(i)) if iCast % 2 === 1) yield iCast
    Optimization.toTypeFilter(query21) should be (query21ExpectedOpt)
  }


  @Test
  def testTypeCase() {
    //Rename R -> T and T -> U, or something.
    case class TypeCase[S, T](classS: Class[S], f: FuncExp[S, T])
    //def when[S, T](f: Exp[S] => Exp[T])(implicit cS: ClassManifest[S]) = TypeCase(cS, FuncExp(f))
    trait WhenResult[S] {
      def apply[T](f: Exp[S] => Exp[T])(implicit cS: ClassManifest[S]): TypeCase[S, T]
    }
    object when {
      def apply[S] = new WhenResult[S] {
        override def apply[T](f: Exp[S] => Exp[T])(implicit cS: ClassManifest[S]) =
          TypeCase(IfInstanceOf.getErasure(cS).
            //XXX: This cast is only guaranteed to succeed because of erasure
            asInstanceOf[Class[S]],
            FuncExp(f))
      }
    }

    //The implementation of this function relies on details of erasure for performance:
    //- We use null instead of relying on Option, but we filter null values away. In theory this is only allowed if T >: Null
    //that is T <: AnyRef; this is valid for all types but T <: AnyVal, i.e. for primitive types, but since T is a type
    //parameter, it will be erased to java.lang.Object and even primitive types will be passed boxed.
    //Hence in practice v: T can be casted to AnyRef and compared against null.
    case class TypeCaseExp[R, T](e: Exp[Traversable[R]], cases: Seq[TypeCase[_, T]]) extends Exp[Traversable[T]] {
      override def nodeArity = cases.length + 1
      override def children = e +: (cases map (_.f))
      override def checkedGenericConstructor: Seq[Exp[_]] => Exp[Traversable[T]] = v => TypeCaseExp(v.head.asInstanceOf[Exp[Traversable[R]]], (cases, v.tail).zipped map ((tc, f) => TypeCase(tc.classS.asInstanceOf[Class[Any]], f.asInstanceOf[FuncExp[Any, T]])))
      private def checkF(v: R): T = {
        for (TypeCase(classS, f: FuncExp[s, _/*T*/]) <- cases) {
          if (classS.isInstance(v))
            return f.interpret()(v.asInstanceOf[s]).asInstanceOf[T]
        }
        null.asInstanceOf[T]
      }
      override def interpret() = {
        (e.interpret() map checkF).view filter (_.asInstanceOf[AnyRef] ne null)
      }
      //cases map { case TypeCase(classS, f) => (v: R) => if (v == null || !classS.isInstance(v)) Util.ifInstanceOfBody(v, classS)}
        
    }
    implicit def pimpl[T](e: Exp[Traversable[T]]) = new {
      def typeCase[T](cases: TypeCase[_, T]*) = TypeCaseExp(e, cases)
    }
    val exp = Seq(1).asSmartCollection typeCase (when[Int](_.toString), when[String](identity))
    println(exp)
    println(exp.interpret())
  }
}

