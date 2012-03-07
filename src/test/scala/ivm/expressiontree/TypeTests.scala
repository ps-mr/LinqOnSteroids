package ivm.expressiontree

import org.scalatest.FunSuite
import org.scalatest.matchers.{HavePropertyMatchResult, HavePropertyMatcher, ShouldMatchers}
import collection.{immutable, TraversableLike, mutable}
import collection.generic.CanBuildFrom
import java.io.{Closeable, File}
import java.nio.channels.{Channel, ByteChannel, FileChannel}
import mutable.{Queue, ArrayBuffer, Builder}
import performancetests.Benchmarking
import ivm.collections.TypeMapping

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

  test("TypeIndexSpeed") {
    import de.tud.cs.st.bat.resolved._
    import performancetests.opaltests.{BATLifting, OpalTestData}
    import OpalTestData._
    import Lifting._
    import BATLifting._

    type QueryAnd[+T] = ((ClassFile, Method), T);
    {
      //Same code as above, except that the index returns all free variables, so that the optimizer might find it.
      val typeIdxBase: Exp[Seq[QueryAnd[Instruction]]] = for {
        cf <- queryData.toSeq
        m <- cf.methods if m.body.isDefined
        i <- m.body.get.instructions
      } yield (asExp((cf, m)), i)

      val typeIdx = typeIdxBase.groupByTupleType2
      val evaluatedtypeindex: Exp[TypeMapping[Seq, QueryAnd, Instruction]] = benchMark("los6 Seq-index (less manually optimized) creation, with fixed type indexing"){ asExp(typeIdx.interpret()) }

      val methodsLos6 = evaluatedtypeindex.get[INSTANCEOF].map(_._1._2.name).toSet

      val m6Int: Traversable[String] = benchMark("los6 (with index, less manually optimized, fixed type indexing)") {
        methodsLos6.interpret()
      }
      println(m6Int)
    }
  }
}
