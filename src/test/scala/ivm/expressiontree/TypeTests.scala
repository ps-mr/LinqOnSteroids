package ivm
package expressiontree

import org.scalatest.FunSuite
import org.scalatest.matchers.{HavePropertyMatchResult, HavePropertyMatcher, ShouldMatchers}
import java.io.{Closeable, File}
import java.nio.channels.FileChannel
import performancetests.Benchmarking

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
}
