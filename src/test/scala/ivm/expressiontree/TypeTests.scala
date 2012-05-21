package ivm
package expressiontree

import org.scalatest.FunSuite
import org.scalatest.matchers.{HavePropertyMatchResult, HavePropertyMatcher, ShouldMatchers}
import java.io.{Closeable, File}
import java.nio.channels.FileChannel
import performancetests.Benchmarking
import collection.TraversableLike
import collection.generic.CanBuildFrom

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

  object Binding {
    def unapply(e: Exp[_]): Option[(Exp[_], Fun[_, _])] = e match {
      case FlatMap(base, f) => Some((base, f))
      case Filter(base, f) => Some((base, f))
      case _ => None
    }
  }
  def testBinding(e: Exp[_]) = e match {
    case Binding(base, f) => base
    case _ => e
  }

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
  //Calling this app hides the existing implicit conversion.
  def app2[A, B](f: Exp[A => B]): Exp[A] => Exp[B] = arg => App(f, arg)

  case class MapOp2[T, Repr, U, That](base: Exp[TraversableLike[T, Repr]], f: Fun[T, U])
                                     (implicit /*protected[this] */val c: CanBuildFrom[Repr, U, That]) extends Arity2Op[Exp[TraversableLike[T, Repr]], Fun[T, U], That, MapOp2[T, Repr, U, That]](base, f) {
    override def interpret() = base.interpret() map f.interpret()
    override def copy(base: Exp[TraversableLike[T, Repr]], f: Fun[T, U]) = MapOp2[T, Repr, U, That](base, f)
  }

  import collection.TraversableLike
  import collection.generic.CanBuildFrom
  import Lifting._

  //Analogous to Lifting.groupBySelImpl; I copied it here just to test whether expToTraversableLikeOps works.
  def groupBySelImpl[T, Repr <: Traversable[T] with
    TraversableLike[T, Repr], K, Rest, That <: Traversable[Rest]](t: Exp[Repr], f: Exp[T] => Exp[K])(
    implicit cbf: CanBuildFrom[Repr, T, Repr], cbf2: CanBuildFrom[Repr, Rest, That]): Exp[Map[K, Repr]] =
  {
    Util.assertTypeAndRet[Exp[Map[K, Repr]]] {
      t.indexBy(f)
    }
  }

  import optimization.OptimizationTransforms.Transformer

  //Let's try to express map fusion, which transforms
  // c map f map g into c map (f andThen g)
  val mergeMaps = new Transformer {
    //import optimization.&
    def apply[T](e: Exp[T]) = e match {
      //case (m: MapNode[t, repr, u, that]) & MapNode(c, base) => //doesn't refine the type of c and base.
      case m: MapNode[t, repr, u, that] => //T = that
        //m.base.map(m.f)(m.c)
        Util.assertType[Exp[repr]](m.base)
        val ret1 = (m.base map m.f)(m.c)

        m.base match {
          case m2: MapNode[t2, repr2, u2, that2] =>
          //case m2: MapNode[t2, repr2, `t` /*u2*/, that2] =>//gives a warning
            //Scalac gets that2 = repr, but not that u2 = t.
            //What type inference could deduce:
            //m.base.type <: Exp[repr]
            //m.base.type = Exp[that2]
            //Exp[that2] <: Exp[repr]
            //
            //that2 <: repr; but in practice, the equality that2 = repr seems to be deduced.
            //that2 <: Traversable[u2] with TraversableLike[u2, that2]
            //repr <: Traversable[t] with TraversableLike[t, repr]
            //Combining everything, we get
            //that2 = repr <: Traversable[t]
            //but there is no type <: that2; in particular, we can't write Traversable[u2] <: that2.
            //What we know (but the compiler doesn't) is that that2 is a collection type, whose element type is equal
            //to u2, and we essentially know that from the contract.
            //However, maybe not everything is necessarily lost; Traversable[u2] >: that2 = repr <: Traversable[t] still
            //gives constraints, namely that that2 = repr must be also a subtype of u2 with t; that's exactly what we
            //would need, but Scalac instead fails. Here's the new output of -explaintypes showing the failure.
/*
that2 <: Traversable[t with u2] with scala.collection.TraversableLike[t with u2,that2]?
  that2 <: Traversable[t with u2]?
    Traversable[u2] <: Traversable[t with u2]?
      u2 <: t with u2?
        u2 <: t?
          u2 <: Nothing?
            <notype> <: Nothing?
            false
            Any <: Nothing?
              <notype> <: Nothing?
              false
            false
          false
          Any <: t?
            Any <: Nothing?
              <notype> <: Nothing?
              false
            false
          false
        false
      false
    false
    Traversable[u2] with scala.collection.TraversableLike[u2,that2] <: Traversable[t with u2]?
      Traversable[u2] <: Traversable[t with u2]?
        u2 <: t with u2?
          u2 <: t?
            u2 <: Nothing?
              <notype> <: Nothing?
              false
              Any <: Nothing?
                <notype> <: Nothing?
                false
              false
            false
            Any <: t?
              Any <: Nothing?
                <notype> <: Nothing?
                false
              false
            false
          false
        false
      false
      scala.collection.TraversableLike[u2,that2] <: Traversable[t with u2]?
        <notype> <: Traversable[t with u2]?
        false
      false
    false
  false
false
 */

            //We could have a different design where m.base would have type CollectionExp[t, repr], or where t would be
            //available as a type member/type function of repr. So the map would accept a function f of type
            //repr.elem => that.elem

            //Different experiments:
            //((m2.base map m2.f)(m2.c) map m.f)(m.c) //doesn't work
            //expToTraversableLikeOps[u2, that2](m2.base.map(m2.f)(m2.c)).map(m.f.f)(m.c) //doesn't work.
            //expToTraversableLikeOps[t, repr](m2.base.map(m2.f)(m2.c)).map(m.f)(m.c) //works
            (expToTraversableLikeOps[t /*u2*/, /*that2*/ repr]((m2.base map m2.f)(m2.c)) map m.f)(m.c) //works
            //Util.checkSameTypeAndRet(ret1)(((m2.base map m2.f)(m2.c) map m.f)(m.c)) //doesn't work.
            //(expToTraversableLikeOps[u2, that2]((m2.base map m2.f)(m2.c)) map m.f)(m.c) //doesn't work, the t = u2 equality is not deduced!
            //(expToTraversableLikeOps[t /*u2*/, that2]((m2.base map m2.f)(m2.c)) map m.f)(m.c) //doesn't work, the t = u2 equality is not deduced!
            //e
            //(expToTraversableLikeOps[t2 /*u2*/, /*that2*/ repr2](m2.base) map (m2.f andThen m.f.f))(m2.c) //doesn't work
            //(m2.base map (m2.f andThen m.f.f))(m2.c) //doesn't work, same reason: we need the t = u2 equality. If we enforce that through an
            //unchecked pattern match, we learn that m2.c has the wrong type...
            //(m2.base map (m2.f andThen m.f.f))(m.c) //... also m.c has the wrong type
            //(m2.base map (m2.f andThen m.f.f))(m.c.asInstanceOf[CanBuildFrom[repr2, u, that]]) //... so we ignore the type of CanBuildFrom,
            //since instances are anyway defined so that it does not matter.
            //If now we remove the t = u2 equality, we need to use this code (with an extra cast):
            //(m2.base map (m2.f andThen m.f.f.asInstanceOf[Exp[u2] => Exp[u]]))(m.c.asInstanceOf[CanBuildFrom[repr2, u, that]]) //... so we ignore the type of CanBuildFrom,
          case _ => e
        }
      case _ => e
    }
  }
}
