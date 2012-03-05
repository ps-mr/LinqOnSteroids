package ivm.expressiontree

import org.scalatest.FunSuite
import org.scalatest.matchers.{HavePropertyMatchResult, HavePropertyMatcher, ShouldMatchers}
import collection.{immutable, TraversableLike, mutable}
import collection.generic.CanBuildFrom
import mutable.{ArrayBuffer, Builder}
import java.io.{Closeable, File}
import java.nio.channels.{Channel, ByteChannel, FileChannel}

trait TypeMatchers {
  def typ[ExpectedT: ClassManifest] = new HavePropertyMatcher[Any, OptManifest[_]] {
    def apply(obj: Any): HavePropertyMatchResult[OptManifest[_]] = {
      val actual = ClassManifest.fromClass(obj.getClass)
      val expected = classManifest[ExpectedT]
      HavePropertyMatchResult(
        //expected.erasure.isInstance(obj), //Natural and wrong way to write this
        ClassUtil.getErasure(expected).isInstance(obj),
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
sealed trait MaybeSub[A, B]
case class YesSub[A, B](implicit val p: A <:< B) extends MaybeSub[A, B]
case class NoSub[A, B]() extends MaybeSub[A, B]

trait LowPriority {
    implicit def noSub[A, B] = NoSub[A, B]
}
object NormalPriority extends LowPriority {
    implicit def yesSub[A, B](implicit p: A <:< B) = YesSub[A, B]
}
import NormalPriority._

class TypeTests extends FunSuite with ShouldMatchers with TypeMatchers {
  //returns all b such that a R* b, where R is the relation represented by map.
  //XXX Note that this is inefficient since we use an immutable array as a mutable one.
  /*private*/ def transitiveQuery[T](map: Map[T, mutable.Set[T]], a: T): mutable.Set[T] = {
    for {
      b <- map.get(a).getOrElse(mutable.Set())
      c <- transitiveQuery(map, b) + b
    } yield c
  }

  class TypeMapping[C[+X] <: TraversableLike[X, C[X]], D[+_], Base](val map: Map[ClassManifest[_], C[D[_]]], val subtypeRel: Map[Class[_], mutable.Set[Class[_]]], origColl: C[D[Base]])(implicit cm: ClassManifest[Base]) {
    //TODO Problem with this implementation: instances of subtypes of T won't be part of the returned collection.
    def getOld[T](implicit tmf: ClassManifest[T]): C[D[T]] = map(tmf).asInstanceOf[C[D[T]]]


    def get[T, That](implicit tmf: ClassManifest[T], m: MaybeSub[Base, T], cbf: CanBuildFrom[C[D[Base]], D[T], That]): That = {
      m match {
        case v @ YesSub() =>
          //origColl map (_ map v.p.apply)
          (cbf() ++= origColl.asInstanceOf[C[D[T]]]) result() //For this to make sense, covariance of C and D is required, as in various other places.
        case NoSub() =>
          val baseResult = map(tmf).asInstanceOf[C[D[Base /*T*/]]]
          val coll = cbf(baseResult)
          for (t <- transitiveQuery(subtypeRel, ClassUtil.getErasure(tmf)))
            //XXX: Jumping back and forth from Class[_] to ClassManifest[_] is extremely annoying, even performance-wise.
            coll ++= map(ClassManifest.fromClass(t)).asInstanceOf[C[D[T]]]
          coll.result()
          //baseResult
      }
    }
  }

  //This must be only used inside the implementation. Mutability fun!
  /*private*/ def groupBySel[A, K, B, Repr <: Traversable[A], That](coll: Repr with Traversable[A])(f: A => K, g: A => B)(implicit cbf: CanBuildFrom[Repr, B, That]): immutable.Map[K, That] = {
    val m = mutable.Map.empty[K, Builder[A, Traversable[A]]]
    for (elem <- coll) {
      val key = f(elem)
      val bldr = m.getOrElseUpdate(key, Traversable.newBuilder[A])
      bldr += elem
    }
    //Remove this part if possible!
    val b = immutable.Map.newBuilder[K, That]
    for ((k, v) <- m)
      b += ((k, v.mapResult(c => (cbf(coll) ++= (c map g)).result()).result()))

    b.result
  }

  /*private*/ def groupBySelAndForeach[A, K, B, Repr <: TraversableLike[A, Repr], That](coll: Repr with TraversableLike[A, Repr])(f: A => K, g: A => B)(h: K => Unit)(implicit cbf: CanBuildFrom[Repr, B, That]): immutable.Map[K, That] = {
    val m = mutable.Map.empty[K, Builder[A, Traversable[A]]]
    for (elem <- coll) {
      val key = f(elem)
      val bldr = m.getOrElseUpdate(key, Traversable.newBuilder[A])
      bldr += elem
      h(key)
    }
    //Remove this part if possible!
    val b = immutable.Map.newBuilder[K, That]
    for ((k, v) <- m)
      b += ((k, v.mapResult(c => (cbf(coll) ++= (c map g)).result()).result()))

    b.result
  }

  //Class.getSuperclass can return null, filter that out. Now make sure that Object is always included? Add testcases for primitive types?
  /*private*/ def superClass(c: Class[_]): Option[Class[_]] = Option(c.getSuperclass) orElse (if (c == classOf[Any]) None else Some(classOf[Any]))
  /*private*/ def superInterfaces(c: Class[_]): Seq[Class[_]] = c.getInterfaces
  //getInterfaces returns all implemented interfaces :-), while getSuperclass does not.
  /*private*/ def superTypes(c: Class[_]): Seq[Class[_]] = superInterfaces(c) ++ superClass(c)


  //TODO: have a per-thread global map, so that the reconstructed type hierarchy can be shared between different indexes.
  //Problem there: GC of entries.
  //Contract: Returns a map defined on interfaces and classes, which returns their implementing classes and possibly
  //their implementing interfaces.
  //With this contract, given a type, we can find its concrete subtypes, and look them up in a type index.
  //XXX Careful: T must be passed explicitly! Otherwise it will be deduced to be Nothing
  def computeSubTypeRel[T: ClassManifest](seenTypes: mutable.Set[ClassManifest[_]]) = {
    val subtypeRel = mutable.Set.empty[(Class[_], Class[_])]
    var classesToScan: List[Class[_]] = Nil
    for {
      t <- seenTypes
      if t != ClassManifest.Null && t != classManifest[T]
      t_ = ClassUtil.getErasure(t)
      s <- superTypes(t_)
    } {
      val superClassOpt = superClass(t_).toList
      classesToScan = superClassOpt ::: classesToScan
      for (s <- superClassOpt ++ superInterfaces(t_))
        subtypeRel += (s -> t_) //Map s to its subtypes.
    }
    //For each supertype found, look up its superclasses. XXX This won't include its superinterfaces though - they will be included only for the original type. Maybe that's OK for looking up concrete types!
    while (classesToScan.nonEmpty) {
      val clazz :: rest = classesToScan
      classesToScan = rest
      //for (s <- superClass(t)) {
      for (superType <- superTypes(clazz)) {
        classesToScan = superType :: classesToScan
        subtypeRel += (superType -> clazz)
      }
      /*
      //No need to scan superInterfaces recursively
      for (s <- superInterfaces(t))
        subtypeRel += (s -> t)
        */
    }
    groupBySel(subtypeRel)(_._1, _._2)
  }
  case class GroupByType[T: ClassManifest, C[+X] <: TraversableLike[X, C[X]], D[+_]](base: Exp[C[D[T]]], f: Exp[D[T] => T]) extends Arity2OpExp[C[D[T]], D[T] => T, TypeMapping[C, D, T],
    GroupByType[T, C, D]](base, f) {
    override def interpret() = {
      val coll: C[D[T]] = base.interpret()
      val g: D[T] => T = f.interpret()
      val seenTypes = mutable.Set.empty[ClassManifest[_]]
      def getType(x: D[T]): ClassManifest[_] = {
        val gx = g(x)
        //Why the null check? Remember that (null instanceof Foo) = false. Hence, without using the index, "if (a instanceof Foo)" subsumes
        //a != null. Here we need to do that check otherwise. To avoid a separate filter stage, and since views don't really support groupBy,
        //aggregate nulls into a separate class.
        if (gx != null)
          ClassManifest.fromClass(gx.getClass)
        else
          ClassManifest.Null
      }

      //val map = coll groupBy getType
      val map = groupBySelAndForeach(coll)(getType, identity)(seenTypes += _)
      val subtypeRel = computeSubTypeRel[T](seenTypes)

      new TypeMapping[C, D, T](map.asInstanceOf[Map[ClassManifest[_], C[D[_]]]], subtypeRel, coll)
    }
    override def copy(base: Exp[C[D[T]]], f: Exp[D[T]=>T]) = GroupByType[T, C, D](base, f)
  }
  val seenTypesEx: mutable.Set[ClassManifest[_]] = mutable.Set(classManifest[Int], classManifest[Null], classManifest[AnyRef], classManifest[String], classManifest[File], classManifest[Long], classManifest[FileChannel])

  test("foo") {
    val rel = computeSubTypeRel[Void](seenTypesEx)
    println("Rel:")
    rel foreach println
    println()
    val res = transitiveQuery(rel, classOf[Any])
    assert(res(classOf[Number]))
    println(res)
    val res2 = transitiveQuery(rel, classOf[Closeable])
    println(res2)
    println(res2(classOf[Channel]))
    println(res2(classOf[ByteChannel]))
    assert(res2(classOf[FileChannel]))
    //XXX: the code below does not work because of weird issues
    //type C = Class[_]
    //val res: Traversable[C] = transitiveQuery(rel, classOf[Any])
    //res should contain (classOf[Number].asInstanceOf[C])
  }
  //XXX test also that YesSub/NoSub works. See /Users/pgiarrusso/tmp/foo.scala
  test("MaybeSub") {
    def f[A, B](implicit p: MaybeSub[A, B]) = p
    f[String, AnyRef] should have (typ[YesSub[String, AnyRef]])
    f[String, AnyRef] should have (typ[YesSub[_, _]])
    f[Int, AnyVal] should have (typ[YesSub[Int, AnyVal]])
    f[AnyVal, Int] should have (typ[NoSub[_, _]])
    f[FileChannel, String] should have (typ[NoSub[_, _]])
    1 should have (typ[Int])
  }
}
