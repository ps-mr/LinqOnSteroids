package ivm.expressiontree

import org.scalatest.FunSuite
import org.scalatest.matchers.{HavePropertyMatchResult, HavePropertyMatcher, ShouldMatchers}
import collection.{immutable, TraversableLike, mutable}
import collection.generic.CanBuildFrom
import java.io.{Closeable, File}
import java.nio.channels.{Channel, ByteChannel, FileChannel}
import mutable.{Queue, ArrayBuffer, Builder}
import performancetests.Benchmarking
import performancetests.opaltests.BATLifting

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
sealed trait MaybeSub[-A, +B]
case class YesSub[-A, +B](implicit val p: A <:< B) extends MaybeSub[A, B]
case object NoSub extends MaybeSub[Any, Nothing]

trait LowPriority {
    implicit def noSub = NoSub
}
object NormalPriority extends LowPriority {
    implicit def yesSub[A, B](implicit p: A <:< B) = YesSub[A, B]
}
import NormalPriority._

class TypeTests extends FunSuite with ShouldMatchers with TypeMatchers with Benchmarking {
  //returns all b such that a R* b, where R is the relation represented by map.
  //XXX Note that this is inefficient since we use a mutable set as an immutable array one.
  /*private*/ def transitiveQuery[T](map: Map[T, collection.Set[T]], a: T): collection.Set[T] = {
    for {
      b <- map.get(a).getOrElse(collection.Set())
      c <- transitiveQuery(map, b) + b
    } yield c
  }

  class TypeMapping[C[+X] <: TraversableLike[X, C[X]], D[+_], Base](val map: Map[Class[_], C[D[_]]], val subtypeRel: Map[Class[_], collection.Set[Class[_]]], origColl: C[D[Base]])(implicit cm: ClassManifest[Base]) {
    //TODO Problem with this implementation: instances of subtypes of T won't be part of the returned collection.
    //def getOld[T](implicit tmf: ClassManifest[T]): C[D[T]] = map(ClassUtil.boxedErasure(tmf)).asInstanceOf[C[D[T]]]


    def get[T, That](implicit tmf: ClassManifest[T], m: MaybeSub[Base, T], cbf: CanBuildFrom[C[D[Base]], D[T], That]): That = {
      m match {
        case v @ YesSub() =>
          //origColl map (_ map v.p.apply)
          (cbf() ++= origColl.asInstanceOf[C[D[T]]]) result() //For this to make sense, covariance of C and D is required, as in various other places.
        case NoSub =>
          val clazz = ClassUtil.boxedErasure(tmf)
          val baseResult = map(clazz).asInstanceOf[C[D[Base /*T*/]]]
          val coll = cbf(baseResult)
          for (t <- transitiveQuery(subtypeRel, clazz))
            coll ++= map(t).asInstanceOf[C[D[T]]]
          coll.result()
          //baseResult
      }
    }
  }

  //This must be only used inside the implementation. Mutability fun!
  /*private*/ def groupBySel[A, K, B, Repr <: Traversable[A], That](coll: Repr with Traversable[A])(f: A => K, g: A => B)(implicit cbf: CanBuildFrom[Repr, B, That]): immutable.Map[K, That] = {
    val m = mutable.Map.empty[K, Builder[B, That]]
    for (elem <- coll) {
      val key = f(elem)
      val bldr = m.getOrElseUpdate(key, cbf(coll))
      bldr += g(elem)
    }
    val b = immutable.Map.newBuilder[K, That]
    for ((k, v) <- m)
      b += ((k, v.result()))

    b.result()
  }

  /*private*/ def groupBySelAndForeach[A, K, B, Repr <: TraversableLike[A, Repr], That](coll: Repr with TraversableLike[A, Repr])(f: A => K, g: A => B)(h: K => Unit)(implicit cbf: CanBuildFrom[Repr, B, That]): immutable.Map[K, That] = {
    val m = mutable.Map.empty[K, Builder[B, That]]
    for (elem <- coll) {
      val key = f(elem)
      if (key != null) { //Special
        val bldr = m.getOrElseUpdate(key, cbf(coll))
        bldr += g(elem)
        h(key) //Special
      }
    }
    val b = immutable.Map.newBuilder[K, That]
    for ((k, v) <- m)
      b += ((k, v.result()))

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
  def computeSubTypeRel[T: ClassManifest](seenTypes: collection.Set[Class[_]]): immutable.Map[Class[_], collection.Set[Class[_]]] = {
    //val subtypeRel = mutable.Set.empty[(Class[_], Class[_])]
    val subtypeRel = ArrayBuffer.empty[(Class[_], Class[_])]
    val classesToScan: Queue[Class[_]] = Queue()
    val interfSubtypeRel = ArrayBuffer.empty[(Class[_], Class[_])]
    def add(clazz: Class[_]) {
      /*
      val superTypesClazz = superTypes(clazz)
      classesToScan enqueue (superTypesClazz: _*)
      for (superType <- superTypesClazz)
        subtypeRel += (superType -> clazz) //Map s to its subtypes.
      */
      superClass(clazz) match {
        case Some(superClazz) =>
          classesToScan enqueue superClazz
          subtypeRel += superClazz -> clazz
        case _ =>
      }
      for (superType <- superInterfaces(clazz))
        interfSubtypeRel += (superType -> clazz) //Map s to its subtypes.
    }
    val erasedT = ClassUtil.boxedErasure(classManifest[T])
    for {
      clazz <- seenTypes
      if clazz != erasedT
      s <- superTypes(clazz)
    } {
      add(clazz)
    }
    //For each supertype found, look up its superclasses.
    while (classesToScan.nonEmpty) {
      add(classesToScan.dequeue())
    }
    // Since we look up only concrete types, we needn't represent the subtype relationships between interfaces.
    //However, this optimization is hardly significant since this code takes a quite small amount of time, compared to
    //iterating over the values themselves.
    def addRec(superInterface: Class[_], clazz: Class[_]) {
      subtypeRel += superInterface -> clazz
      for (interf <- superInterfaces(superInterface))
        addRec(interf, clazz)
    }
    for ((superInterface, clazz) <- interfSubtypeRel)
      addRec(superInterface, clazz)
    groupBySel(subtypeRel)(_._1, _._2)(collection.breakOut)
  }
  case class GroupByType[T: ClassManifest, C[+X] <: TraversableLike[X, C[X]], D[+_]](base: Exp[C[D[T]]], f: D[T] => T) extends Arity1OpExp[C[D[T]], TypeMapping[C, D, T],
    GroupByType[T, C, D]](base) {
    override def interpret() = {
      val coll: C[D[T]] = base.interpret()
      val g: D[T] => T = f
      val seenTypes = Set.newBuilder[Class[_]]
      def getType(x: D[T]): Class[_] = {
        val gx = g(x)
        //Why the null check? Remember that (null instanceof Foo) = false. Hence, without using the index, "if (a instanceof Foo)" subsumes
        //a != null. Here we need to do that check otherwise. To avoid a separate filter stage, and since views don't really support groupBy,
        //aggregate nulls into a separate class.
        if (gx != null)
          ClassUtil.primitiveToBoxed(gx.getClass)
        else
          null
      }

      //val map = coll groupBy getType
      val map = groupBySelAndForeach(coll)(getType, identity)(seenTypes += _)
      val subtypeRel = computeSubTypeRel[T](seenTypes.result())

      new TypeMapping[C, D, T](map.asInstanceOf[Map[Class[_], C[D[_]]]], subtypeRel, coll)
    }
    override def copy(base: Exp[C[D[T]]]) = GroupByType[T, C, D](base, f)
  }

  import java.{lang => jl}
  val seenTypesEx: Set[Class[_]] = Set(classOf[jl.Integer], classOf[Null], classOf[AnyRef], classOf[String], classOf[File], classOf[jl.Long], classOf[FileChannel])

  trait PartialApply1Of2[T[+_, +_], A] {
    type Apply[+B] = T[A, B]
    type Flip[+B] = T[B, A]
  }

  class GroupByTupleTypeOps[T: ClassManifest, U: ClassManifest, C[+X] <: TraversableLike[X, C[X]]](val t: Exp[C[(T, U)]]) {
    import Lifting.{GroupByType => _, PartialApply1Of2 => _, _}
    def groupByTupleType1 /*(f: Exp[(T, U)] => Exp[T]) */ = GroupByType[T, C, PartialApply1Of2[Tuple2, U]#Flip](this.t, _._1)
    def groupByTupleType2 /*(f: Exp[(T, U)] => Exp[U]) */ = GroupByType[U, C, PartialApply1Of2[Tuple2, T]#Apply](this.t, _._2)
  }
  implicit def expToGroupByTupleType[T: ClassManifest, U: ClassManifest, C[+X] <: TraversableLike[X, C[X]]](t: Exp[C[(T, U)]]) = new GroupByTupleTypeOps(t)

  test("foo") {
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
  //XXX test also that YesSub/NoSub works. See /Users/pgiarrusso/tmp/foo.scala
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
    import Lifting.{GroupByType => _, PartialApply1Of2 => _, expToGroupByTupleType => _, _}
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
    }
  }
}
