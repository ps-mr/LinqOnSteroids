package ivm.expressiontree

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import collection.{immutable, TraversableLike, mutable}
import mutable.Builder
import collection.generic.CanBuildFrom

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

class TypeTests extends FunSuite with ShouldMatchers {
  private def queryTransitiveClosure[T](map: Map[T, mutable.Set[T]], v: T): mutable.Set[T] = {
    //XXX buggy, we need the transitive closure!
    map(v)
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
          for (t <- queryTransitiveClosure(subtypeRel, IfInstanceOf.getErasure(tmf)))
            //Jumping back and forth from Class[_] to ClassManifest[_] is extremely annoying.
            coll ++= map(ClassManifest.fromClass(t)).asInstanceOf[C[D[T]]]
          coll.result()
          //baseResult
      }
    }
  }

  //This must be only used inside the implementation. Mutability fun!
  private def groupBySel[A, K, B, Repr <: Traversable[A], That](coll: Repr with Traversable[A])(f: A => K, g: A => B)(implicit cbf: CanBuildFrom[Repr, B, That]): immutable.Map[K, That] = {
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
  
  private def groupBySelAndForeach[A, K, B, Repr <: TraversableLike[A, Repr], That](coll: Repr with TraversableLike[A, Repr])(f: A => K, g: A => B)(h: K => Unit)(implicit cbf: CanBuildFrom[Repr, B, That]): immutable.Map[K, That] = {
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

  private def superTypes(c: Class[_]): Seq[Class[_]] = c.getInterfaces :+ c.getSuperclass

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
      val subtypeRel = mutable.Set.empty[(Class[_], Class[_])]
      for {
        t <- seenTypes
        if t != implicitly[ClassManifest[T]] && t != ClassManifest.Null
        t_ = IfInstanceOf.getErasure(t)
        s <- superTypes(t_)
      }
        subtypeRel += (s -> t_) //Map s to its subtypes.

      new TypeMapping[C, D, T](map.asInstanceOf[Map[ClassManifest[_], C[D[_]]]], groupBySel(subtypeRel)(_._1, _._2), coll)
    }
    override def copy(base: Exp[C[D[T]]], f: Exp[D[T]=>T]) = GroupByType[T, C, D](base, f)
  }
}
