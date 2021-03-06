/*
package ivm.expressiontree

import collection.{immutable, TraversableLike, mutable}
import mutable.{Buffer, ArrayBuffer}
import ivm.collections.IncArrayBuffer
import Util.ExtraImplicits._

/**
 * User: pgiarrusso
 * Date: 25/11/2011
 */

trait FoldOperators {
  type BinOp[Out, In] = (Out, In) => Out

  trait IncBinOp[Out, In] extends BinOp[Out, In] {
    def remove(o: Out, i: In): Out
    def update(o: Out, oldI: In, newI: In): Out = apply(remove(o, oldI), newI)
  }

  abstract class IncBinOpC[Out, In](f: BinOp[Out, In]) extends IncBinOp[Out, In] {
    def apply(o: Out, i: In): Out = f(o, i)
  }

  //Allow reusing an IncBinOp to build expression nodes. Not sure yet how useful this is...
  case class ExpBinOp[Out, In](a: Exp[Out], b: Exp[In], op: BinOp[Out, In]) extends Arity2OpExp[Out, In, Out, ExpBinOp[Out, In]](a, b) {
    def interpret() = op(a.interpret(), b.interpret())
    def copy(a: Exp[Out], b: Exp[In]) = ExpBinOp(a, b, op)
  }

  /*private def convertBinFunInternal[T](f: (Exp[T], Exp[T]) => Exp[T]): Exp[((T, T)) => T] = Fun(f.tupled compose Lifting.unliftPair)
  private def convertBinFun[T](f: (Exp[T], Exp[T]) => Exp[T]): ((T, T)) => T = convertBinFunInternal(f).interpret()*/

  //It creates autocompletion disasters!
  //implicit def convToSome[T] = Some[T] _

  /*def treeReduce[T](coll: Exp[Traversable[T]])(f: (T, T) => T) = //: Exp[T] =
    TreeFold[T](coll, f)*/

  def treeFold[T](coll: Exp[Traversable[T]])(z: T, f: (T, T) => T) = //: Exp[T] =
    TreeFold[T](coll, f, z)

  def foldl[Out, In](coll: Exp[Traversable[In]])(f: IncBinOpC[Out, In], z: Out) = Foldl(coll, f, z)

  case class Foldl[Out, In](coll: Exp[Traversable[In]], f: IncBinOpC[Out, In], z: Out)
    extends Arity1OpExp[Traversable[In], Out, Foldl[Out, In]](coll) with EvtTransformerEl[Traversable[In], Out, Traversable[In]]
    with CachingExp[Out]
  {
    override def interpret() = {
      //XXX: we should get the initial status otherwise. When we'll get notifications about the existing elements, this will become wrong.
      val res = coll.interpret().foldLeft(z)(f)
      cache = Some(res)
      res
    }

    override def copy(coll: Exp[Traversable[In]]) = Foldl(coll, f, z)
    override def notifyEv(pub: Traversable[In], evt: Message[Traversable[In]]) {
      cache = Some(
        evt match {
          case Include(v) =>
            f(cache.get, v)
          case Remove(v) =>
            f.remove(cache.get, v)
          case Update(oldV, newV) =>
            f.update(cache.get, oldV, newV)
          case Reset =>
            z
          case _ => //Should not be possible
            throw new IllegalArgumentException
        })
    }
  }

  //Here I accept a primitive function because I believe the overhead for expression trees would be too significant, especially with all the wrapping and unwrapping done by convertBinFunInternal.
  //However, normalization-by-evaluation and a two-argument version of FunInterp could come to the rescue!
  case class TreeFold[T](coll: Exp[Traversable[T]], f: (T, T) => T, z: T) extends Arity1OpExp[Traversable[T], T, TreeFold[T]](coll) with TravMsgSeqSubscriber[T, Traversable[T]] with MsgSeqPublisher[T, Exp[T]] { //Arity2OpExp[Traversable[T], (T, T) => T, T](coll, f) {
    private def combineIfAvailable(arr: Buffer[T], i: Int) =
      if (i + 1 < arr.size)
        f(arr(i), arr(i + 1))
      else
        arr(i)

    private var tree: Buffer[Buffer[T]] = _
    private var positions: mutable.Map[T, Buffer[Int]] = _
    private var freePositions: ArrayBuffer[Int] = new ArrayBuffer
    var res: T = _
    override def interpret() = {
      //Do we actually need to recompute the tree? Won't it have been updated by IVM? Hmm... you never know... especially since this is not IncrementalResult and it thus doesn't include the needed machinery for listener setup!
      val interpColl = coll.interpret().toBuffer
      val (lTree, lRes) = treeFold(interpColl)(z, f)
      //XXX: It would be nice to be able to write something like this, and it should be possible, but unfortunately it isn't. Report it as a Scala bug!
      //var positions: Map[T, Int] = interpColl.zipWithIndex(Map.canBuildFrom[T, Int])
      //What is accepted instead is this:
      //var positions: Map[T, Int] = interpColl.zipWithIndex.toMap
      //But actually, we need the list of positions, i.e. this ugly statement:
      //positions = mutable.HashMap.empty ++ interpColl.zipWithIndex.groupBy(_._1).map(v => (v._1, v._2.map(_._2)))
      positions = mutable.HashMap.empty ++ interpColl.zipWithIndex.groupBySel(_._1, _._2)
      tree = lTree
      res = lRes
      res
    }
    override def copy(coll: Exp[Traversable[T]]) = TreeFold(coll, f, z)
    override def notify(pub: Traversable[T], evts: Seq[Message[Traversable[T]]]) {
      val oldRes = res
      for (evt <- evts) {
        evt match {
          case Include(v) =>
            val newPos = if (freePositions.nonEmpty) {
              val pos = freePositions.head
              freePositions.remove(0)
              tree(0)(pos) = v
              updateTreeFromPos(pos, update = true)
              pos
            } else {
              //XXX: this code is much trickier than functional code and than most other code I have. Try to find a way to simplify it!
              val insertPos = tree(0).size
              tree(0) += v
              //updateTreeFromPos(insertPos, update = false)
              ///*
              for (i <- 0 until tree.size - 1) {
                if (tree(i).size % 2 == 1 && (tree(i).size + 1) / 2 > tree(i + 1).size) {
                  tree(i + 1) += tree(i).last
                } else {
                  val lastIdx = tree(i + 1).size - 1
                  tree(i + 1)(lastIdx) = combineIfAvailable(tree(i), 2 * lastIdx)
                }
              }
              //*/
              val lastLevel = tree.size - 1
              assert(tree(lastLevel).size <= 2)
              if (tree(lastLevel).size == 2) {
                val pair = tree(lastLevel)
                tree += Buffer(f(pair(0), pair(1)))
              }
              insertPos
            }
            positions.getOrElseUpdate(v, new ArrayBuffer[Int]()) += newPos
          //Remove requires an underlying bag to find the element, or an additional map, like done here.
          //XXX: implement a simpler Remove for Bags
          case Remove(v) =>
            val pos = positions(v).head
            positions(v).remove(0)
            tree(0)(pos) = z
            freePositions += pos
            updateTreeFromPos(pos, update = true)
          case _ =>
        }
      }
      res = tree.last.headOption.getOrElse(z)

      publish(UpdateVal(oldRes, res))
    }

    def updateTreeFromPos(_pos: Int, update: Boolean) {
      var currPos = _pos
      for (i <- 0 until tree.size - 1) {
        currPos = currPos / 2
        val newVal = combineIfAvailable(tree(i), 2 * currPos)

        if (update || tree(i + 1).size > currPos)
          tree(i + 1)(currPos) = newVal
        else
          tree(i + 1) += newVal
      }
    }
  }

  //Mmmh, for folds we might care about bags, to allow for repeated elements!
  def treeFold[T](coll: Buffer[T])(z: T, f: (T, T) => T): (Buffer[Buffer[T]], T) = {
    var layer: Buffer[T] = coll
    var tree = new ArrayBuffer[Buffer[T]]
    tree += layer
    while (layer.size >= 2) {
      layer = layer.grouped(2).map(pair => if (pair.size == 2) f(pair(0), pair(1)) else pair(0)).toBuffer
      tree += layer
    }
    (tree, layer.headOption.getOrElse(z))
  }
}

object FoldOperators extends FoldOperators

object FoldOperatorsMain {
  import FoldOperators._

  def reverse[Out, In](op: IncBinOp[Out, In]) = new IncBinOp[Out, In] {
    override def remove(o: Out, i: In): Out = op.apply(o, i)
    override def apply(o: Out, i: In): Out = op.remove(o, i)
  }

  val And: BinOp[Boolean, Boolean] = (_ && _)
  val Or: BinOp[Boolean, Boolean] = (_ || _)
  val Xor = new IncBinOpC[Boolean, Boolean](_ ^ _) {
    override def remove(o: Boolean, i: Boolean): Boolean = o ^ i
  }
  def Plus[T](implicit num: Numeric[T]) = new IncBinOpC[T, T](num.plus(_, _)) {
    override def remove(o: T, i: T): T = num.minus(o, i)
  }
  def Minus[T: Numeric] = reverse(Plus[T])
  def Times[T](implicit num: Numeric[T]): BinOp[T, T] = num.times(_, _)

  def Times[T](implicit num: Fractional[T]) = new IncBinOpC[T, T](num.times(_, _)) {
    override def remove(o: T, i: T): T = num.div(o, i)
  }
  def Div[T: Fractional] = reverse(Times[T])

  /*
  def main(args: Array[String]) {
    import Lifting._
    for (n <- 1 to 5) {
      val coll = Seq.fill(n)((math.random * 2).toInt)
      val collQ = coll.asSquopt
      println(coll)
      val res = coll.forall(_ % 2 == 0)
      val res2 = coll.exists(_ % 2 == 0)
      println("%s %s" format (res, res2))
      val query = collQ forall (_ % 2 ==# 0)
      val query2 = collQ exists (_ % 2 ==# 0)
      println(query)
      println(query2)
      assert(query.interpret() == res)
      assert(query2.interpret() == res2)
    }
    for (n <- 1 to 17) {
      val (_, res) = treeFold((1 to n).toBuffer)(0, _ + _)
      assert(res == (1 to n).sum)
      println("treeFold(1 .. %d)(0, +) == %d" format (n, res))
      val res2 = treeFold((1 to n): Exp[Traversable[Int]])(0, _ + _)
      println(res2)
      assert(res2.interpret() == (1 to n).sum)
      //println("Exp.treeFold(1 .. %d) == %d" format (n, res2.interpret()))
      //println()
    }
    {
      val incBuf: IncArrayBuffer[Int] = IncArrayBuffer(1)
      //val res = treeReduce(incBuf.asQueryable)(_ + _) //Does work
      //val res = treeReduce[Int](incBuf)(_ + _) //Does not work
      val res = treeFold(incBuf: Exp[Traversable[Int]])(0,  _ + _) //Does work
      incBuf.addSubscriber(res)
      res.interpret()
      assert(res.res == 1)
      for (n <- 2 to 17) {
        incBuf += n
        //println("Adding n = %d, res.res = %d" format (n, res.res))
        assert(res.res == (1 to n).sum)
      }
    }
    { //Won't work without a neutral element, so here I supply it.
      val incBuf: IncArrayBuffer[Int] = IncArrayBuffer.empty[Int]
      //val res = treeReduce(incBuf.asQueryable)(_ + _) //Does work
      //val res = treeReduce[Int](incBuf)(_ + _) //Does not work
      val res = treeFold(incBuf: Exp[Traversable[Int]])(0, _ + _) //Does work
      incBuf.addSubscriber(res)
      res.interpret()
      for (n <- 1 to 17) {
        incBuf += n
        assert(res.res == (1 to n).sum)
        incBuf -= 1
        assert(res.res == (2 to n).sum)
        incBuf += n
        assert(res.res == (2 to n).sum + n)
        incBuf -= n
        assert(res.res == (2 to n).sum)
        incBuf += 1
        assert(res.res == (1 to n).sum)
      }
    }
    {
      val incBuf: IncArrayBuffer[Int] = IncArrayBuffer.empty[Int]
      //val coll = Buffer.fill(n)((math.random * 2).toInt)
      //println(coll)
      //val res = incBuf.forall(_ % 2 == 0)
      //println(res)
      val incBufQ = incBuf.asQueryable
      val query = incBufQ forall (_ % 2 ==# 0)
      val query2 = incBufQ exists (_ % 2 ==# 0)
      incBuf.addSubscriber(query)
      val query2Content = query2.t1.asInstanceOf[Forall[Int]]
      incBuf.addSubscriber(query2Content)
      //XXX this is disabled because IVM support for Not was disabled
      //query2Content.addSubscriber(query2)
      println(query)
      for (n <- 0 to (4, 2)) {
        incBuf += n
        assert(query.value == incBuf.forall(_ % 2 == 0))
        assert(query2.value == incBuf.exists(_ % 2 == 0))
        println("%s, forall even: %s, exists even: %s" format (incBuf, query.value(), query2.value()))
      }
      for (n <- 1 to (5, 2)) {
        incBuf += n
        assert(query.value == incBuf.forall(_ % 2 == 0))
        assert(query2.value == incBuf.exists(_ % 2 == 0))
        println("%s, forall even: %s, exists even: %s" format (incBuf, query.value(), query2.value()))
      }
      incBuf.clear()
      assert(query.value == incBuf.forall(_ % 2 == 0))
      assert(query2.value == incBuf.exists(_ % 2 == 0))
      println("%s, forall even: %s, exists even: %s" format (incBuf, query.value(), query2.value()))
      for (n <- 1 to 5) {
        incBuf += (math.random * 4).toInt
        assert(query.value == incBuf.forall(_ % 2 == 0))
        assert(query2.value == incBuf.exists(_ % 2 == 0))
        println("%s, forall even: %s, exists even: %s" format (incBuf, query.value(), query2.value()))
      }
    }
  }
  */
}
*/
