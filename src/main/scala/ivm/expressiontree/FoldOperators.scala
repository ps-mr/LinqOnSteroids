package ivm.expressiontree

import collection.mutable
import mutable.{Buffer, ArrayBuffer}
import ivm.collections.IncArrayBuffer

/**
 * User: pgiarrusso
 * Date: 25/11/2011
 */

object FoldOperators {
  type BinOp[Out, In] = (Out, In) => Out

  trait IncBinOp[Out, In] extends BinOp[Out, In] {
    def remove(o: Out, i: In): Out
    def update(o: Out, oldI: In, newI: In): Out = apply(remove(o, oldI), newI)
  }

  abstract class IncBinOpC[Out, In](f: BinOp[Out, In]) extends IncBinOp[Out, In] {
    def apply(o: Out, i: In): Out = f(o, i)
  }

  def reverse[Out, In](op: IncBinOp[Out, In]) = new IncBinOp[Out, In] {
    override def remove(o: Out, i: In): Out = op.apply(o, i)
    override def apply(o: Out, i: In): Out = op.remove(o, i)
  }

  //Allow reusing an IncBinOp to build expression nodes. Not sure yet how useful this is...
  case class ExpBinOp[Out, In](a: Exp[Out], b: Exp[In], op: BinOp[Out, In]) extends BinaryOpExp[Out, In, Out](a, b) {
    def interpret() = op(a.interpret(), b.interpret())
    def copy(a: Exp[Out], b: Exp[In]) = ExpBinOp(a, b, op)
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

  /*private def convertBinFunInternal[T](f: (Exp[T], Exp[T]) => Exp[T]): Exp[((T, T)) => T] = FuncExp(f.tupled compose Lifting.unliftPair)
  private def convertBinFun[T](f: (Exp[T], Exp[T]) => Exp[T]): ((T, T)) => T = convertBinFunInternal(f).interpret()*/

  //It creates autocompletion disasters!
  //implicit def convToSome[T] = Some[T] _

  def treeReduce[T](coll: Exp[Traversable[T]])(f: (T, T) => T) = //: Exp[T] =
    TreeFold[T](coll, f)

  def treeFold[T](coll: Exp[Traversable[T]])(z: T, f: (T, T) => T) = //: Exp[T] =
    TreeFold[T](coll, f, Some(z))

  def foldl[Out, In](coll: Exp[Traversable[In]])(f: IncBinOpC[Out, In], z: Out) = Foldl(coll, f, z)
  case class Foldl[Out, In](coll: Exp[Traversable[In]], f: IncBinOpC[Out, In], z: Out) extends UnaryOpExp[Traversable[In], Out](coll) with TravMsgSeqSubscriber[In, Traversable[In]] with MsgSeqPublisher[Out] {
    var res: Out = _
    override def interpret() = {
      res = coll.interpret().foldLeft(z)(f)
      res
    }

    override def copy(coll: Exp[Traversable[In]]) = Foldl(coll, f, z)
    override def notify(pub: Traversable[In], evts: Seq[Message[Traversable[In]]]) {
      for (evt <- evts) {
        evt match {
          case Include(v) =>
            res = f(res, v)
          case Remove(v) =>
            res = f.remove(res, v)
          case Update(oldV, newV) =>
            res = f.update(res, oldV, newV)
          case Reset =>
            res = z
        }
      }
    }
  }
  //TODO: Forall and exists only require counting the number of false (respectively, true) values, and they are implementable in term of one another, given a notification-propagating Not.

  //Here I accept a primitive function because I believe the overhead for expression trees would be too significant, especially with all the wrapping and unwrapping done by convertBinFunInternal.
  //However, normalization-by-evaluation and a two-argument version of FuncExpInt could come to the rescue!
  case class TreeFold[T](coll: Exp[Traversable[T]], f: (T, T) => T, z: Option[T] = None) extends UnaryOpExp[Traversable[T], T](coll) with TravMsgSeqSubscriber[T, Traversable[T]] with MsgSeqPublisher[T] { //BinaryOpExp[Traversable[T], (T, T) => T, T](coll, f) {
    private var tree: Buffer[Buffer[T]] = _
    var res: T = _
    override def interpret() = {
      //Do we actually need to recompute the tree? Won't it have been updated by IVM? Hmm... you never know... especially since this is not IncrementalResult and it thus doesn't include the needed machinery for listener setup!
      val (lTree, lRes) = treeFold(coll.interpret().toBuffer)(z, f)
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
            //XXX: this code is much trickier than functional code and than most other code I have. Try to find a way to simplify it!
            tree(0) += v
            var i = 0
            while (i + 1 < tree.size) {
              if (tree(i).size % 2 == 1) {
                tree(i + 1) += v
              } else {
                val lastIdx = tree(i + 1).size - 1
                tree(i + 1)(lastIdx) = f(tree(i + 1)(lastIdx), v)
              }
              /*
              //Alternative, if the neutral element is always there:
              //{{{
              val destPos = tree(i).size / 2 - 1
              tree(i + 1)(destPos) = f(tree(i + 1).orElse[Int, T]{ case _ => z.get }(destPos), v)
              //}}}
              */
              i += 1
            }
            assert(tree(i).size <= 2)
            if (tree(i).size == 2) {
              val pair = tree(i)
              tree += Buffer(f(pair(0), pair(1)))
            }
            //XXX: Remove requires an underlying bag!
          case _ =>
        }
      }
      res = tree.last.headOption.orElse(z).get

      publish(UpdateEl(oldRes, res))
    }
  }

  //Mmmh, for folds we might care about bags, to allow for repeated elements!
  def treeFold[T](coll: Buffer[T])(z: Option[T] = None, f: (T, T) => T): (Buffer[Buffer[T]], T) = {
    var layer: Buffer[T] = coll
    var tree = new ArrayBuffer[Buffer[T]]
    tree += layer
    while (layer.size >= 2) {
      layer = layer.grouped(2).map(pair => if (pair.size == 2) f(pair(0), pair(1)) else pair(0)).toBuffer
      tree += layer
    }
    (tree, layer.headOption.orElse(z).get)
  }

  def main(args: Array[String]) {
    import Lifting._
    for (n <- 1 to 17) {
      val (_, res) = treeFold((1 to n).toBuffer)(Some(0), _ + _)
      assert(res == (1 to n).sum)
      println("treeFold(1 .. %d)(0, +) == %d" format (n, res))
      val res2 = treeReduce((1 to n): Exp[Traversable[Int]])(_ + _)
      println(res2)
      assert(res2.interpret() == (1 to n).sum)
      println("Exp.treeFold(1 .. %d) == %d" format (n, res2.interpret()))
      println()
    }
    {
      val incBuf: IncArrayBuffer[Int] = IncArrayBuffer(1)
      //val res = treeReduce(incBuf.asQueryable)(_ + _) //Does work
      //val res = treeReduce[Int](incBuf)(_ + _) //Does not work
      val res = treeReduce(incBuf: Exp[Traversable[Int]])(_ + _) //Does work
      incBuf.subscribe(res)
      res.interpret()
      assert(res.res == 1)
      for (n <- 2 to 17) {
        incBuf += n
        println("Adding n = %d, res.res = %d" format (n, res.res))
        assert(res.res == (1 to n).sum)
      }
    }
    { //Won't work without a neutral element, so here I supply it.
      val incBuf: IncArrayBuffer[Int] = IncArrayBuffer.empty[Int]
      //val res = treeReduce(incBuf.asQueryable)(_ + _) //Does work
      //val res = treeReduce[Int](incBuf)(_ + _) //Does not work
      val res = treeFold(incBuf: Exp[Traversable[Int]])(0, _ + _) //Does work
      incBuf.subscribe(res)
      res.interpret()
      for (n <- 1 to 17) {
        incBuf += n
        assert(res.res == (1 to n).sum)
      }
    }
  }
}
