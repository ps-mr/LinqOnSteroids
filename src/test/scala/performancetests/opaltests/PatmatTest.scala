package performancetests.opaltests

import ivm._
import expressiontree._
import Lifting._
import de.tud.cs.st.bat.resolved._
import BATLifting._
import InstructionLifting._
//import de.tud.cs.st.bat.resolved.reader.Java6Framework._


/**
 * User: pgiarrusso
 * Date: 3/8/2012
 */

object PatmatTest {
  //Attempt 1:
  /*
  object __match {
    def one[T](x: Exp[T]): Exp[Option[T]] = Some(x)
    def zero: Exp[Option[Nothing]] = None
    def guard[T](cond: Exp[Boolean], thenBranch: => Exp[T]): Exp[Option[T]] = if_# (cond) { Some(thenBranch) } else_# { None }
    def runOrElse[T, U](in: Exp[T])(matcher: Exp[T] => Exp[Option[U]]): Exp[U] = matcher(in).getOrElse(
      fmap(in, 'throwMatchError)('MatchError, in => throw new MatchError(in)))
  }

  object INSTANCEOF {
    def unapply(t: Exp[_]): Exp[Option[ReferenceType]] = {
      if (t ne null)
        t.ifInstanceOf[INSTANCEOF].map(_.referenceType).asInstanceOf[Exp[Option[ReferenceType]]] //XXX will fail at runtime.
      else None
    }
  }
  */

  //Attempt 1.5, almost works, and matches the code in mainline. Almost.
  type MatchMonad[+T] = Exp[Option[T]]
  object __match {
    def one[T](x: Exp[T]): MatchMonad[T] = Some(x)
    def zero: MatchMonad[Nothing] = None
    def guard[T](cond: Exp[Boolean], thenBranch: => Exp[T]): MatchMonad[T] = if_# (cond) { Some(thenBranch) } else_# { None }
    def runOrElse[T, U](in: Exp[T])(matcher: Exp[T] => MatchMonad[U]): Exp[U] = matcher(in).getOrElse(
      fmap(in, 'throwMatchError)('MatchError, in => throw new MatchError(in)))
  }

  /*
  //Attempt 2
  case class MatchMonad[+T](x: Exp[Option[T]]) extends Exp[Option[T]] {
    def children = x.children
    override def checkedGenericConstructor = ??? //x.checkedGenericConstructor
    def nodeArity = x.nodeArity
    def interpret() = x.interpret()
  }

  object __match {
    def one[T](x: Exp[T]): MatchMonad[T] = MatchMonad(Some(x))
    def zero: MatchMonad[Nothing] = MatchMonad(None)
    def guard[T](cond: Exp[Boolean], thenBranch: => Exp[T]): MatchMonad[T] = MatchMonad(if_# (cond) { Some(thenBranch) } else_# { None })
    def runOrElse[T, U](in: Exp[T])(matcher: Exp[T] => MatchMonad[U]): Exp[U] = matcher(in).getOrElse(
      fmap(in, 'throwMatchError)('MatchError, in => throw new MatchError(in)))
  }

  object INSTANCEOF {
    def unapply(t: Exp[_]): MatchMonad[ReferenceType] = {
      MatchMonad(if (t ne null)
        t.ifInstanceOf[INSTANCEOF].map(_.referenceType).asInstanceOf[MatchMonad[ReferenceType]] //XXX will fail at runtime.
      else None)
    }
  }
  */

  val testdata: Set[ClassFile] = ???
  val queryData = asExp(testdata)

  val baseCol = Seq(1).asSmart
  val query = for (i <- baseCol.typeFilter[Int]; j <- Let(i) if j % 2 ==# 1) yield j
  val query2 = for (i <- baseCol.typeFilter[Int]; j <- asExp(Some(i)) if j % 2 ==# 1) yield j

  def test = 7 match {
    case 5 => "foo"
    case _ => "bar"
  }

  def f1(i: Exp[Instruction]) = {
  //def f1(i: Instruction) = {
    i match {
      case INSTANCEOF(_) => 1
      //case _ =>
    }
  }

  /*val methodsLos1 =
    for {
      cf <- queryData
      m <- cf.methods
      mBody <- m.body
      INSTANCEOF(_) <- mBody.instructions
    } yield m.name*/
}
