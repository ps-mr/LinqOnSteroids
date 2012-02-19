package ivm.expressiontree
import Util.ExtraImplicits._

object Fix {
  def fix[A](f: A => A, v: A): A = {
    var newV = v
    var curr = newV
    do {
      curr = newV
      newV = f(curr)
    } while (newV != curr)
    newV
  }
}

//Exploration into the design space.
case class Fix[T](col: Exp[Traversable[T]], f: FuncExp[Traversable[T], Traversable[T]])
  extends Arity2Op[Exp[Traversable[T]], FuncExp[Traversable[T], Traversable[T]], Traversable[T], Fix[T]](col, f)
{
  def copy(col: Exp[Traversable[T]], f: FuncExp[Traversable[T], Traversable[T]]) = Fix(col, f)
  override def interpret() = {
    // XXX: very very basic implementation of fixpoint computation
    Fix.fix(f.interpret(), col.interpret())
  }
}

case class TransitiveClosure[T](rel: Exp[Traversable[(T, T)]]) extends UnaryOpExp[Traversable[(T, T)], Traversable[(T, T)], TransitiveClosure[T]](rel) {
  def copy(rel: Exp[Traversable[(T, T)]]) = TransitiveClosure(rel)
  def interpret() = {
    //XXX: crappy (and untested) implementation, with complexity O(n^3) per fixpoint iteration, worse than Floyd-Warshall (O(n^3) overall).
    //There's tons of research here: http://ftp.cs.wisc.edu/pub/techreports/1988/TR765.pdf
    val relInt = rel.interpret().toSet
    val idx = relInt.groupBySel(_._1, _._2)
    def close(rel: Set[(T, T)]) =
      rel ++ (for {
        (a, b) <- rel
        c <- idx.getOrElse(b, Set.empty)
      } yield (a, c))
    Fix.fix(close _, relInt)
  }
}
