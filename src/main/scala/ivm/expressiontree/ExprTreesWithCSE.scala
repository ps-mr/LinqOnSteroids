package ivm.expressiontree

import collection.mutable

/**
 * User: pgiarrusso
 * Date: 28/9/2012
 */
object ExprTreesWithCSE {
  trait DefTransformer {
    def apply[T](e: Def[T]): Def[T]
  }
  object DefTransformer {
    def apply(f: Def[_] => Def[_]) = new DefTransformer {
      def apply[T](e: Def[T]): Def[T] = f(e).asInstanceOf[Def[T]]
    }
  }

  trait TreeNode[+T] {
    def interpret(): T
    def children: List[Exp[_]]
  }

  sealed trait Exp[+T] extends TreeNode[T] {
    def transform(transformer: Def[_] => Def[_]): Exp[T] =
      transform(DefTransformer(transformer))
    def transform(transformer: DefTransformer): Exp[T]
  }
  def definitions: mutable.Map[Def[_], Sym[_]] = new mutable.HashMap()
  implicit def toAtom[T](d: Def[T]): Exp[T] =
    definitions.asInstanceOf[mutable.Map[Def[T], Sym[T]]].getOrElseUpdate(d, Sym[T](d))//.asInstanceOf[Sym[T]]

  case class Sym[+T] private[ExprTreesWithCSE](d: Def[T]) extends Exp[T] {
    def interpret() = d.interpret()
    def children = d.children
    def transform(transformer: DefTransformer): Exp[T] = {
      val transformedChildren = children mapConserve (_ transform transformer)
      val newSelf =
        if (transformedChildren eq children)
          d
        else
          d genericConstructor transformedChildren
      transformer(newSelf)
      //transformer(d.genericConstructor(children mapConserve (_ transform transformer)))
    }
  }
  case class Const[T](t: T) extends Exp[T] {
    def interpret() = t
    def children: List[Exp[_]] = Nil
    def transform(transformer: DefTransformer): Exp[T] = this
  }

  trait Def[+T] extends TreeNode[T] {
    def nodeArity: Int

    /*private[ivm]*/ def children: List[Exp[_]]
    protected def checkedGenericConstructor(v: List[Exp[_]]): Def[T]

    /*private[ivm]*/ def genericConstructor(v: List[Exp[_]]): Def[T] =
      if (v.length == nodeArity)
        checkedGenericConstructor(v)
      else
        throw new IllegalArgumentException()

    //def children: Seq[Exp[_]]
  }
}
