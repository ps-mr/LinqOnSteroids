package ivm
package expressiontree

import collection.mutable

/**
 * User: pgiarrusso
 * Date: 28/9/2012
 */
object ExprTreesWithCSE {
  val transf: Exp[_] => Exp[_] = {
    case e => e
  }
  //Test that this code compiles:
  def tr[T](e: Exp[T]) = e transform transf
  def compile[T](e: Exp[T]): String = {
    /*val symDecls = e __find {
      case _: Sym[_] => true
    } map {
      case SymWithId(defNode, id) => s"val s${id} = ${defNode.toCode}"
      case _ => throw new Throwable()
    } mkString ("\n")
    val constlessE = e transform {
      case c: Const[_] =>
        CrossStagePersistence.persist(c.x)(c.cTag, c.tTag)
      case SymWithId(_, id) => e
      //This has the wrong type and crashes the compiler (?)
        //NamedVar("s" + id)
    }

    val body = constlessE.toCode
    s"""{
    |  ${symDecls}
    |  ${body}
    |}""".stripMargin
    */
    ""
  }
}
