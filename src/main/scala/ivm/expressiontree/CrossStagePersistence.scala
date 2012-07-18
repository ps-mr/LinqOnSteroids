package ivm.expressiontree

import collection.mutable.ArrayBuffer

/**
 * User: pgiarrusso
 * Date: 18/7/2012
 */
case class CSPVar(name: String, typ: ClassManifest[_])

//This object is what is shared with Const. The split is done to speed up recompilation.
object CrossStagePersistence {
  private[expressiontree] val map = new ScalaThreadLocal(ArrayBuffer[(Any, CSPVar)]())
  private[expressiontree] val varId = new Util.ThreadLocalIDGenerator

  def addVar[T: ClassManifest](node: Const[T]) = {
    val name = "x" + varId()
    map.get() += (node.x -> CSPVar(name, classManifest[T]))
    name
  }
}
