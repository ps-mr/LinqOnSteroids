package ivm.expressiontree

import collection.mutable
import collection.mutable.ArrayBuffer

/**
 * User: pgiarrusso
 * Date: 17/7/2012
 */

trait Compiled[T] {
  def result: T
}

case class CSPVar(name: String, typ: String)

object Compile {
  private val codeCache = new ScalaThreadLocal(mutable.Map[String, Class[_]]())
  private val map = new ScalaThreadLocal(ArrayBuffer[(Any, CSPVar)]())
  def addVar[T: ClassManifest](node: Const[T]) = {
    val name = "x" + varId()
    map.get() += (node.x -> CSPVar(name, classManifest[T].toString))
    name
  }
  val classId = new Util.GlobalIDGenerator
  //private val varId = new ScalaThreadLocal(0)
  val varId = new Util.ThreadLocalIDGenerator

  //Just (or mostly?) for testing.
  def reset() {
    codeCache.get().clear()
    map.get().clear()
    varId.localReset()
    classId.reset()
  }

  def compile[T: ClassManifest](e: Exp[T]) = {
    val name = "Outclass" + classId()
    val typ = classManifest[T]
    map.get().clear()
    val body = e.toCode
    val declValues = (map.get().toSeq map {
      case (value, CSPVar(memberName, memberType)) =>
        ("val %s: %s" format (memberName, memberType), value)
    })
    val (decls, values) = declValues.unzip
    val declsStr = decls mkString ", "
    val res =
      """class %s(%s) extends Compiled[%s] {
        |  override def result = %s
        |}""".stripMargin format (name, declsStr, typ, body)
    //Compile res and cache the result.
    res
  }
}
