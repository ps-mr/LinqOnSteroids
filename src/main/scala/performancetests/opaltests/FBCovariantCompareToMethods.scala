package performancetests
package opaltests

import ivm._
import expressiontree._
import Lifting._

import de.tud.cs.st.bat
import bat.resolved._

import collection.immutable.Seq
import collection.{Seq => CSeq}

/**
 * User: pgiarrusso
 * Date: 4/8/2012
 */

trait FBCovariantCompareToMethods {
  this: FBAnalysesBase =>

  def analyzeCovariantCompareToMethods() {
    val comparableType = ObjectType("java/lang/Comparable")
    benchQueryComplete("COVARIANT_COMPARETO"){// FB: "CO_SELF_NO_OBJECT/CO_ABSTRACT_SELF") {
      // Weakness: In a project, where we extend a predefined class (of the JDK) that
      // inherits from Comparable and in which we define covariant comparesTo method,
      // we will not be able to identify this issue unless we have identified the whole
      // class hierarchy.
      for {
        allComparables ← classHierarchy.subtypes(comparableType).toList
        comparable ← allComparables
        classFile ← getClassFile.get(comparable).toList
        method @ Method(_, "compareTo", MethodDescriptor(CSeq(parameterType), IntegerType), _) ← classFile.methods if parameterType != ObjectType.Object
      } yield (classFile, method)
    } {
      import BATLifting._
      for {
        allComparables ← classHierarchy.subtypes(comparableType).toList.asSmart
        comparable ← allComparables
        classFile ← getClassFile.get(comparable) //getClassFile is lifted through Const and makes optimization expensive.
        method ← classFile.methods //if parameterType != ObjectType.Object
        if method.name ==# "compareTo" && method.descriptor.returnType ==# IntegerType
        parameterTypes <- Let(method.descriptor.parameterTypes)
        if parameterTypes.length ==# 1 && parameterTypes(0) !=# ObjectType.Object

      } yield (classFile, method)
    }
  }
}
