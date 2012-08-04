package performancetests
package opaltests

import ivm._
import expressiontree._
import Lifting._
import optimization.Optimization

import de.tud.cs.st.bat
import bat.resolved._
import analyses._

import reader.Java6Framework
import analyses.ClassHierarchy

import collection.immutable.Seq
import collection.{Seq => CSeq}

/**
 * User: pgiarrusso
 * Date: 4/8/2012
 */

trait FBAbstractClassesThatDefinesCovariantEquals {
  this: FBAnalysesBase =>

  def analyzeAbstractClassesThatDefinesCovariantEquals() {
    //XXX this was changed in BAT.
    benchQueryComplete("COVARIANT_EQUALS") { // FB: EQ_ABSTRACT_SELF") {
      for {
        classFile ← classFiles
        method @ Method(_, "equals", MethodDescriptor(CSeq(parameterType), BooleanType), _) ← classFile.methods
        if method.isAbstract && parameterType == classFile.thisClass //!= ObjectType.Object
      } yield (classFile, method)
    } {
      import BATLifting._
      for {
        classFile ← classFiles.asSmart
        method ← classFile.methods
        if method.isAbstract && method.name ==# "equals" && method.descriptor.returnType ==# BooleanType
        parameterTypes <- Let(method.descriptor.parameterTypes)
        if parameterTypes.length ==# 1 && parameterTypes(0) ==# classFile.thisClass //parameterTypes(0) !=# ObjectType.Object
      } yield (classFile, method)
    }
  }
}
