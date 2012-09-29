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

trait FBPublicFinalizer {
  this: FBAnalysesBase =>

  //XXX: for the native version, FI_PUBLIC_SHOULD_BE_PROTECTED is faster; for the LoS version, FI_PUBLIC_SHOULD_BE_PROTECTED-2 is faster.
  //However, the first one should get as fast if I enable the unnesting of Exists.
  def analyzePublicFinalizer() {
    // FINDBUGS: FI: Finalizer should be protected, not public (FI_PUBLIC_SHOULD_BE_PROTECTED)
    benchQueryComplete("FINALIZER_NOT_PROTECTED"){ // FB: "FI_PUBLIC_SHOULD_BE_PROTECTED") {
      for (
        classFile ← classFiles
        if classFile.methods.exists(method ⇒ method.name == "finalize" && method.isPublic && method.descriptor.returnType == VoidType && method.descriptor.parameterTypes.size == 0)
      ) yield classFile
    } {
      import BATLifting._
      for (
        classFile ← classFiles.asSquopt
        if classFile.methods.exists(method ⇒ method.name ==# "finalize" && method.isPublic && method.descriptor.returnType ==# VoidType && method.descriptor.parameterTypes.size ==# 0)
      ) yield classFile
    }
  }

  def analyzePublicFinalizer2() {
    import BATLifting._
    // FINDBUGS: FI: Finalizer should be protected, not public (FI_PUBLIC_SHOULD_BE_PROTECTED)
    benchQueryComplete("FINALIZER_NOT_PROTECTED-2") (
      for {
        classFile ← classFiles
        method ← classFile.methods
        if method.name == "finalize" && method.isPublic && method.descriptor.returnType == VoidType && method.descriptor.parameterTypes.size == 0
      } yield classFile, {
      import schema._
      for {
        MethodRecord(classFile, method) ← methodsNative()
        if method.name == "finalize" && method.isPublic && method.descriptor.returnType == VoidType && method.descriptor.parameterTypes.size == 0
      } yield classFile
    }) (
      for {
        classFile ← classFiles.asSquopt
        method ← classFile.methods
        if method.name ==# "finalize" && method.isPublic && method.descriptor.returnType ==# VoidType && method.descriptor.parameterTypes.size ==# 0
      } yield classFile, {
      import schema.squopt._
      for {
        methodRecord ← methodsSQuOpt()
        if methodRecord.method.name ==# "finalize" && methodRecord.method.isPublic && methodRecord.method.descriptor.returnType ==# VoidType && methodRecord.method.descriptor.parameterTypes.size ==# 0
      } yield methodRecord.classFile
    })
  }
}
