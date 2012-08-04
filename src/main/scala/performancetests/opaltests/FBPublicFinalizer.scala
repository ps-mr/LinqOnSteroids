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
        classFile ← classFiles.asSmart
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
      } yield classFile,
      for {
        (classFile, method) ← methodsNative()
        if method.name == "finalize" && method.isPublic && method.descriptor.returnType == VoidType && method.descriptor.parameterTypes.size == 0
      } yield classFile
    ) (
      for {
        classFile ← classFiles.asSmart
        method ← classFile.methods
        if method.name ==# "finalize" && method.isPublic && method.descriptor.returnType ==# VoidType && method.descriptor.parameterTypes.size ==# 0
      } yield classFile,
      for {
        classFileMethod ← methodsSQuOpt()
        method ← Let(classFileMethod._2)
        if method.name ==# "finalize" && method.isPublic && method.descriptor.returnType ==# VoidType && method.descriptor.parameterTypes.size ==# 0
      } yield classFileMethod._1
    )
  }
}
