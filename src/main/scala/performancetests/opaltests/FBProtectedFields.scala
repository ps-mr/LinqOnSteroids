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
import bat.resolved.analyses.ClassHierarchy

import collection.immutable.Seq
import collection.{Seq => CSeq}

/**
 * User: pgiarrusso
 * Date: 4/8/2012
 */

trait FBProtectedFields {
  this: FBAnalysesBase =>

  def analyzeProtectedFields() {
    import BATLifting._
    // FINDBUGS: CI: Class is final but declares protected field (CI_CONFUSED_INHERITANCE) // http://code.google.com/p/findbugs/source/browse/branches/2.0_gui_rework/findbugs/src/java/edu/umd/cs/findbugs/detect/ConfusedInheritance.java
    benchQueryComplete("PROTECTED_FIELD") ( //FB:"CI_CONFUSED_INHERITANCE") {
      for {
        classFile ← classFiles if classFile.isFinal
        field ← classFile.fields if field.isProtected
      } yield (classFile, field),
      for {
        (classFile, field) <- fieldsNative() if classFile.isFinal
        if field.isProtected
      } yield (classFile, field)
    ) (
      for {
        classFile ← classFiles.asSmart if classFile.isFinal
        field ← classFile.fields if field.isProtected
      } yield (classFile, field),
      for {
        classFileField <- fieldsSQuOpt()
        classFile <- Let(classFileField._1)
        field <- Let(classFileField._2)
        if classFile.isFinal
        if field.isProtected
      } yield (classFile, field)
    )
  }
}
