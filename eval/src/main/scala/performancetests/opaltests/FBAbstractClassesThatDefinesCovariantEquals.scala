package performancetests
package opaltests

import de.tud.cs.st.bat.resolved._

/**
 * User: pgiarrusso, Ralf Mitschke
 * Date: 4/8/2012
 */
// EQ_ABSTRACT_SELF
trait FBAbstractClassesThatDefinesCovariantEquals {
  this: FBAnalysesBase =>


  private def analyzeBaseWithoutAbstractions() = {
    import collection.{Seq => CSeq}
    for {
      classFile ← classFiles
      method @ Method(_, "equals", MethodDescriptor(CSeq(parameterType), BooleanType), _) ← classFile.methods
      if method.isAbstract && parameterType == classFile.thisClass //!= ObjectType.Object
    } yield (classFile.thisClass, method.name, method.descriptor)
  }

  private def analyzeSQuOptWithoutAbstractions() = {
    import de.tud.cs.st.bat.resolved._
    import ivm._
    import expressiontree._
    import Lifting._
    import BATLifting._
    import performancetests.opaltests.InstructionLifting._
    import ivm.expressiontree.Util.ExtraImplicits._
    import schema.squopt._
    for {
      classFile ← classFiles.asSquopt
      method ← classFile.methods
      if method.isAbstract && method.name ==# "equals" && method.descriptor.returnType ==# BooleanType
      parameterTypes <- Let(method.descriptor.parameterTypes)
      if parameterTypes.length ==# 1 && parameterTypes(0) ==# classFile.thisClass //parameterTypes(0) !=# ObjectType.Object
    } yield (classFile.thisClass, method.name, method.descriptor)
  }


  private def analyzeBaseWithAbstractions() = {
    import collection.{Seq => CSeq}
    import schema._
    for { MethodRecord(classFile, method @ Method(_, "equals", MethodDescriptor(CSeq(parameterType), BooleanType), _)) ← methodsNative
          if method.isAbstract && parameterType == classFile.thisClass //!= ObjectType.Object
    } yield (classFile.thisClass, method.name, method.descriptor)
  }

  private def analyzeSQuOptWithAbstractions() = {
    import de.tud.cs.st.bat.resolved._
    import ivm._
    import expressiontree._
    import Lifting._
    import BATLifting._
    import performancetests.opaltests.InstructionLifting._
    import ivm.expressiontree.Util.ExtraImplicits._
    import schema.squopt._
    for {
      methodRecord ← methodsSQuOpt
      if methodRecord.method.isAbstract && methodRecord.method.name ==# "equals" && methodRecord.method.descriptor.returnType ==# BooleanType
      parameterTypes <- Let(methodRecord.method.descriptor.parameterTypes)
      if parameterTypes.length ==# 1 && parameterTypes(0) ==# methodRecord.classFile.thisClass //parameterTypes(0) !=# ObjectType.Object
    } yield (methodRecord.classFile.thisClass, methodRecord.method.name, methodRecord.method.descriptor)
  }

  def analyzeAbstractClassesThatDefinesCovariantEquals() {
    //XXX this was changed in BAT.
    benchQueryComplete("COVARIANT_EQUALS") ( // FB: EQ_ABSTRACT_SELF") {
         analyzeBaseWithoutAbstractions(),
         analyzeBaseWithAbstractions()
       )(
        analyzeSQuOptWithoutAbstractions(),
        analyzeSQuOptWithAbstractions()
        )
  }
}
