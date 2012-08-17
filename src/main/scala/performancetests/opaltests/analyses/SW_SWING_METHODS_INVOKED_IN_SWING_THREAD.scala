package performancetests.opaltests.analyses

import de.tud.cs.st.bat.resolved._

/**
 *
 * Author: Ralf Mitschke
 * Date: 09.08.12
 * Time: 14:47
 *
 */
trait SW_SWING_METHODS_INVOKED_IN_SWING_THREAD {
  this: performancetests.opaltests.FBAnalysesBase =>

  private val emptyDescriptor = MethodDescriptor(Nil, VoidType)

  private val boolParamDescriptor = MethodDescriptor(List(BooleanType), VoidType)

  private def analyzeBaseWithoutAbstractions() = {
      for (classFile ← classFiles;
             method ← classFile.methods if (
                                           method.body.isDefined &&
                                           method.isPublic &&
                                           method.isStatic &&
                                           method.name == "main" ||
                                           classFile.thisClass.className.toLowerCase.indexOf("benchmark") >= 0
                                           );
             (INVOKEVIRTUAL(targetType, name, desc), idx) ← withIndexNative(method.body.get.instructions)
             if (
                targetType.isObjectType &&
                targetType.asInstanceOf[ObjectType].className.startsWith("javax/swing/")) &&
                (
                name == "show" && desc == emptyDescriptor ||
                name == "pack" && desc == emptyDescriptor ||
                name == "setVisible" && desc == boolParamDescriptor
                )
        ) yield
          (classFile.thisClass, method.name, method.descriptor, idx)
  }


  private def analyzeSQuOptWithoutAbstractions() = {
    import de.tud.cs.st.bat.resolved._
    import ivm._
    import expressiontree._
    import Lifting._
    import BATLifting._
    import performancetests.opaltests.InstructionLifting._
    import ivm.expressiontree.Util.ExtraImplicits._

      for (classFile ← classFiles.asSmart;
             method ← classFile.methods if (
                                           method.body.isDefined &&
                                           method.isPublic &&
                                           method.isStatic &&
                                           method.name ==# "main" ||
                                           classFile.thisClass.className.toLowerCase.indexOf("benchmark") >= 0
                                           );
             instructionInfo ← withIndexSQuOpt(method.body.get.instructions);
             invoke ← instructionInfo._1.ifInstanceOf[INVOKEVIRTUAL]
             if (
                invoke.declaringClass.isObjectType &&
                invoke.declaringClass.asInstanceOf_#[ObjectType].className.startsWith("javax/swing/")) &&
                (
                invoke.name ==# "show" && invoke.methodDescriptor ==# emptyDescriptor ||
                invoke.name ==# "pack" && invoke.methodDescriptor ==# emptyDescriptor ||
                invoke.name ==# "setVisible" && invoke.methodDescriptor ==# boolParamDescriptor
                )
        ) yield
          (classFile.thisClass, method.name, method.descriptor, instructionInfo._2)
  }


  private def analyzeBaseWithAbstractions() = {
          for ( schema.BytecodeInstrIndexed(classFile, method,
                                    INVOKEVIRTUAL(targetType, name, desc), idx) ← methodBodiesInstructionsIndexedModularNative
                if (targetType.isObjectType &&
                    targetType.asInstanceOf[ObjectType].className.startsWith("javax/swing/")) &&
                    (
                    name == "show" && desc == emptyDescriptor ||
                    name == "pack" && desc == emptyDescriptor ||
                    name == "setVisible" && desc == boolParamDescriptor
                    ) && ((
                      method.isPublic &&
                      method.isStatic &&
                      method.name == "main"
                    ) || classFile.thisClass.className.toLowerCase.indexOf("benchmark") >= 0)
            ) yield
              (classFile.thisClass, method.name, method.descriptor, idx)
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
        for ( instructionInfo ← methodBodiesInstructionsIndexedModularSQuOpt;
              invoke ← instructionInfo.instruction.ifInstanceOf[INVOKEVIRTUAL]
              if (
                    invoke.declaringClass.isObjectType &&
                    invoke.declaringClass.asInstanceOf_#[ObjectType].className.startsWith("javax/swing/")) &&
                    (
                    invoke.name ==# "show" && invoke.methodDescriptor ==# emptyDescriptor ||
                    invoke.name ==# "pack" && invoke.methodDescriptor ==# emptyDescriptor ||
                    invoke.name ==# "setVisible" && invoke.methodDescriptor ==# boolParamDescriptor
                )&&
                    ((
                      instructionInfo.method.isPublic &&
                      instructionInfo.method.isStatic &&
                      instructionInfo.method.name ==# "main"
                    ) || instructionInfo.classFile.thisClass.className.toLowerCase.indexOf("benchmark") >= 0
                    )
          ) yield
            (instructionInfo.classFile.thisClass, instructionInfo.method.name, instructionInfo.method.descriptor, instructionInfo.index)
    }


  def analyzeSW_SWING_METHODS_INVOKED_IN_SWING_THREAD() {
    benchQueryComplete("SW_SWING_METHODS_INVOKED_IN_SWING_THREAD")(
      analyzeBaseWithoutAbstractions(),
      analyzeBaseWithAbstractions()
      )(
      analyzeSQuOptWithoutAbstractions(),
      analyzeSQuOptWithAbstractions()
    )
  }
}
