package performancetests.opaltests.analyses

import de.tud.cs.st.bat.resolved._

/**
 *
 * Author: Ralf Mitschke
 * Date: 09.08.12
 * Time: 09:35
 *
 */
trait UR_UNINIT_READ_CALLED_FROM_SUPER_CONSTRUCTOR{
    this: performancetests.opaltests.FBAnalysesBase =>

    private def analyzeBaseWithoutAbstractions() = {
      for (classFile ← classFiles;
           method ← classFile.methods if (
                                         method.body.isDefined &&
                                         method.name != "<init>" &&
                                         !method.isStatic &&
                                         isOverrideNative(classFile)(method));
           (GETFIELD(declaringClass, name, fieldType), idx) ← withIndexNative(method.body.get.instructions);
           constructor ← classFile.constructors
           if declaresFieldNative(classFile)(name, fieldType);
           (superClass, superConstructor) ← calledSuperConstructorNative(classFile, constructor)
           if (callsNative(superConstructor, superClass, method))

      ) yield
        (classFile,method, declaringClass, name, fieldType, idx)
    }

/*
    private def analyzeSQuOptWithoutAbstractions() = {
        import de.tud.cs.st.bat.resolved._
        import ivm._
        import expressiontree._
        import Lifting._
        import BATLifting._
        import performancetests.opaltests.InstructionLifting._
        import ivm.expressiontree.Util.ExtraImplicits._
        import schema.squopt._
      for {classFile ← classFiles.asSmart
           method ← classFile.methods if (
                                         method.body.isDefined &&
                                         method.name != "<init>" &&
                                         !method.isStatic &&
                                         isOverride(classFile)(method))
           instruction ← withIndexSQuOpt(method.body.get.instructions)
           getField ← instruction._1.asInstanceOf_#[GETFIELD]
           constructor ← classFile.constructors
           if declaresField(classFile)(getField.name, getField.fieldType);
           calledSuperConstructorInfo ← calledSuperConstructor(classFile, constructor)
           if (calls(calledSuperConstructorInfo._1, calledSuperConstructorInfo._2, method))
      } yield
        (classFile,method, getField.declaringClass, getField.name, getField.fieldType, instruction._2)
    }
*/

    private def analyzeBaseWithAbstractions() = {
        for (schema.BytecodeInstrIndexed(classFile, method,
             GETFIELD(declaringClass, name, fieldType), idx) ← methodBodiesInstructionsIndexedModularNative
             if method.body.isDefined &&
                method.name != "<init>" &&
                !method.isStatic &&
                isOverrideNative(classFile)(method);
             constructor ← classFile.constructors
             if declaresFieldNative(classFile)(name, fieldType);
             (superClass, superConstructor) ← calledSuperConstructorNative(classFile, constructor)
             if (callsNative(superConstructor, superClass, method))

        ) yield
          (classFile,method, declaringClass, name, fieldType, idx)
    }

/*
    private def analyzeSQuOptWithAbstractions() = {
            import de.tud.cs.st.bat.resolved._
            import ivm._
            import expressiontree._
            import Lifting._
            import BATLifting._
            import performancetests.opaltests.InstructionLifting._
            import ivm.expressiontree.Util.ExtraImplicits._
            import schema.squopt._
          for { instructionWithIndex ← methodBodiesInstructionsIndexedModularSQuOpt
                getField ← instructionWithIndex.instruction.asInstanceOf_#[GETFIELD]
                if(  instructionWithIndex.method.body.isDefined &&
                     instructionWithIndex.method.name != "<init>" &&
                     !instructionWithIndex.method.isStatic &&
                     isOverride(instructionWithIndex.classFile)(instructionWithIndex.method))
               constructor ← instructionWithIndex.classFile.constructors
               if declaresField(instructionWithIndex.classFile)(getField.name, getField.fieldType);
               calledSuperConstructorInfo ← calledSuperConstructor(instructionWithIndex.classFile, constructor)
               if (calls(calledSuperConstructorInfo._1, calledSuperConstructorInfo._2, instructionWithIndex.method))
          } yield
            (instructionWithIndex.classFile, instructionWithIndex.method, getField.declaringClass, getField.name, getField.fieldType, instructionWithIndex.index)
        }
*/
}