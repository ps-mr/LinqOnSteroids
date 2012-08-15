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
        (classFile.thisClass,method.name, method.descriptor, declaringClass, name, fieldType, idx)
    }

    private def analyzeBaseCompletelyWithoutAbstractions() = {
      for (classFile ← classFiles;
           method ← classFile.methods if (
                                         method.body.isDefined &&
                                         method.name != "<init>" &&
                                         !method.isStatic && (
                                           (for (superClass ← classHierarchy.superclasses(classFile.thisClass).getOrElse(Set());
                                                                    methodDecl ← getMethodDeclarationNative(superClass, method.name, method.descriptor)
                                           ) yield methodDecl).size > 0)
                                         );
           (GETFIELD(declaringClass, name, fieldType), idx) ← (method.body.get.instructions.zipWithIndex.filter { case (instr, _) => instr != null });
           constructor ← classFile.constructors;
           if (
                classFile.fields.exists {
                                          case Field(_, `name`, `fieldType`, _) => true
                                          case _                                => false
                                        }
           );
           (superClass, superConstructor) ← {
                 val superClasses = classHierarchy.superclasses(classFile.thisClass)
                 if (!superClasses.isDefined) {
                    None
                 }
                 val superConstructor = constructor.body.get.instructions.collectFirst {
                                   case INVOKESPECIAL(trgt, n, d)
                                     if superClasses.get.contains(trgt.asInstanceOf[ObjectType]) =>
                                     (trgt.asInstanceOf[ObjectType], n, d)

                 }
                 superConstructor match {
                   case Some((targetType, name, desc)) => getMethodDeclarationNative(targetType, name, desc)
                   case None => None // we encountered java.lang.Object
                 }
               }
           if(
               superConstructor.body.isDefined &&
               superConstructor.body.get.instructions.exists {
                 case INVOKEINTERFACE(targetType, name, desc) => superClass.thisClass == targetType && method.name == name && method.descriptor == desc
                 case INVOKEVIRTUAL(targetType, name, desc)   => superClass.thisClass == targetType && method.name == name && method.descriptor == desc
                 case INVOKESTATIC(targetType, name, desc)    => superClass.thisClass == targetType && method.name == name && method.descriptor == desc
                 case INVOKESPECIAL(targetType, name, desc)   => superClass.thisClass == targetType && method.name == name && method.descriptor == desc
                 case _                                       => false
               }
              )
      ) yield
        (classFile.thisClass,method.name, method.descriptor, declaringClass, name, fieldType, idx)
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
        for {classFile ← classFiles.asSmart
             method ← classFile.methods if (
                                           method.body.isDefined &&
                                           method.name !=# "<init>" &&
                                           !method.isStatic &&
                                           isOverrideSQuOpt(classFile)(method))
             instruction ← withIndexSQuOpt(method.body.get.instructions)
             getField ← instruction._1.ifInstanceOf[GETFIELD]
             constructor ← classFile.constructors
             if declaresFieldSQuOpt(classFile)(getField.name, getField.fieldType);
             calledSuperConstructorInfo ← calledSuperConstructorSQuOpt(classFile, constructor)
             if (callsSQuOpt(calledSuperConstructorInfo._2, calledSuperConstructorInfo._1, method))
        } yield
          (classFile.thisClass,method.name, method.descriptor, getField.declaringClass, getField.name, getField.fieldType, instruction._2)
    }


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
          (classFile.thisClass,method.name, method.descriptor, declaringClass, name, fieldType, idx)
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
          for { instructionWithIndex ← methodBodiesInstructionsIndexedModularSQuOpt
                getField ← instructionWithIndex.instruction.ifInstanceOf[GETFIELD]
                if(  instructionWithIndex.method.body.isDefined &&
                     instructionWithIndex.method.name !=# "<init>" &&
                     !instructionWithIndex.method.isStatic &&
                     isOverrideSQuOpt(instructionWithIndex.classFile)(instructionWithIndex.method))
               constructor ← instructionWithIndex.classFile.constructors
               if declaresFieldSQuOpt(instructionWithIndex.classFile)(getField.name, getField.fieldType);
               calledSuperConstructorInfo ← calledSuperConstructorSQuOpt(instructionWithIndex.classFile, constructor)
               if (callsSQuOpt(calledSuperConstructorInfo._2, calledSuperConstructorInfo._1, instructionWithIndex.method))
          } yield
            (instructionWithIndex.classFile.thisClass, instructionWithIndex.method.name, instructionWithIndex.method.descriptor, getField.declaringClass, getField.name, getField.fieldType, instructionWithIndex.index)
        }

  def analyzeUR_UNINIT_READ_CALLED_FROM_SUPER_CONSTRUCTOR() {
    benchQueryComplete("UR_UNINIT_READ_CALLED_FROM_SUPER_CONSTRUCTOR")(
                                                     analyzeBaseWithoutAbstractions(),
                                                     analyzeBaseWithAbstractions()
                                                    )(
                                                     analyzeSQuOptWithoutAbstractions(),
                                                     analyzeSQuOptWithAbstractions()
                                         )
  }
}