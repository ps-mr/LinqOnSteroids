package performancetests.opaltests.analyses

import de.tud.cs.st.bat.resolved._
import performancetests.opaltests.FBAnalysesBase
import schema.BytecodeInstrWindow

/**
 *
 * Author: Ralf Mitschke
 * Date: 09.08.12
 * Time: 16:28
 *
 */
trait BX_BOXING_IMMEDIATELY_UNBOXED_TO_PERFORM_COERCION {
  this: FBAnalysesBase =>

  import BaseAnalyses._

  def analyzeBaseWithoutAbstractions() = {
    for (classFile ← classFiles if classFile.majorVersion > 49;
         method ← classFile.methods if method.body.isDefined;
         Seq(
         (INVOKESPECIAL(firstReceiver, _, de.tud.cs.st.bat.resolved.MethodDescriptor(Seq(paramType), _)), _),
         (INVOKEVIRTUAL(secondReceiver, name, de.tud.cs.st.bat.resolved.MethodDescriptor(Seq(), returnType)), idx)
            ) ← withIndex(method.body.get.instructions).sliding(2)
         if (
            !paramType.isReferenceType &&
            firstReceiver.asInstanceOf[ObjectType].className.startsWith("java/lang") &&
            firstReceiver == secondReceiver &&
            name.endsWith("Value") &&
            returnType != paramType // coercion to another type performed
            )
    ) yield {
      (classFile, method, idx)
    }
  }

  /*
  def analyzeSQuOptWithoutAbstractions() {
    for (classFile ← classFiles.asSmart if( classFile.majorVersion > 49);
         method ← classFile.methods if method.body.isDefined;
         Seq(
         (INVOKESPECIAL(firstReceiver, _, MethodDescriptor(Seq(paramType), _)), _),
         (INVOKEVIRTUAL(secondReceiver, name, MethodDescriptor(Seq(), returnType)), idx)
            ) ← withIndex(method.body.get.instructions).sliding(2)
         if (
            !paramType.isReferenceType &&
            firstReceiver.asInstanceOf[ObjectType].className.startsWith("java/lang") &&
            firstReceiver == secondReceiver &&
            name.endsWith("Value") &&
            returnType != paramType // coercion to another type performed
            )
    ) yield {
      (classFile, method, idx)
    }
  }
  */

  def analyzeBaseWithAbstractions() = {
    for (BytecodeInstrWindow(Seq(_, idx),
                             Seq(INVOKESPECIAL(firstReceiver, _, de.tud.cs.st.bat.resolved.MethodDescriptor(Seq(paramType), _)),
                                 INVOKEVIRTUAL(secondReceiver, name, de.tud.cs.st.bat.resolved.MethodDescriptor(Seq(), returnType))),
                             classFile,
                             method) ← methodBodiesInstructionsSlidingNative(2)
         if (
            !paramType.isReferenceType &&
            firstReceiver.asInstanceOf[ObjectType].className.startsWith("java/lang") &&
            firstReceiver == secondReceiver &&
            name.endsWith("Value") &&
            returnType != paramType // coercion to another type performed
            )
    ) yield {
      (classFile, method, idx)
    }
  }


  /*
    def analyzeSQuOptWithAbstractions() {
      for ((classFile, method, Seq((INVOKESPECIAL(firstReceiver, _, MethodDescriptor(Seq(paramType), _)), _),
                                   (INVOKEVIRTUAL(secondReceiver, name, MethodDescriptor(Seq(), returnType)), idx)
                                  )) ← methodBodiesInstructionsSlidingSQuOpt(2)
           if (classFile.majorVersion > 49 &&
               method.body.isDefined
               ! paramType.isReferenceType &&
               firstReceiver.asInstanceOf[ObjectType].className.startsWith("java/lang") &&
               firstReceiver == secondReceiver &&
               name.endsWith("Value") &&
               returnType != paramType // coercion to another type performed
              )
      ) yield {
        (classFile, method, idx)
      }
    }
  */

}