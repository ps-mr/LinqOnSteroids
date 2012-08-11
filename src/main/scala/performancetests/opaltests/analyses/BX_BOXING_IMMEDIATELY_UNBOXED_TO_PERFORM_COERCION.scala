package performancetests.opaltests
package analyses


/**
 *
 * Author: Ralf Mitschke
 * Date: 09.08.12
 * Time: 16:28
 *
 */
trait BX_BOXING_IMMEDIATELY_UNBOXED_TO_PERFORM_COERCION {
  this: performancetests.opaltests.FBAnalysesBase =>

  import BaseAnalyses._

  def analyzeBaseWithoutAbstractions() = {
    import de.tud.cs.st.bat.resolved._
    for {classFile ← classFiles if classFile.majorVersion > 49
         method ← classFile.methods if method.body.isDefined
         Seq(
         (INVOKESPECIAL(firstReceiver, _,MethodDescriptor(Seq(paramType), _)), _),
         (INVOKEVIRTUAL(secondReceiver, name, MethodDescriptor(Seq(), returnType)), idx)
         ) ← withIndex(method.body.get.instructions).sliding(2)
         if !paramType.isReferenceType &&
            firstReceiver.asInstanceOf[ObjectType].className.startsWith("java/lang") &&
            firstReceiver == secondReceiver &&
            name.endsWith("Value") &&
            returnType != paramType // coercion to another type performed
    } yield
      (classFile, method, idx)
  }


  def analyzeSQuOptWithoutAbstractions() = {
    import de.tud.cs.st.bat.resolved._
    import ivm._
    import expressiontree._
    import Lifting._
    import BATLifting._
    import InstructionLifting._
    import ivm.expressiontree.Util.ExtraImplicits._

    for {classFile ← classFiles.asSmart if classFile.majorVersion > 49
         method ← classFile.methods if method.body.isDefined
         window ← withIndexExp(method.body.get.instructions).sliding(2)
         first ← window.head._1.ifInstanceOf[INVOKESPECIAL]
         second ← window.last._1.ifInstanceOf[INVOKEVIRTUAL]
         if first.methodDescriptor.parameterTypes.size ==# 1 &&
            !first.methodDescriptor.parameterTypes.head.isReferenceType &&
            second.methodDescriptor.parameterTypes.size ==# 0 &&
            first.declaringClass.asInstanceOf_#[ObjectType].className.startsWith("java/lang") &&
            first.declaringClass ==# second.declaringClass &&
            second.name.endsWith("Value") &&
            first.methodDescriptor.parameterTypes.head ==# second.methodDescriptor.returnType
    } yield
      (classFile, method, window.last._2)
  }


  def analyzeBaseWithAbstractions() = {
    import de.tud.cs.st.bat.resolved._
    import schema._
    for (BytecodeInstrWindow(Seq(_, idx),
                             Seq(INVOKESPECIAL(firstReceiver, _, MethodDescriptor(Seq(paramType), _)),
                                 INVOKEVIRTUAL(secondReceiver, name, MethodDescriptor(Seq(), returnType))),
                             classFile,
                             method) ← methodBodiesInstructionsSlidingNative(2)
         if classFile.majorVersion > 49 &&
            !paramType.isReferenceType &&
            firstReceiver.asInstanceOf[ObjectType].className.startsWith("java/lang") &&
            firstReceiver == secondReceiver &&
            name.endsWith("Value") &&
            returnType != paramType // coercion to another type performed
    ) yield
      (classFile, method, idx)
  }


  def analyzeSQuOptWithAbstractions() = {
      import de.tud.cs.st.bat.resolved._
      import ivm._
      import expressiontree._
      import Lifting._
      import BATLifting._
      import InstructionLifting._
      import ivm.expressiontree.Util.ExtraImplicits._
      import schema.squopt._

      for {window ← methodBodiesInstructionsSlidingSQuOpt(2)
           first ← window.instrs.head.ifInstanceOf[INVOKESPECIAL]
           second ← window.instrs.last.ifInstanceOf[INVOKEVIRTUAL]
           if
              window.classFile.majorVersion > 49 &&
              window.method.body.isDefined &&
              first.methodDescriptor.parameterTypes.size ==# 1 &&
              !first.methodDescriptor.parameterTypes.head.isReferenceType &&
              second.methodDescriptor.parameterTypes.size ==# 0 &&
              first.declaringClass.asInstanceOf_#[ObjectType].className.startsWith("java/lang") &&
              first.declaringClass ==# second.declaringClass &&
              second.name.endsWith("Value") &&
              first.methodDescriptor.parameterTypes.head ==# second.methodDescriptor.returnType
      } yield
        (window.classFile, window.method, window.instrIdxes.last)
    }


  def analyzeBOXING_IMMEDIATELY_UNBOXED_TO_PERFORM_COERCION() {
    benchQueryComplete("BX_BOXING_IMMEDIATELY_UNBOXED_TO_PERFORM_COERCION")(
      analyzeBaseWithoutAbstractions(),
      analyzeBaseWithAbstractions())(
      analyzeSQuOptWithoutAbstractions(),
      analyzeSQuOptWithAbstractions()
    )
  }
}
