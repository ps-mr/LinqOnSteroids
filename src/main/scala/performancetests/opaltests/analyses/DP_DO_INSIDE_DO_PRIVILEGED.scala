package performancetests.opaltests.analyses

/**
 *
 * Author: Ralf Mitschke
 * Date: 10.08.12
 * Time: 10:10
 *
 */
trait DP_DO_INSIDE_DO_PRIVILEGED {
  this: performancetests.opaltests.FBAnalysesBase =>

  import de.tud.cs.st.bat.resolved.ObjectType

  val reflectionField = ObjectType("java/lang/reflect/Field")

  val reflectionMethod = ObjectType("java/lang/reflect/Method")

  val priviledgedAction = ObjectType("java/security/PrivilegedAction")

  val priviledgedExceptionAction = ObjectType("java/security/PrivilegedExceptionAction")

  import BaseAnalyses._

  def analyzeBaseWithoutAbstractions() = {
    import de.tud.cs.st.bat.resolved._
    for (classFile ← classFiles
         if !classFile.interfaces.exists((t) => t == priviledgedAction || t == priviledgedExceptionAction);
         method ← classFile.methods if method.body.isDefined;
         (INVOKEVIRTUAL(receiver, "setAccessible", _), idx) ← withIndex(method.body.get.instructions)
         if (receiver == reflectionField || receiver == reflectionMethod)
    ) yield {
      (classFile, method, idx)
    }
  }


  def analyzeSQuOptWithoutAbstractions() = {
    import de.tud.cs.st.bat.resolved._
    import ivm._
    import expressiontree._
    import Lifting._
    import performancetests.opaltests.InstructionLifting._
    import BATLifting._

    for (classFile ← classFiles.asSmart
         if !classFile.interfaces.exists((t) => (t ==# priviledgedAction) || (t ==# priviledgedExceptionAction));
         method ← classFile.methods if method.body.isDefined;
         instructionWithIndex ← withIndexExp(method.body.get.instructions);
         invoke ← instructionWithIndex._1.ifInstanceOf[INVOKEVIRTUAL]
         if (
            (invoke.declaringClass ==# reflectionField || invoke.declaringClass ==# reflectionMethod) &&
            invoke.name ==# "setAccessible"
            )
    ) yield {
      (classFile, method, instructionWithIndex._2)
    }
  }


  def analyzeBaseWithAbstractions() = {
    import de.tud.cs.st.bat.resolved._
    import schema._

    for (
      BytecodeInstrIndexed(classFile, method, INVOKEVIRTUAL(receiver, "setAccessible", _), idx) ← methodBodiesInstructionsIndexedModularNative
      if( !classFile.interfaces.exists((t) => t == priviledgedAction || t == priviledgedExceptionAction) &&
            (receiver == reflectionField || receiver == reflectionMethod))
    ) yield
      (classFile, method, idx)
  }

  def analyzeSQuOptWithAbstractions() = {
    import de.tud.cs.st.bat.resolved._
    import ivm._
    import expressiontree._
    import Lifting._
    import BATLifting._
    import performancetests.opaltests.InstructionLifting._
    import ivm.expressiontree.Util.ExtraImplicits._
    import schema.squopt._

    for (
      instructionWithIndex ← methodBodiesInstructionsIndexedModularNative;
      invoke ← instructionWithIndex.instruction.ifInstanceOf[INVOKEVIRTUAL]
      if (!instructionWithIndex.classFile.interfaces.exists((t) =>  {(t ==# priviledgedAction) || (t ==# priviledgedExceptionAction)}) &&
         (invoke.declaringClass ==# reflectionField || invoke.declaringClass ==# reflectionMethod) &&
         invoke.name ==# "setAccessible"
         )
    ) yield
      (instructionWithIndex.classFile, instructionWithIndex.method, instructionWithIndex.index)
  }

  def analyzeDP_DO_INSIDE_DO_PRIVILEGED() {
    benchQueryComplete("DP_DO_INSIDE_DO_PRIVILEGED")(
                                                     analyzeBaseWithoutAbstractions(),
                                                     analyzeBaseWithAbstractions()
                                                    )(
                                                     analyzeSQuOptWithoutAbstractions(),
                                                     analyzeSQuOptWithAbstractions()
                                         )
  }

}