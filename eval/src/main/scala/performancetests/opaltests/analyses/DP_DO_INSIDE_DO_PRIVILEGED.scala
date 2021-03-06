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

  private val reflectionField = ObjectType("java/lang/reflect/Field")

  private val reflectionMethod = ObjectType("java/lang/reflect/Method")

  private val priviledgedAction = ObjectType("java/security/PrivilegedAction")

  private val priviledgedExceptionAction = ObjectType("java/security/PrivilegedExceptionAction")

  private def analyzeBaseWithoutAbstractions() = {
    import de.tud.cs.st.bat.resolved._
    for (classFile ← classFiles
         if !classFile.interfaces.exists((t) => t == priviledgedAction || t == priviledgedExceptionAction);
         method ← classFile.methods if method.body.isDefined;
         (INVOKEVIRTUAL(receiver, "setAccessible", _), idx) ← withIndexNative(method.body.get.instructions)
         if (receiver == reflectionField || receiver == reflectionMethod)
    ) yield
      (classFile, method, idx)
  }


  private def analyzeSQuOptWithoutAbstractions() = {
    import de.tud.cs.st.bat.resolved._
    import ivm._
    import expressiontree._
    import Lifting._
    import performancetests.opaltests.InstructionLifting._
    import BATLifting._

    for (classFile ← classFiles.asSquopt
         if !classFile.interfaces.exists((t) => (t ==# priviledgedAction) || (t ==# priviledgedExceptionAction));
         method ← classFile.methods if method.body.isDefined;
         instructionWithIndex ← withIndexSQuOpt(method.body.get.instructions);
         invoke ← instructionWithIndex._1.ifInstanceOf[INVOKEVIRTUAL]
         if (
            (invoke.declaringClass ==# reflectionField || invoke.declaringClass ==# reflectionMethod) &&
            invoke.name ==# "setAccessible"
            )
    ) yield
      (classFile, method, instructionWithIndex._2)
  }


  private def analyzeBaseWithAbstractions() = {
    import de.tud.cs.st.bat.resolved._
    import schema._

    for (
      BytecodeInstrIndexed(classFile, method, INVOKEVIRTUAL(receiver, "setAccessible", _), idx) ← methodBodiesInstructionsIndexedModularNative
      if( !classFile.interfaces.exists((t) => t == priviledgedAction || t == priviledgedExceptionAction) &&
            (receiver == reflectionField || receiver == reflectionMethod))
    ) yield
      (classFile, method, idx)
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

    for  {
      instructionWithIndex ← methodBodiesInstructionsIndexedModularSQuOpt
      invoke ← instructionWithIndex.instruction.ifInstanceOf[INVOKEVIRTUAL]
      if (!instructionWithIndex.classFile.interfaces.exists((t) =>  (t ==# priviledgedAction) || (t ==# priviledgedExceptionAction)) &&
         (invoke.declaringClass ==# reflectionField || invoke.declaringClass ==# reflectionMethod) &&
         invoke.name ==# "setAccessible"
         )
    } yield
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
