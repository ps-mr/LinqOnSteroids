package performancetests.opaltests.analyses

import de.tud.cs.st.bat.resolved._

/**
 *
 * Author: Ralf Mitschke
 * Date: 06.08.12
 * Time: 15:53
 *
 */
trait DMI_LONG_BITS_TO_DOUBLE_INVOKED_ON_INT{
    this: performancetests.opaltests.FBAnalysesBase =>

    import ivm.expressiontree.Exp

    private val doubleClass = ObjectType("java/lang/Double")

    private val longBitsToDoubleDescriptor = MethodDescriptor(List(LongType), DoubleType)

    private def analyzeBaseWithoutAbstractions() = {
      for (classFile ← classFiles;
           method ← classFile.methods if method.body.isDefined;
           Seq(
           (I2L, _),
           (INVOKESTATIC(`doubleClass`, "longBitsToDouble", `longBitsToDoubleDescriptor`), idx)
              ) ← withIndexNative(method.body.get.instructions).sliding(2)
      ) yield
        (classFile, method, idx)
    }


    private def analyzeSQuOptWithoutAbstractions() = {
      import ivm._
      import expressiontree._
      import Lifting._
      import BATLifting._
      import performancetests.opaltests.InstructionLifting._
      import ivm.expressiontree.Util.ExtraImplicits._
      for {classFile ← classFiles.asSmart
           method ← classFile.methods if method.body.isDefined
           window ← withIndexSQuOpt(method.body.get.instructions).sliding(2)
           second ← window.last._1.ifInstanceOf[INVOKESTATIC]
           if  window.head._1 ==# I2L &&
                second.declaringClass ==# doubleClass &&
                second.name ==# "longBitsToDouble" &&
                second.methodDescriptor ==# longBitsToDoubleDescriptor
      } yield
        (classFile, method, window.last._2)
    }


    private def analyzeBaseWithAbstractions() = {
      for (schema.BytecodeInstrWindow(Seq(_,idx),
              Seq(I2L,INVOKESTATIC(`doubleClass`, "longBitsToDouble", `longBitsToDoubleDescriptor`)),
               classFile, method) ← methodBodiesInstructionsSlidingNative(2)
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
        for ( window ← methodBodiesInstructionsSlidingSQuOpt(3);
              second ← window.instrs.last.ifInstanceOf[INVOKESTATIC]
               if   window.instrs.head ==# I2L &&
                    second.declaringClass ==# doubleClass &&
                    second.name ==# "longBitsToDouble" &&
                    second.methodDescriptor ==# longBitsToDoubleDescriptor
         ) yield
            (window.classFile, window.method, window.instrIdxes.last)

    }


  def analyzeDMI_LONG_BITS_TO_DOUBLE_INVOKED_ON_INT() {
    benchQueryComplete("DMI_LONG_BITS_TO_DOUBLE_INVOKED_ON_INT")(
      analyzeBaseWithoutAbstractions(),
      analyzeBaseWithAbstractions()
      )(
      analyzeSQuOptWithoutAbstractions(),
      analyzeSQuOptWithAbstractions()
    )
  }
}
