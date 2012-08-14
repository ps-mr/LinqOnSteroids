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

    import BaseAnalyses._

    import ivm.expressiontree.Exp

    val doubleClass = ObjectType("java/lang/Double")

    val longBitsToDoubleDescriptor = MethodDescriptor(List(LongType), DoubleType)

    def analyzeBaseWithoutAbstractions() = {
      for (classFile ← classFiles;
           method ← classFile.methods if method.body.isDefined;
           Seq(
           (I2L, _),
           (INVOKESTATIC(`doubleClass`, "longBitsToDouble", `longBitsToDoubleDescriptor`), idx)
              ) ← withIndexNative(method.body.get.instructions).sliding(2)
      ) yield
        (classFile, method, idx)
    }

/*
    def analyzeSQuOptWithoutAbstractions() = {
      import ivm._
      import expressiontree._
      import Lifting._
      import BATLifting._
      import performancetests.opaltests.InstructionLifting._
      import ivm.expressiontree.Util.ExtraImplicits._
      for (classFile ← classFiles.asSmart;
           method ← classFile.methods if method.body.isDefined;
           window ← withIndexSQuOpt(method.body.get.instructions).sliding(2);
           //[error] D:\workspace\LinqOnSteroids\src\main\scala\performancetests\opaltests\analyses\DMI_LONG_BITS_TO_DOUBLE_INVOKED_ON_INT.scala:46: value filter is not a member of ivm.expressiontree.Exp[de.tud.cs.st.bat.resolved.INVOKESTATIC]
           //[error]            second ? window.last._1.asInstanceOf_#[INVOKESTATIC]
           second ← window.last._1.asInstanceOf_#[INVOKESTATIC]
           if(  window.head._1.isInstanceOf_#[I2L] &&
                second.declaringClass ==# doubleClass &&
                second.name ==# "longBitsToDouble" &&
                second.methodDescriptor ==# longBitsToDoubleDescriptor
              )
      ) yield
        (classFile, method, window.last._2)
    }
*/

    def analyzeBaseWithAbstractions() = {
      for (schema.BytecodeInstrWindow(Seq(_,idx),
              Seq(I2L,INVOKESTATIC(`doubleClass`, "longBitsToDouble", `longBitsToDoubleDescriptor`)),
               classFile, method) ← methodBodiesInstructionsSlidingNative(2)
      ) yield
        (classFile, method, idx)
    }

/*
    def analyzeSQuOptWithAbstractions() = {
        import de.tud.cs.st.bat.resolved._
        import ivm._
        import expressiontree._
        import Lifting._
        import BATLifting._
        import performancetests.opaltests.InstructionLifting._
        import ivm.expressiontree.Util.ExtraImplicits._
        import schema.squopt._
        for ( window ← methodBodiesInstructionsSlidingSQuOpt(3);
        //[error] D:\workspace\LinqOnSteroids\src\main\scala\performancetests\opaltests\analyses\DMI_LONG_BITS_TO_DOUBLE_INVOKED_ON_INT.scala:74: value filter is not a member of ivm.expressiontree.Exp[de.tud.cs.st.bat.resolved.INVOKESTATIC]
        //[error]               second ? window.instrs.last.asInstanceOf_#[INVOKESTATIC]
              second ← window.instrs.last.asInstanceOf_#[INVOKESTATIC]
               if(  window.instrs.head.isInstanceOf_#[I2L] &&
                    second.declaringClass ==# doubleClass &&
                    second.name ==# "longBitsToDouble" &&
                    second.methodDescriptor ==# longBitsToDoubleDescriptor
                  )
         ) yield
            (window.classFile, window.method, window.instrIdxs.last)

    }
*/
}