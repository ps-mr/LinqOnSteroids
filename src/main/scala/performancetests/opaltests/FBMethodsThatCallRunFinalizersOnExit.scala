package performancetests
package opaltests

import ivm._
import expressiontree._
import Lifting._

import de.tud.cs.st.bat
import bat.resolved._

import collection.immutable.Seq
import collection.{Seq => CSeq}

/**
 * User: pgiarrusso
 * Date: 4/8/2012
 */

trait FBMethodsThatCallRunFinalizersOnExit {
  this: FBAnalysesBase =>

  def analyzeMethodsThatCallRunFinalizersOnExit() {
    benchQueryComplete("RUN_FINALIZERS_ON_EXIT") ({ // FB: DM_RUN_FINALIZERS_ON_EXIT
      for {
        classFile ← classFiles
        method ← classFile.methods
        body ← method.body.toList
        //the method descriptor is not checked in FindBugs - there's no need
        instruction @ INVOKESTATIC(ObjectType(recvClassName), "runFinalizersOnExit", _/*MethodDescriptor(CSeq(BooleanType), VoidType)*/) ← body.instructions
        if recvClassName == "java/lang/System" || recvClassName == "java/lang/Runtime"
      } yield (classFile, method, instruction)
    },
    for {
      (classFile, method, body, instruction @
        INVOKESTATIC(ObjectType(recvClassName),
        "runFinalizersOnExit", _/*MethodDescriptor(CSeq(BooleanType), VoidType)*/)) ← methodBodiesInstructionsModularNative()
      //the method descriptor is not checked in FindBugs - there's no need
      if recvClassName == "java/lang/System" || recvClassName == "java/lang/Runtime"
    } yield (classFile, method, instruction))({
      import BATLifting._
      import InstructionLifting._

      for {
        classFile ← classFiles.asSquopt
        method ← classFile.methods
        body ← method.body
        instruction ← body.instructions.typeCase(when[INVOKESTATIC](
          instruction => instruction.name ==# "runFinalizersOnExit" && (instruction.declaringClass ==# ObjectType("java/lang/System") || instruction.declaringClass ==# ObjectType("java/lang/Runtime")), identity))
      //        if instruction.declaringClass ==# ObjectType("java/lang/System") || instruction.declaringClass ==# ObjectType("java/lang/Runtime")
      //        desc <- Let(instruction.methodDescriptor)
      //        recv <- instruction.declaringClass.ifInstanceOf[ObjectType]
      //        if (recv.className ==# "java/lang/System" || recv.className ==# "java/lang/Runtime") &&
      //          desc ==# MethodDescriptor(Seq(BooleanType), VoidType)
      //the method descriptor is not checked in FindBugs - there's no need
      // && instruction.methodDescriptor ==# MethodDescriptor(Seq(BooleanType), VoidType)
      //if desc ==# MethodDescriptor(Seq(BooleanType), VoidType)
      } yield (classFile, method, instruction)
    }, {
      import BATLifting._
      import InstructionLifting._
      import schema.squopt._

      for {
        cfb ← methodBodiesModularSQuOpt()
        instruction ← cfb.body.instructions.typeCase(when[INVOKESTATIC](
          instruction => instruction.name ==# "runFinalizersOnExit" && (instruction.declaringClass ==# ObjectType("java/lang/System") || instruction.declaringClass ==# ObjectType("java/lang/Runtime")), identity))
      } yield (cfb.classFile, cfb.method, instruction)
    }/*, {
      import BATLifting._
      import InstructionLifting._

      for {
        cfbi ← methodBodiesInstructionsModularSQuOpt()
        classFile ← Let(cfbi._1)
        method ← Let(cfbi._2)
        body ← Let(cfbi._3)
        instruction ← body.instructions.typeCase(when[INVOKESTATIC](
          instruction => instruction.name ==# "runFinalizersOnExit" && (instruction.declaringClass ==# ObjectType("java/lang/System") || instruction.declaringClass ==# ObjectType("java/lang/Runtime")), identity))
      } yield (classFile, method, instruction)
    }*/)
  }
}
