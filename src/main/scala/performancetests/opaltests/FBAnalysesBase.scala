package performancetests
package opaltests

import ivm._
import expressiontree._
import Lifting._

import de.tud.cs.st.bat
import bat.resolved._
import analyses._

import reader.Java6Framework
import bat.resolved.analyses.ClassHierarchy

import collection.immutable.Seq
import collection.{Seq => CSeq}

import org.scalatest.matchers.ShouldMatchers

/**
 * User: pgiarrusso
 * Date: 4/8/2012
 */

abstract class FBAnalysesBase extends QueryBenchmarking with ShouldMatchers {
  def zipFiles: Seq[String]

  val classFiles: Seq[ClassFile] = benchMark("Reading all class files", execLoops = 1, minSampleLoops = 1, maxCoV = None) {
    for (zipFile ← zipFiles.toVector; classFile ← Java6Framework.ClassFiles(zipFile)) yield classFile
  }
  val getClassFile: Map[ObjectType, ClassFile] = classFiles.map(cf ⇒ (cf.thisClass, cf)).toMap

  val getClassFileSQuOpt = getClassFile.asSmart

  // Now the classHierarchy is built functionally - hence, the result could be incrementally maintained (at least for the
  // addition of classes).
  val classHierarchy = (new ClassHierarchy /: classFiles)(_ + _)

  val classHierarchySQuOpt = classHierarchy.asSmart

  println("Number of class files: " + classFiles.length)
  println("Numer of methods: " + methodsSQuOpt().interpret().size)


  def fieldsNative() = {
    import schema._
    for {
      classFile ← classFiles
      field ← classFile.fields
    } yield FieldRecord(classFile, field)
  }

  def fieldsSQuOpt() = {
    import BATLifting._
    import schema.squopt._
    for {
      classFile ← classFiles.asSmart
      field ← classFile.fields
    } yield FieldRecord(classFile, field)
  }


  def methodsNative() = {
    import schema._
    for {
      classFile ← classFiles
      method ← classFile.methods
    } yield MethodRecord(classFile, method)
  }

  def methodsSQuOpt() = {
    import BATLifting._
    import schema.squopt._
    for {
      classFile ← classFiles.asSmart
      method ← classFile.methods
    } yield MethodRecord(classFile, method)
  }


  def methodBodiesSQuOpt() = {
    import BATLifting._
    //import schema._ //{squopt => _, _}
    import schema.squopt._
    for {
      classFile ← classFiles.asSmart
      method ← classFile.methods
      body ← method.body
    } yield ConcreteMethodRecord(classFile, method, body)
  }


  def methodBodiesModularNative() = {
    import schema._
    for {
      MethodRecord(classFile, method) <- methodsNative()
      body <- method.body
    } yield ConcreteMethodRecord(classFile, method, body) //ConcreteMethodRecord(cfM._1, cfM._2, body)
  }

  def methodBodiesModularSQuOpt() = {
    import BATLifting._
    import schema.squopt._
    for {
      cfM <- methodsSQuOpt()
      body <- cfM.method.body
    } yield ConcreteMethodRecord(cfM.classFile, cfM.method, body) //ConcreteMethodRecord(cfM._1, cfM._2, body)
  }


  def methodBodiesInstructionsModularNative() = {
    import schema._
    for {
      ConcreteMethodRecord(classFile, method, body) ← methodBodiesModularNative()
      instruction ← body.instructions
    } yield (classFile, method, body, instruction) //ConcreteMethodRecord(cfM._1, cfM._2, body)
  }

  def methodBodiesInstructionsModularSQuOpt() = {
    import BATLifting._
    import schema.squopt._
    for {
      cfMB ← methodBodiesModularSQuOpt()
      instruction ← cfMB.body.instructions
    } yield (cfMB.classFile, cfMB.method, cfMB.body, instruction)
  }


  def methodBodiesInstructionsIndexedModularNative() : Seq[schema.BytecodeInstrIndexed] = {
    import schema._
    for {
      ConcreteMethodRecord(classFile, method, body) ← methodBodiesModularNative()
      (instruction, instrIdx) ← body.instructions.zipWithIndex.filter(_._1 != null)
    } yield BytecodeInstrIndexed(classFile, method, instruction, instrIdx) //ConcreteMethodRecord(cfM._1, cfM._2, body)
  }

  def methodBodiesInstructionsIndexedModularSQuOpt() :  Exp[Seq[schema.BytecodeInstrIndexed]] = {
    import BATLifting._
    import schema.squopt._
    for {
      methodRecord ← methodBodiesModularSQuOpt()
      indexedInstr ← methodRecord.body.instructions.zipWithIndex.filter(_._1 !=# null)
    } yield BytecodeInstrIndexed(methodRecord.classFile, methodRecord.method, indexedInstr._1, indexedInstr._2)
  }

  def methodBodiesInstructionsSlidingNative(len: Int): Seq[schema.BytecodeInstrWindow] = {
    import schema._
    for {
      ConcreteMethodRecord(classFile, method, body) ← methodBodiesModularNative()
      (instrs, instrIdxes) ← body.instructions.zipWithIndex.filter(_._1 != null).sliding(len).map(_.unzip)
    } yield BytecodeInstrWindow(instrIdxes, instrs, classFile, method)
  }

  def methodBodiesInstructionsSlidingSQuOpt(len: Int): Exp[Seq[schema.BytecodeInstrWindow]] = {
    import BATLifting._
    import schema.squopt._
    for {
      methodRecord ← methodBodiesModularSQuOpt()
      window ← methodRecord.body.instructions.zipWithIndex.filter(_._1 !=# null).sliding(len).map(_.unzip)
    } yield BytecodeInstrWindow(window._2, window._1, methodRecord.classFile, methodRecord.method)
  }

 /**
   * Returns all declared fields ever read by any method in any analyzed class
   * as tuple (from,field) = ((classFile,Method)(declaringClass, name, fieldType))
   */
  def readFieldsNative: Set[((ClassFile, Method), (ObjectType, String, Type))] = {
    (for (classFile ← classFiles if !classFile.isInterfaceDeclaration;
          method ← classFile.methods if method.body.isDefined;
          instruction ← method.body.get.instructions
          if (
             instruction match {
               case _: GETFIELD  ⇒ true
               case _: GETSTATIC ⇒ true
               case _            ⇒ false
             }
             )
    ) yield {
      instruction match {
        case GETFIELD(declaringClass, name, fieldType)  ⇒ ((classFile, method), (declaringClass, name, fieldType))
        case GETSTATIC(declaringClass, name, fieldType) ⇒ ((classFile, method), (declaringClass, name, fieldType))
      }
    }).toSet
  }



  //Nested tuples don't work so well. Note the use of asExp below:
  def readFieldsSQuOpt: Exp[Set[((ClassFile, Method), (ObjectType, String, FieldType))]] = {
    import de.tud.cs.st.bat.resolved._
    import ivm._
    import expressiontree._
    import Lifting._
    import BATLifting._
    import performancetests.opaltests.InstructionLifting._
    (for (classFile ← classFiles.asSmart if !classFile.isInterfaceDeclaration;
          method ← classFile.methods if method.body.isDefined;
          instruction ← method.body.get.instructions
          if (instruction.isInstanceOf_#[GETFIELD] || instruction.isInstanceOf_#[GETSTATIC])
    ) yield {
      if_# (instruction.isInstanceOf_#[GETFIELD]) {
        val instr = instruction.asInstanceOf_#[GETFIELD]
        (asExp((classFile, method)), asExp((instr.declaringClass, instr.name, instr.fieldType)))
      } else_# if_# (instruction.isInstanceOf_#[GETFIELD]) {
        val instr = instruction.asInstanceOf_#[GETFIELD]
        (asExp((classFile, method)), asExp((instr.declaringClass, instr.name, instr.fieldType)))
      } else_# {
        null
      }
    }).toSet
  }

  /**
   * Returns a filtered sequence of instructions without the bytecode padding
   */
  def withIndexNative(instructions: Array[Instruction]): scala.collection.Seq[(Instruction, Int)] = {
    instructions.zipWithIndex.filter { case (instr, _) => instr != null }
  }



  /**
   * Returns a filtered sequence of instructions without the bytecode padding
   */
  def withIndexSQuOpt(instructions: Exp[scala.collection.Seq[Instruction]]): Exp[scala.collection.Seq[(Instruction, Int)]] = {
    import ivm.expressiontree.{Exp, Lifting}
    import Lifting._
    instructions.zipWithIndex.filter(_._1 !=# null)
    //.toSeq // RM: Okay this was an IDE Problem of IntelliJ
    //PG: I'm confused - I can omit the toSeq even now, and everything just compiles.
  }
}
