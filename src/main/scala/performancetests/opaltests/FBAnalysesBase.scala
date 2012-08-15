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
        NULL //null causes problems!
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

    def getMethodDeclarationNative(receiver: ObjectType,
                                     methodName: String,
                                     methodDescriptor: MethodDescriptor): Option[(ClassFile, Method)] = {
      val classFileLookup = getClassFile.get(receiver)
      for (classFile ← classFileLookup;
           methodDecl ← (
                for (method ← classFile.methods
                      if method.name == methodName &&
                      method.descriptor == methodDescriptor) yield (classFile, method)
                ).headOption
          ) yield methodDecl
    }

    def getMethodDeclarationSQuOpt(receiver: Exp[ObjectType],
                                   methodName: Exp[String],
                                   methodDescriptor: Exp[MethodDescriptor]): Exp[Option[(ClassFile, Method)]] = {
      import de.tud.cs.st.bat.resolved._
      import ivm._
      import expressiontree._
      import Lifting._
      import BATLifting._
      import performancetests.opaltests.InstructionLifting._
      val classFileLookup : Exp[Option[ClassFile]] = getClassFileSQuOpt.get(receiver)

      if_# (!classFileLookup.isDefined) {
                                       return None
      } else_# {
          val classFile = classFileLookup.get
          (for (method ← classFile.methods
               if method.name ==# methodName && method.descriptor ==# methodDescriptor
               ) yield (classFile, method)
          ).headOption
      }
    }

  /**
   * Returns true if the method is also declared in the superclass; regardless of abstract or interface methods
   */
  def isOverrideNative(classFile: ClassFile)(method: Method): Boolean = {
    (for (superClass ← classHierarchy.superclasses(classFile.thisClass).getOrElse(Set());
                             methodDecl ← getMethodDeclarationNative(superClass, method.name, method.descriptor)
    ) yield {
      methodDecl
    }).size > 0
  }

    /**
     * Returns true if the method is also declared in the superclass; regardless of abstract or interface methods
     */
    def isOverrideSQuOpt(classFile: Exp[ClassFile])(method: Exp[Method]): Exp[Boolean] = {
      import de.tud.cs.st.bat.resolved._
      import ivm._
      import expressiontree._
      import Lifting._
      import BATLifting._
      import performancetests.opaltests.InstructionLifting._
      (for (superClass ← classHierarchySQuOpt.superclasses(classFile.thisClass).getOrElse(Set());
                               methodDecl ← getMethodDeclarationSQuOpt(superClass, method.name, method.descriptor)
      ) yield {
        methodDecl
      }).size > 0
    }

  /**
   * Returns true if classFile declares the given Field
   */
  def declaresFieldNative(classFile: ClassFile)(name: String, fieldType: FieldType): Boolean = {
    classFile.fields.exists {
                              case Field(_, `name`, `fieldType`, _) => true
                              case _                                => false
                            }
  }

  def declaresFieldSQuOpt(classFile: Exp[ClassFile])(name: Exp[String], fieldType: Exp[FieldType]): Exp[Boolean] = {
    import de.tud.cs.st.bat.resolved._
    import ivm._
    import expressiontree._
    import Lifting._
    import BATLifting._
    import performancetests.opaltests.InstructionLifting._
    classFile.fields.exists { field => field.name ==# name && field.fieldType ==# fieldType }
  }

  /**
   * Returns the super constructor called in the given constructor or None
   */
  def calledSuperConstructorNative(classFile: ClassFile,
                                   constructor: Method): Option[(ClassFile, Method)] = {
    val superClasses = classHierarchy.superclasses(classFile.thisClass)
    if (!superClasses.isDefined) {
      return None
    }
    val Some((targetType, name, desc)) = constructor.body.get.instructions.collectFirst {
                                                                                          case INVOKESPECIAL(trgt, n, d)
                                                                                            if superClasses.get.contains(trgt.asInstanceOf[ObjectType]) =>
                                                                                            (trgt.asInstanceOf[ObjectType], n, d)

                                                                                        }
    getMethodDeclarationNative(targetType, name, desc)
  }

  def calledSuperConstructorSQuOpt(classFile: Exp[ClassFile],
                                   constructor: Exp[Method]): Exp[Option[(ClassFile, Method)]] = {
    import de.tud.cs.st.bat.resolved._
    import ivm._
    import expressiontree._
    import Lifting._
    import BATLifting._
    import performancetests.opaltests.InstructionLifting._


    val classType : Exp[ObjectType] =classFile.thisClass
    val superClasses : Exp[Option[Set[ObjectType]]] = classHierarchySQuOpt.superclasses(classType)
    if_# (!superClasses.isDefined) {
      return None
    } else_# {
      // TODO requires collectFirst
      //[error] D:\workspace\LinqOnSteroids\src\main\scala\performancetests\opaltests\FBAnalysesBase.scala:349: value collectFirst is not a member of ivm.expressiontree.Exp[Seq[de.tud.cs.st.bat.resolved.Instruction]]
      val methodCall : Exp[Option[INVOKESPECIAL]] = None /* constructor.body.get.instructions.collectFirst {
          def isDefinedAt(x:Instruction) = x.isInstanceOf_#[INVOKESPECIAL] &&  superClasses.get.contains(x.asInstanceOf_#[INVOKESPECIAL].declaringClass.asInstanceOf_#[ObjectType])
          def apply(x:Instruction) = x.asInstanceOf_#[INVOKESPECIAL]
      }*/
      if_# (!methodCall.isDefined) {
        return None
      } else_# {
        return getMethodDeclarationSQuOpt(methodCall.get.declaringClass.asInstanceOf_#[ObjectType], methodCall.get.name, methodCall.get.methodDescriptor)
      }
    }
  }

  def callsNative(sourceMethod: Method, targetClass: ClassFile, targetMethod: Method): Boolean = {
    sourceMethod.body.isDefined &&
    sourceMethod.body.get.instructions.exists {
                                                case INVOKEINTERFACE(targetType, name, desc) => targetClass.thisClass == targetType && targetMethod
                                                                                                                                       .name == name && targetMethod.descriptor == desc
                                                case INVOKEVIRTUAL(targetType, name, desc)   => targetClass.thisClass == targetType && targetMethod
                                                                                                                                       .name == name && targetMethod.descriptor == desc
                                                case INVOKESTATIC(targetType, name, desc)    => targetClass.thisClass == targetType && targetMethod
                                                                                                                                       .name == name && targetMethod.descriptor == desc
                                                case INVOKESPECIAL(targetType, name, desc)   => targetClass.thisClass == targetType && targetMethod
                                                                                                                                       .name == name && targetMethod.descriptor == desc
                                                case _                                       => false
                                              }
  }

  def callsSQuOpt(sourceMethod: Exp[Method], targetClass: Exp[ClassFile], targetMethod: Exp[Method]): Exp[Boolean ]= {
    import de.tud.cs.st.bat.resolved._
    import ivm._
    import expressiontree._
    import Lifting._
    import BATLifting._
    import performancetests.opaltests.InstructionLifting._
    // TODO in future versions it would be nice to compare to concrete instances directly ( but just if pattern matching does not work, otherwise I could do the pattern matching)
    /*
    val targetINVOKEINTERFACE = INVOKEINTERFACE(targetClass.thisClass, targetMethod.name, targetMethod.descriptor)
    val targetINVOKEVIRTUAL = INVOKEVIRTUAL(targetClass.thisClass, targetMethod.name, targetMethod.descriptor)
    val targetINVOKESTATIC = INVOKESTATIC(targetClass.thisClass, targetMethod.name, targetMethod.descriptor)
    val targetINVOKESPECIAL = INVOKESPECIAL(targetClass.thisClass, targetMethod.name, targetMethod.descriptor)
    sourceMethod.body.isDefined &&
    sourceMethod.body.get.instructions.exists {
        instr => instr ==# targetINVOKEINTERFACE || instr ==# targetINVOKEVIRTUAL || instr ==# targetINVOKESTATIC || instr ==# targetINVOKESPECIAL
    }
    */

    sourceMethod.body.isDefined &&
    sourceMethod.body.get.instructions.exists {
        instr => (
           (instr.isInstanceOf_#[INVOKEINTERFACE] && { val invoke = instr.asInstanceOf_#[INVOKEINTERFACE]; invoke.declaringClass ==# targetClass.thisClass && invoke.name ==# targetMethod.name && invoke.methodDescriptor ==# targetMethod.descriptor }) ||
           (instr.isInstanceOf_#[INVOKEVIRTUAL] && { val invoke = instr.asInstanceOf_#[INVOKEVIRTUAL]; invoke.declaringClass ==# targetClass.thisClass && invoke.name ==# targetMethod.name && invoke.methodDescriptor ==# targetMethod.descriptor }) ||
           (instr.isInstanceOf_#[INVOKESTATIC] && { val invoke = instr.asInstanceOf_#[INVOKESTATIC]; invoke.declaringClass ==# targetClass.thisClass && invoke.name ==# targetMethod.name && invoke.methodDescriptor ==# targetMethod.descriptor }) ||
           (instr.isInstanceOf_#[INVOKESPECIAL] && { val invoke = instr.asInstanceOf_#[INVOKESPECIAL]; invoke.declaringClass ==# targetClass.thisClass && invoke.name ==# targetMethod.name && invoke.methodDescriptor ==# targetMethod.descriptor })
           )
    }
  }
}
