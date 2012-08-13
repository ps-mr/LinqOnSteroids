package performancetests.opaltests.analyses

import de.tud.cs.st.bat.resolved._
import analyses.{Project, ClassHierarchy}

/**
 *
 * Author: Ralf Mitschke
 * Date: 07.08.12
 * Time: 16:36
 *
 */
object BaseAnalyses {
  /**
   * Returns all declared fields ever read by any method in any analyzed class
   * as tuple (from,field) = ((classFile,Method)(declaringClass, name, fieldType))
   */
  def readFields(classFiles: Traversable[ClassFile]): Set[((ClassFile, Method), (ObjectType, String, Type))] = {
    (for (classFile ← classFiles if !classFile.isInterfaceDeclaration;
          method ← classFile.methods if method.body.isDefined;
          instruction ← method.body.get.instructions
          if (
            instruction match {
              case _: GETFIELD ⇒ true
              case _: GETSTATIC ⇒ true
              case _ ⇒ false
            }
            )
    ) yield {
      instruction match {
        case GETFIELD(declaringClass, name, fieldType) ⇒ ((classFile, method), (declaringClass, name, fieldType))
        case GETSTATIC(declaringClass, name, fieldType) ⇒ ((classFile, method), (declaringClass, name, fieldType))
      }
    }).toSet
  }

  import ivm.expressiontree.Exp

  def readFields(classFiles: Exp[Traversable[ClassFile]]): Exp[scala.collection.immutable.Set[((Exp[ClassFile], Exp[Method]), (Exp[ObjectType], Exp[String], Exp[FieldType]))]] = {
    import de.tud.cs.st.bat.resolved._
    import ivm._
    import expressiontree._
    import Lifting._
    import BATLifting._
    import performancetests.opaltests.InstructionLifting._
    import ivm.expressiontree.Util.ExtraImplicits._
    (for (classFile ← classFiles if !classFile.isInterfaceDeclaration;
          method ← classFile.methods if method.body.isDefined;
          instruction ← method.body.get.instructions
          if ( instruction.isInstanceOf_#[GETFIELD] || instruction.isInstanceOf_#[GETSTATIC])
    ) yield {
      if( instruction.isInstanceOf_#[GETFIELD].value ){ // TODO: added a _.value is this correct?
         val instr = instruction.asInstanceOf_#[GETFIELD]
        ((classFile, method), (instr.declaringClass, instr.name, instr.fieldType))
      }
      else if( instruction.isInstanceOf_#[GETFIELD].value) // TODO: added a _.value is this correct?
      {
        val instr = instruction.asInstanceOf_#[GETFIELD]
        ((classFile, method), (instr.declaringClass, instr.name, instr.fieldType))
      }
      else
        null
    }).toSet
  }

  /**
   * Returns true if the method is also declared in the superclass; regardless of abstract or interface methods
   */
  def isOverride(project : Project)(classFile: ClassFile)(method: Method): Boolean = {
    // TODO we could also check for an @Override annotation
    val superMethods = (for (superClass ← project.classHierarchy.superclasses(classFile.thisClass).getOrElse(Set());
                             (_, method) ← project.lookupMethodDeclaration(superClass, method.name, method.descriptor)
    ) yield {
      method
    })

    superMethods.size > 0

  }

  /**
   * Returns the field declared in a given classFile
   */
  def findField(classFile: ClassFile)(name: String, fieldType: FieldType): Option[Field] = {
    classFile.fields.find {
      case Field(_, `name`, `fieldType`, _) => true
      case _ => false
    }
  }

  /**
   * Returns true if classFile declares the given Field
   */
  def declaresField(classFile: ClassFile)(name: String, fieldType: FieldType): Boolean = {
    classFile.fields.exists {
      case Field(_, `name`, `fieldType`, _) => true
      case _ => false
    }
  }

  /**
   * Returns the super constructor called in the given constructor or None
   */
  def calledSuperConstructor(project : Project)
                            (classFile: ClassFile,
                             constructor: Method): Option[(ClassFile, Method)] = {
    val superClasses = project.classHierarchy.superclasses(classFile.thisClass)
    if (!superClasses.isDefined) {
      return None
    }
    val Some((targetType, name, desc)) = constructor.body.get.instructions.collectFirst {
      case INVOKESPECIAL(trgt, n, d)
        if superClasses.get.contains(trgt.asInstanceOf[ObjectType]) =>
        (trgt.asInstanceOf[ObjectType], n, d)

    }
    project.lookupMethodDeclaration(targetType, name, desc)
  }

  def calls(sourceMethod: Method, targetClass: ClassFile, targetMethod: Method): Boolean = {
    sourceMethod.body.isDefined &&
      sourceMethod.body.get.instructions.exists {
        case INVOKEINTERFACE(targetType, name, desc) => targetClass.thisClass == targetType && targetMethod
          .name == name && targetMethod.descriptor == desc
        case INVOKEVIRTUAL(targetType, name, desc) => targetClass.thisClass == targetType && targetMethod
          .name == name && targetMethod.descriptor == desc
        case INVOKESTATIC(targetType, name, desc) => targetClass.thisClass == targetType && targetMethod
          .name == name && targetMethod.descriptor == desc
        case INVOKESPECIAL(targetType, name, desc) => targetClass.thisClass == targetType && targetMethod
          .name == name && targetMethod.descriptor == desc
        case _ => false
      }
  }

  /**
   * Returns a filtered sequence of instructions without the bytecode padding
   */
  def withIndex(instructions: Array[Instruction]): Seq[(Instruction, Int)] = {
    instructions.zipWithIndex.filter {
      case (instr, _) => instr != null
    }
  }

  import ivm.expressiontree.{Exp, Lifting}
  import Lifting._

  /**
   * Returns a filtered sequence of instructions without the bytecode padding
   */
  def withIndexExp(instructions: Exp[Seq[Instruction]]): Exp[Seq[(Instruction, Int)]] = {
    instructions.zipWithIndex.filter(_._1 !=# null)
  }
}
