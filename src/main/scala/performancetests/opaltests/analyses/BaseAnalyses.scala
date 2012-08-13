package performancetests.opaltests.analyses

import de.tud.cs.st.bat.resolved._
import analyses.ClassHierarchy

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

  import ivm.expressiontree.Exp

  //Exp[scala.collection.immutable.Set[Exp[Exp[((Exp[ClassFile], Exp[Method])], Exp[(Exp[ObjectType], Exp[String], Exp[FieldType])])]]]
  def readFields(classFiles: Exp[Traversable[ClassFile]]) = {
    import de.tud.cs.st.bat.resolved._
    import ivm._
    import expressiontree._
    import Lifting._
    import BATLifting._
    import performancetests.opaltests.InstructionLifting._
    (for (classFile ← classFiles if !classFile.isInterfaceDeclaration;
          method ← classFile.methods if method.body.isDefined;
          instruction ← method.body.get.instructions
          if (instruction.isInstanceOf_#[GETFIELD] || instruction.isInstanceOf_#[GETSTATIC])
    ) yield {
      if (instruction.isInstanceOf_#[GETFIELD].value) { // TODO: added a _.value is this correct?
        val instr = instruction.asInstanceOf_#[GETFIELD]
        ((classFile, method), (instr.declaringClass, instr.name, instr.fieldType))
      }
      else if (instruction.isInstanceOf_#[GETFIELD].value) { // TODO: added a _.value is this correct?
        val instr = instruction.asInstanceOf_#[GETFIELD]
        ((classFile, method), (instr.declaringClass, instr.name, instr.fieldType))
      }
      else
        null
    }).toSet
  }

  // TODO this should be indexed
  def getClassFile(classFiles: Traversable[ClassFile])(t: ObjectType): Option[ClassFile] = {
    (for (classFile ← classFiles if classFile.thisClass == t) yield classFile).headOption
  }

  def getClassFile(classFiles: Exp[Traversable[ClassFile]])(t: Exp[ObjectType]) : Exp[Option[Exp[ClassFile]]] = {
    import de.tud.cs.st.bat.resolved._
    import ivm._
    import expressiontree._
    import Lifting._
    import BATLifting._
    import performancetests.opaltests.InstructionLifting._
    import ivm.expressiontree.Util.ExtraImplicits._
    val list = for (classFile ← classFiles if classFile.thisClass ==# t) yield {classFile}
    // TODO: implement this
    /*
    if(list.isEmpty.value)  // TODO: added a _.value is this correct?
    {
        None.asSmart
    }
    else
    {
        Some[Exp[ClassFile]](list.head).asSmart
    }
    //list.headOption // TODO .headOption is not defined
    */
    null
  }

  // TODO this should be indexed
  def getMethodDeclaration(classFiles: Traversable[ClassFile])(receiver: ObjectType,
                                                               methodName: String,
                                                               methodDescriptor: MethodDescriptor): Option[(ClassFile, Method)] = {
    val classFileLookup = getClassFile(classFiles)(receiver)
    for (classFile ← classFileLookup;
         methodDecl = (
              for (method ← classFile.methods
                    if method.name == methodName &&
                    method.descriptor == methodDescriptor) yield (classFile, method)
              ).headOption
          if methodDecl.isDefined
        ) yield {
        methodDecl.get
    }
  }

  def getMethodDeclaration(classFiles: Exp[Traversable[ClassFile]])(receiver: Exp[ObjectType],
                                                               methodName: Exp[String],
                                                               methodDescriptor: Exp[MethodDescriptor]): Exp[Option[(ClassFile, Method)]] = {
    import de.tud.cs.st.bat.resolved._
    import ivm._
    import expressiontree._
    import Lifting._
    import BATLifting._
    import performancetests.opaltests.InstructionLifting._
    // TODO implement this
    /*
    val classFileLookup = getClassFile(classFiles)(receiver)
    for (classFile ← classFileLookup;
         methodDecl = (
                      for (method ← classFile.methods
                           if method.name ==# methodName &&
                              method.descriptor ==# methodDescriptor) yield (classFile, method)
                      ).headOption
         if methodDecl.isDefined
    ) yield {
      methodDecl.get
    }
    */
    null
  }

  /**
   * Returns true if the method is also declared in the superclass; regardless of abstract or interface methods
   */
  def isOverride(classFiles: Traversable[ClassFile], classHierarchy: ClassHierarchy)(classFile: ClassFile)(method: Method): Boolean = {
    val superMethods = (for (superClass ← classHierarchy.superclasses(classFile.thisClass).getOrElse(Set());
                             methodDecl ← getMethodDeclaration(classFiles)(superClass, method.name, method.descriptor)
    ) yield {
      methodDecl
    })
    superMethods.size > 0

  }

  def isOverrideExp(classFiles: Exp[Seq[ClassFile]], classHierarchy: Exp[ClassHierarchy])(classFile: Exp[ClassFile])(method: Exp[Method]): Exp[Boolean] = {
    import de.tud.cs.st.bat.resolved._
    import ivm._
    import expressiontree._
    import Lifting._
    import BATLifting._
    import performancetests.opaltests.InstructionLifting._
    // TODO implement this
    /*
    val superMethods = (for (superClass ← classHierarchy.superclasses(classFile.thisClass).getOrElse(Set());
                             methodDecl ← getMethodDeclaration(classFiles)(superClass, method.name, method.descriptor)
    ) yield {
      methodDecl
    })
    superMethods.size > 0
    */
    null
  }


  /**
   * Returns the field declared in a given classFile
   */
  def findField(classFile: ClassFile)(name: String, fieldType: FieldType): Option[Field] = {
    classFile.fields.find {
                            case Field(_, `name`, `fieldType`, _) => true
                            case _                                => false
                          }
  }

  /**
   * Returns true if classFile declares the given Field
   */
  def declaresField(classFile: ClassFile)(name: String, fieldType: FieldType): Boolean = {
    classFile.fields.exists {
                              case Field(_, `name`, `fieldType`, _) => true
                              case _                                => false
                            }
  }

  /**
   * Returns the super constructor called in the given constructor or None
   */
  def calledSuperConstructor(classFiles: Traversable[ClassFile], classHierarchy: ClassHierarchy)
                            (classFile: ClassFile,
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
    getMethodDeclaration(classFiles)(targetType, name, desc)
  }

  def calledSuperConstructorExp(classFiles: Exp[Traversable[ClassFile]], classHierarchy: Exp[ClassHierarchy])
                            (classFile: Exp[ClassFile],
                             constructor: Exp[Method]): Exp[Option[(ClassFile, Method)]] = {
    // TODO implement this
    null
  }

  def calls(sourceMethod: Method, targetClass: ClassFile, targetMethod: Method): Boolean = {
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

  /**
   * Returns a filtered sequence of instructions without the bytecode padding
   */
  def withIndex(instructions: Array[Instruction]): Seq[(Instruction, Int)] = {
    instructions.zipWithIndex.filter {
                                       case (instr, _) => instr != null
                                     }
  }



  /**
   * Returns a filtered sequence of instructions without the bytecode padding
   */
  def withIndexExp(instructions: Exp[Seq[Instruction]]): Exp[Seq[(Instruction, Int)]] = {
    import ivm.expressiontree.{Exp, Lifting}
    import Lifting._
    instructions.zipWithIndex.filter(_._1 !=# null).toSeq // TODO I had to add toSeq; why did this compile earlier
  }
}
