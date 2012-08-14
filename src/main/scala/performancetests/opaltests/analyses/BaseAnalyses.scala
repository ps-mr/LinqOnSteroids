package performancetests.opaltests.analyses

import de.tud.cs.st.bat.resolved._
import analyses.ClassHierarchy
  import ivm.expressiontree.Exp
/**
 *
 * Author: Ralf Mitschke
 * Date: 07.08.12
 * Time: 16:36
 *
 */
object BaseAnalyses {



  // TODO this should be indexed
  def getMethodDeclaration(classFiles: Traversable[ClassFile])(receiver: ObjectType,
                                                               methodName: String,
                                                               methodDescriptor: MethodDescriptor): Option[(ClassFile, Method)] = {
      /*
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
    */
    null
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


}
