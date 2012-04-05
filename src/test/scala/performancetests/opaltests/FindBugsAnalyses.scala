/* License (BSD Style License):
 *  Copyright (c) 2009, 2011
 *  Software Technology Group
 *  Department of Computer Science
 *  Technische Universität Darmstadt
 *  All rights reserved.
 *
 *  Redistribution and use in source and binary forms, with or without
 *  modification, are permitted provided that the following conditions are met:
 *
 *  - Redistributions of source code must retain the above copyright notice,
 *    this list of conditions and the following disclaimer.
 *  - Redistributions in binary form must reproduce the above copyright notice,
 *    this list of conditions and the following disclaimer in the documentation
 *    and/or other materials provided with the distribution.
 *  - Neither the name of the Software Technology Group or Technische
 *    Universität Darmstadt nor the names of its contributors may be used to
 *    endorse or promote products derived from this software without specific
 *    prior written permission.
 *
 *  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 *  AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 *  IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 *  ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
 *  LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 *  CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 *  SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 *  INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
 *  CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 *  ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 *  POSSIBILITY OF SUCH DAMAGE.
 */

package performancetests
package opaltests

import ivm._

import collections.TypeMapping
import de.tud.cs.st.bat
import bat.resolved._
import analyses._

import reader.Java6Framework

import expressiontree.{CollectionUtils, Exp, Lifting, BATLifting, Util}
import Lifting._
import Util.ExtraImplicits._
import org.scalatest.{FunSuite, BeforeAndAfterAll}
import org.scalatest.matchers.ShouldMatchers
import optimization.Optimization

import collection.immutable.Seq
import collection.{Seq => CSeq}

/**
 * Implementation of some simple static analyses to demonstrate the flexibility
 * and power offered by Scala and BAT when analyzing class files.
 *
 * The implemented static analyses are insprired by Findbugs
 * (http://findbugs.sourceforge.net/bugDescriptions.html).
 *
 * @author Michael Eichberg
 */

object FindBugsAnalyses {
  private def printUsage: Unit = {
    println("Usage: java … ClassHierarchy <ZIP or JAR file containing class files>+")
    println("(c) 2011 Michael Eichberg (eichberg@informatik.tu-darmstadt.de)")
  }

  def main(args: Array[String]) {
    if (args.length == 0 || !args.forall(arg ⇒ arg.endsWith(".zip") || arg.endsWith(".jar"))) {
      printUsage
      sys.exit(1)
    }

    for (arg ← args) {
      val file = new java.io.File(arg)
      if (!file.canRead() || file.isDirectory()) {
        println("The file: " + file + " cannot be read.");
        printUsage
        sys.exit(1)
      }
    }

    //Use toList to convert args to an immutable sequence - arrays can be converted implicitly only to generic sequences (in particular, mutable ones)!
    (new FindBugsAnalyses(args.toList)).analyze()
  }
}

class FindBugsAnalyses(zipFiles: Seq[String]) extends FunSuite with BeforeAndAfterAll with ShouldMatchers with QueryBenchmarking {
  //def this() = this(Seq("lib/scalatest-1.6.1.jar"))
  def this() = this(Seq("src/test/resources/Bugs.zip"))

  val classFiles: Seq[ClassFile] = benchMark("Reading all class files", execLoops = 1, warmUpLoops = 0, sampleLoops = 1) {
    for (zipFile ← zipFiles; classFile ← Java6Framework.ClassFiles(zipFile)) yield classFile
  }
  val getClassFile: Map[ObjectType, ClassFile] = classFiles.map(cf ⇒ (cf.thisClass, cf)).toMap

  // Now the classHierarchy is built functionally - hence, the result could be incrementally maintained (at least for the
  // addition of classes).
  val classHierarchy = (new ClassHierarchy {} /: classFiles)(_ + _)

  println("Number of class files: " + classFiles.length)
  println("Numer of methods: " + (for {
      classFile ← classFiles
      method ← classFile.methods
    } yield (classFile, method)).size)

  // The following code is meant to show how easy it is to write analyses;
  // it is not meant to demonstrate how to write such analyses in an efficient
  // manner.
  test("ConfusedInheritance") {
    analyzeConfusedInheritance()
  }
  def analyzeConfusedInheritance() {
    // FINDBUGS: CI: Class is final but declares protected field (CI_CONFUSED_INHERITANCE) // http://code.google.com/p/findbugs/source/browse/branches/2.0_gui_rework/findbugs/src/java/edu/umd/cs/findbugs/detect/ConfusedInheritance.java
    benchQueryComplete("CI_CONFUSED_INHERITANCE") {
      for {
        classFile ← classFiles if classFile.isFinal
        field ← classFile.fields if field.isProtected
      } yield (classFile, field)
    } {
      import BATLifting._
      Query(for {
        classFile ← classFiles.asSmartCollection if classFile.isFinal
        field ← classFile.fields if field.isProtected
      } yield (classFile, field))
    }
  }

  test("UnusedFields") {
    analyzeUnusedFields()
  }
  def analyzeUnusedFields() {
    // FINDBUGS: UuF: Unused field (UUF_UNUSED_FIELD)
    //XXX This analysis was totally rewritten for BAT, reconsider.
    /*val unusedFields: Seq[(ClassFile, Set[String])] = benchMark("UUF_UNUSED_FIELD") {
      for {
        classFile ← classFiles if !classFile.isInterfaceDeclaration
        instructions = for {
          method ← classFile.methods
          body ← method.body.toList
          instruction ← body.instructions
        } yield instruction
        declaringClass = classFile.thisClass
        privateFields = (for (field ← classFile.fields if field.isPrivate) yield field.name).toSet
        usedPrivateFields = instructions withFilter {
          case GETFIELD(`declaringClass`, _, _) ⇒ true
          case GETSTATIC(`declaringClass`, _, _) ⇒ true
          case _ ⇒ false
        } map {
          case GETFIELD(`declaringClass`, name, _) ⇒ name
          case GETSTATIC(`declaringClass`, name, _) ⇒ name
        }
        unusedPrivateFields = privateFields -- usedPrivateFields //for (field ← privateFields if !usedPrivateFields.contains(field)) yield field
        if unusedPrivateFields.size > 0
      } yield (classFile, privateFields)
    }*/

    benchQueryComplete("UUF_UNUSED_FIELD") {
      for {
        classFile ← classFiles if !classFile.isInterfaceDeclaration
        declaringClass = classFile.thisClass
        privateFields = (for (field ← classFile.fields if field.isPrivate) yield field.name).toSet
        unusedPrivateFields = privateFields -- (for {
          method ← classFile.methods
          body ← method.body.toList
          instruction ← body.instructions
          usedPrivateField ← instruction match {
            case GETFIELD(`declaringClass`, name, _) ⇒ Some(name)
            case GETSTATIC(`declaringClass`, name, _) ⇒ Some(name)
            case _ ⇒ None
          }
        } yield usedPrivateField) //for (field ← privateFields if !usedPrivateFields.contains(field)) yield field
        if unusedPrivateFields.size > 0
      } yield (classFile, privateFields)
    } {
      import BATLifting._
      import InstructionLifting._
      Query(for {
        classFile ← classFiles.asSmartCollection if !classFile.isInterfaceDeclaration
        instructions ← Let(for {
          method ← classFile.methods
          body ← method.body
          instruction ← body.instructions
        } yield instruction)
        declaringClass ← Let(classFile.thisClass)
        privateFields ← Let((for (field ← classFile.fields if field.isPrivate) yield field.name).toSet)
        usedPrivateFields ← Let(instructions.typeCase(
          when[GETFIELD](asGETFIELD => asGETFIELD.declaringClass ==# declaringClass, _.name),
          when[GETSTATIC](asGETSTATIC => asGETSTATIC.declaringClass ==# declaringClass, _.name)))
        unusedPrivateFields ← Let(privateFields -- usedPrivateFields) //for (field ← privateFields if !usedPrivateFields.contains(field)) yield field
        if unusedPrivateFields.size > 0
      } yield (classFile, privateFields))
    }
  }

  test("ExplicitGC") {
    analyzeExplicitGC()
  }
  def analyzeExplicitGC() {
    val NoArgNoRetMethodDesc = MethodDescriptor(Seq(), VoidType)

    // FINDBUGS: Dm: Explicit garbage collection; extremely dubious except in benchmarking code (DM_GC)
    benchQueryComplete("DM_GC") {
      (for {
        classFile ← classFiles
        method ← classFile.methods
        body ← method.body.toList
        instruction ← body.instructions
        if (instruction match {
          case INVOKESTATIC(ObjectType("java/lang/System"), "gc", NoArgNoRetMethodDesc) |
               INVOKEVIRTUAL(ObjectType("java/lang/Runtime"), "gc", NoArgNoRetMethodDesc) ⇒ true
          case _ ⇒ false
        })
      } yield (classFile, method, instruction)).toSet
    } {
      import BATLifting._
      import InstructionLifting._
      (for {
        classFile ← classFiles.asSmartCollection
        method ← classFile.methods
        body ← method.body
        instruction ← body.instructions.typeCase(
          when[INVOKESTATIC](instr =>
            instr.declaringClass ==# ObjectType("java/lang/System") && instr.name ==# "gc" && instr.methodDescriptor ==# NoArgNoRetMethodDesc, identity),
          when[INVOKEVIRTUAL](instr =>
            instr.declaringClass ==# ObjectType("java/lang/Runtime") && instr.name ==# "gc" && instr.methodDescriptor ==# NoArgNoRetMethodDesc, identity))
      } yield (classFile, method, instruction)).toSet
    }
  }

  test("PublicFinalizer") {
    analyzePublicFinalizer()
  }
  def analyzePublicFinalizer() {
    // FINDBUGS: FI: Finalizer should be protected, not public (FI_PUBLIC_SHOULD_BE_PROTECTED)
    benchQueryComplete("FI_PUBLIC_SHOULD_BE_PROTECTED") {
      for (
        classFile ← classFiles
        if classFile.methods.exists(method ⇒ method.name == "finalize" && method.isPublic && method.descriptor.returnType == VoidType && method.descriptor.parameterTypes.size == 0)
      ) yield classFile
    } {
      import BATLifting._
      Query(for (
        classFile ← classFiles.asSmartCollection
        if classFile.methods.exists(method ⇒ method.name ==# "finalize" && method.isPublic && method.descriptor.returnType ==# VoidType && method.descriptor.parameterTypes.size ==# 0)
      ) yield classFile)
    }
  }


  test("PublicFinalizer2") {
    analyzePublicFinalizer2()
  }
  def analyzePublicFinalizer2() {
    // FINDBUGS: FI: Finalizer should be protected, not public (FI_PUBLIC_SHOULD_BE_PROTECTED)
    benchQueryComplete("FI_PUBLIC_SHOULD_BE_PROTECTED-2") {
      for {
        classFile ← classFiles
        method ← classFile.methods
        if method.name == "finalize" && method.isPublic && method.descriptor.returnType == VoidType && method.descriptor.parameterTypes.size == 0
      } yield classFile
    } {
      import BATLifting._
      Query(for {
        classFile ← classFiles.asSmartCollection
        method ← classFile.methods
        if method.name ==# "finalize" && method.isPublic && method.descriptor.returnType ==# VoidType && method.descriptor.parameterTypes.size ==# 0
      } yield classFile)
    }
  }

  test("SerializableNoConstructor") {
    analyzeSerializableNoConstructor()
  }
  def analyzeSerializableNoConstructor() {
    // FINDBUGS: Se: Class is Serializable but its superclass doesn't define a void constructor (SE_NO_SUITABLE_CONSTRUCTOR)
    val serializableClasses = classHierarchy.subclasses(ObjectType("java/io/Serializable")).getOrElse(Set.empty)
    benchQueryComplete("SE_NO_SUITABLE_CONSTRUCTOR") {
      for {
        superclass ← classHierarchy.superclasses(serializableClasses) if getClassFile.isDefinedAt(superclass) && // the class file of some supertypes (defined in libraries, which we do not analyze) may not be available
        {
          val superClassFile = getClassFile(superclass)
          !superClassFile.isInterfaceDeclaration &&
            !superClassFile.constructors.exists(_.descriptor.parameterTypes.length == 0)
        }
      } yield superclass // there can be at most one method
    } {
      import BATLifting._
      Query(for {
        superclass ← classHierarchy.superclasses(serializableClasses).asSmartCollection if getClassFile.asSmartCollection.isDefinedAt(superclass) && // the class file of some supertypes (defined in libraries, which we do not analyze) may not be available
        {
          val superClassFile = (getClassFile.asSmartCollection)(superclass)
          !superClassFile.isInterfaceDeclaration &&
            !superClassFile.constructors.exists(_.descriptor.parameterTypes.length ==# 0)
        }
      } yield superclass) // there can be at most one method
    }
  }

  test("CatchIllegalMonitorStateException") {
    analyzeCatchIllegalMonitorStateException()
  }
  def analyzeCatchIllegalMonitorStateException() {
    // FINDBUGS: (IMSE_DONT_CATCH_IMSE) http://code.google.com/p/findbugs/source/browse/branches/2.0_gui_rework/findbugs/src/java/edu/umd/cs/findbugs/detect/DontCatchIllegalMonitorStateException.java
    val IllegalMonitorStateExceptionType = ObjectType("java/lang/IllegalMonitorStateException")
    benchQueryComplete("IMSE_DONT_CATCH_IMSE") {
      for {
        classFile ← classFiles if classFile.isClassDeclaration
        method ← classFile.methods
        body ← method.body.toList
        exceptionHandler ← body.exceptionHandlers if exceptionHandler.catchType == IllegalMonitorStateExceptionType
      } yield (classFile, method)
    } {
      import BATLifting._
      Query(for {
        classFile ← classFiles.asSmartCollection if classFile.isClassDeclaration
        method ← classFile.methods
        body ← method.body
        exceptionHandler ← body.exceptionHandlers if exceptionHandler.catchType ==# IllegalMonitorStateExceptionType
      } yield (classFile, method))
    }
  }

  test("CovariantCompareToMethods") {
    analyzeCovariantCompareToMethods()
  }
  def analyzeCovariantCompareToMethods() {
    val comparableType = ObjectType("java/lang/Comparable")
    benchQueryComplete("CO_SELF_NO_OBJECT/CO_ABSTRACT_SELF") {
      // Weakness: In a project, where we extend a predefined class (of the JDK) that
      // inherits from Comparable and in which we define covariant comparesTo method,
      // we will not be able to identify this issue unless we have identified the whole
      // class hierarchy.
      for {
        allComparables ← classHierarchy.subtypes(comparableType).toList
        comparable ← allComparables
        classFile ← getClassFile.get(comparable).toList
        method @ Method(_, "compareTo", MethodDescriptor(CSeq(parameterType), IntegerType), _) ← classFile.methods if parameterType != ObjectType.Object
      } yield (classFile, method)
    } {
      import BATLifting._
      Query(for {
        allComparables ← classHierarchy.subtypes(comparableType).toList.asSmartCollection
        comparable ← allComparables
        classFile ← getClassFile.get(comparable)
        method ← classFile.methods //if parameterType != ObjectType.Object
        if method.name ==# "compareTo" && method.descriptor.returnType ==# IntegerType
        parameterTypes <- Let(method.descriptor.parameterTypes)
        if parameterTypes.length ==# 1 && parameterTypes(0) !=# ObjectType.Object

      } yield (classFile, method))
    }
  }

  test("AbstractClassesThatDefinesCovariantEquals") {
    analyzeAbstractClassesThatDefinesCovariantEquals()
  }
  def analyzeAbstractClassesThatDefinesCovariantEquals() {
    //XXX This analysis was changed for BAT, reconsider.
    benchQueryComplete("EQ_ABSTRACT_SELF") {
      for {
        classFile ← classFiles
        method @ Method(_, "equals", MethodDescriptor(CSeq(parameterType), BooleanType), _) ← classFile.methods
        if  method.isAbstract && parameterType != ObjectType.Object
      } yield (classFile, method)
    } {
      import BATLifting._
      Query(for {
        classFile ← classFiles.asSmartCollection
        method ← classFile.methods
        if method.isAbstract && method.name ==# "equals" && method.descriptor.returnType ==# BooleanType
        parameterTypes <- Let(method.descriptor.parameterTypes)
        if parameterTypes.length ==# 1 && parameterTypes(0) !=# ObjectType.Object
      } yield (classFile, method))
    }
  }

  test("MethodsThatCallRunFinalizersOnExit") {
    analyzeMethodsThatCallRunFinalizersOnExit()
  }
  def analyzeMethodsThatCallRunFinalizersOnExit() {
    benchQueryComplete("DM_RUN_FINALIZERS_ON_EXIT") {
      for {
        classFile ← classFiles
        method ← classFile.methods
        body ← method.body.toList
        instruction @ INVOKESTATIC(ObjectType(recvClassName), "runFinalizersOnExit", MethodDescriptor(CSeq(BooleanType), VoidType)) ← body.instructions
        if recvClassName == "java/lang/System" || recvClassName == "java/lang/Runtime"
      } yield (classFile, method, instruction)
    } {
      import BATLifting._
      import InstructionLifting._

      for {
        classFile ← classFiles.asSmartCollection
        method ← classFile.methods
        body ← method.body
        //instruction ← body.instructions
        instruction ← body.instructions.typeCase(when[INVOKESTATIC](_.name ==# "runFinalizersOnExit", identity)) //.typeFilter[INVOKESTATIC]
        //if instruction.name ==# "runFinalizersOnExit"
        desc <- Let(instruction.methodDescriptor)
        recv <- instruction.declaringClass.ifInstanceOf[ObjectType]
        if (recv.className ==# "java/lang/System" || recv.className ==# "java/lang/Runtime") &&
          desc ==# MethodDescriptor(Seq(BooleanType), VoidType)
      } yield (classFile, method, instruction)
    }
  }

  test("CloneableNoClone") {
    analyzeCloneableNoClone()
  }
  def analyzeCloneableNoClone() {
    // FINDBUGS: CN: Class implements Cloneable but does not define or use clone method (CN_IDIOM)
    benchQueryComplete("CN_IDIOM") {
      // Weakness: We will not identify cloneable classes in projects, where we extend a predefined
      // class (of the JDK) that indirectly inherits from Cloneable. ME
      // Why? PG
      for {
        allCloneable ← classHierarchy.subtypes(ObjectType("java/lang/Cloneable")).toList
        cloneable ← allCloneable
        classFile ← getClassFile.get(cloneable).toList
        if !(classFile.methods exists {
          case Method(_, "clone", MethodDescriptor(CSeq(), ObjectType.Object), _) ⇒ true
          case _ ⇒ false
        })
      } yield classFile.thisClass.className
    } {
      import BATLifting._

      for {
        allCloneable ← classHierarchy.subtypes(ObjectType("java/lang/Cloneable")).toList.asSmartCollection
        cloneable ← allCloneable
        classFile ← getClassFile.get(cloneable)
        if !(classFile.methods exists (method => method.descriptor ==# MethodDescriptor(Seq(), ObjectType.Object) && method.name ==# "clone"))
      } yield classFile.thisClass.className
    }
  }

  test("CloneDoesNotCallSuperClone") {
    analyzeCloneDoesNotCallSuperClone()
  }
  def analyzeCloneDoesNotCallSuperClone() {
    //XXX This analysis was changed for BAT, and now adapted here; retest.
    benchQueryComplete("CN_IDIOM_NO_SUPER_CALL") {
      // FINDBUGS: CN: clone method does not call super.clone() (CN_IDIOM_NO_SUPER_CALL)
      for {
        classFile ← classFiles
        if !classFile.isInterfaceDeclaration && !classFile.isAnnotationDeclaration
        superClass ← classFile.superClass.toList
        method @ Method(_, "clone", MethodDescriptor(CSeq(), ObjectType.Object), _) ← classFile.methods
        body ← method.body
        //if !method.isAbstract //Redundant; we just check if there is a body.
        if !(body.instructions exists {
          case INVOKESPECIAL(`superClass`, "clone", MethodDescriptor(CSeq(), ObjectType.Object)) ⇒ true
          case _ ⇒ false
        })
      } yield (classFile, method)
    } {
      import BATLifting._
      import InstructionLifting._
      for {
        classFile ← classFiles.asSmartCollection
        if !classFile.isInterfaceDeclaration && !classFile.isAnnotationDeclaration
        superClass ← classFile.superClass
        method ← classFile.methods
        if method.descriptor ==# MethodDescriptor(Seq(), ObjectType.Object) && method.name ==# "clone"
        body ← method.body
        if !(body.instructions.typeFilter[INVOKESPECIAL] exists {
          instr =>
            instr.name ==# "clone" && instr.methodDescriptor ==# MethodDescriptor(Seq(), ObjectType.Object) && instr.declaringClass ==# superClass
        })
      } yield (classFile, method)
    }
  }

  test("CloneButNotCloneable") {
    analyzeCloneButNotCloneable()
  }
  def analyzeCloneButNotCloneable() {
    benchQueryComplete("CN_IMPLEMENTS_CLONE_BUT_NOT_CLONEABLE") {
        // FINDBUGS: CN: Class defines clone() but doesn't implement Cloneable (CN_IMPLEMENTS_CLONE_BUT_NOT_CLONEABLE)
      for {
        classFile ← classFiles if !classFile.isAnnotationDeclaration && classFile.superClass.isDefined
        method @ Method(_, "clone", MethodDescriptor(CSeq(), ObjectType.Object), _) ← classFile.methods
        if !classHierarchy.isSubtypeOf(classFile.thisClass, ObjectType("java/lang/Cloneable")).getOrElse(false)
      } yield (classFile.thisClass.className, method.name)
        //println("\tViolations: " /*+cloneButNotCloneable.mkString(", ")*/ +cloneButNotCloneable.size)
    } {
      import BATLifting._
      import InstructionLifting._
      for {
        classFile ← classFiles.asSmartCollection
        if !classFile.isAnnotationDeclaration && classFile.superClass.isDefined
        method ← classFile.methods
        if method.descriptor ==# MethodDescriptor(Seq(), ObjectType.Object) && method.name ==# "clone"
        //Shouldn't we have a lifter for this? Yep.
        if !onExp(classFile.thisClass)('foo, classHierarchy.isSubtypeOf(_, ObjectType("java/lang/Cloneable")).getOrElse(false))
      } yield (classFile.thisClass.className, method.name)
    }
  }

  //Template for new tests:
  /*
  test("") {
    analyze()
  }
  def analyze() {
    benchQueryComplete("") {

    } {
      import BATLifting._
      import InstructionLifting._

    }
  }
   */

  /*
  def setupAnalysis(zipFiles: Seq[String]) {
    classFiles = benchMark("Reading all class files", execLoops = 1, warmUpLoops = 0, sampleLoops = 1) {
      for (zipFile ← zipFiles; classFile ← Java6Framework.ClassFiles(zipFile)) yield classFile
    }
    // This operation is not incrementalizable by itself. If classHierarchy supports removing classes, we might
    // provide a way to setup a listener easily.
    for (classFile ← classFiles)
      classHierarchy += classFile
    //As an alternative, classHierarchy might support IVM directly.
    //classHierarchy.update(classFiles)

    getClassFile = classFiles.map(cf ⇒ (cf.thisClass, cf)).toMap
    println("Number of class files: " + classFiles.length)
  }
  */

  import BATLifting._

  val methodNameIdx: Exp[Map[String, Seq[(ClassFile, Method)]]] = (for {
    classFile ← classFiles.asSmartCollection
    method ← classFile.methods
  } yield (classFile, method)).groupBy(_._2.name)

  val excHandlerTypeIdx: Exp[Map[ObjectType, Traversable[(ClassFile, Method, Code, ExceptionHandler)]]] = (for {
    classFile ← classFiles.asSmartCollection if classFile.isClassDeclaration
    method ← classFile.methods
    body ← method.body
    exceptionHandler ← body.exceptionHandlers
  } yield (classFile, method, body, exceptionHandler)) groupBy (_._4.catchType)

  type QueryAnd[+T] = ((ClassFile, Method, Code), T)
  val typeIdxBase: Exp[Seq[QueryAnd[Instruction]]] = for {
    classFile ← classFiles.asSmartCollection
    method ← classFile.methods
    body ← method.body
    instruction ← body.instructions
  } yield (asExp((classFile, method, body)), instruction)
  val typeIdx: Exp[TypeMapping[Seq, QueryAnd, Instruction]] = typeIdxBase.groupByTupleType2

  Optimization.pushEnableDebugLog(false)

  benchMark("Method-name index creation (for e.g. FI_PUBLIC_SHOULD_BE_PROTECTED)")(Optimization.addIndex(methodNameIdx, Some(
    CollectionUtils.groupBy(for {
      classFile ← classFiles
      method ← classFile.methods
    } yield (classFile, method))(_._2.name))))
  benchMark("Exception-handler-type index creation (for e.g. IMSE_DONT_CATCH_IMSE)")(Optimization.addIndex(excHandlerTypeIdx))
  benchMark("Instructions type-index creation")(Optimization.addIndex(typeIdx))

  Optimization.popEnableDebugLog()

  //def setupIndexes() {
    /*
    import BATLifting._
    /*
    (for {
      classFile ← classFiles.asSmartCollection
      method ← classFile.methods
    } yield (classFile, method)).size
    */

    /*
    methodNameIdx = (for {
      classFile ← classFiles.asSmartCollection
      method ← classFile.methods
    } yield (classFile, method)).groupBy(_._2.name)

    val methodNameIdxVal = (for {
      classFile ← classFiles
      method ← classFile.methods
    } yield (classFile, method)).groupBy(_._2.name)

    val idxBase = for {
      classFile ← classFiles.asSmartCollection if classFile.isClassDeclaration
      method ← classFile.methods
      body ← method.body
      exceptionHandler ← body.exceptionHandlers
    } yield (classFile, method, body, exceptionHandler)
    excHandlerTypeIdx = idxBase groupBy (_._4.catchType)
    */

    */
  //}

  def tearDownIndexes() {
    Optimization.removeIndex(methodNameIdx)
    Optimization.removeIndex(excHandlerTypeIdx)
    Optimization.removeIndex(typeIdx)
  }

  def analyze() {
    analyzeConfusedInheritance()
    analyzeUnusedFields()
    analyzeExplicitGC()
    analyzePublicFinalizer()
    analyzePublicFinalizer2()
    analyzeSerializableNoConstructor()
    analyzeCatchIllegalMonitorStateException()
    analyzeCovariantCompareToMethods()
    analyzeAbstractClassesThatDefinesCovariantEquals()
    analyzeMethodsThatCallRunFinalizersOnExit()
    analyzeCloneableNoClone()
    analyzeCloneDoesNotCallSuperClone()
    analyzeCloneButNotCloneable()
  }

  override def afterAll() {
    tearDownIndexes()
  }
}
