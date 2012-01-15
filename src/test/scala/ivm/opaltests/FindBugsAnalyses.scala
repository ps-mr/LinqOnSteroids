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
package ivm
package opaltests

import de.tud.cs.st._
import bat.resolved._
import analyses._

import reader.Java6Framework

import org.junit.Test
import org.scalatest.junit.{ShouldMatchersForJUnit, JUnitSuite}
import expressiontree.{Exp, Lifting}
import Lifting._
import collection.TraversableView
import optimization.Optimization
import tests.TestUtil

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

    (new FindBugsAnalyses).analyze(args)
    sys.exit(0)
  }
}

class FindBugsAnalyses extends JUnitSuite with ShouldMatchersForJUnit with TestUtil {
  import ivm.performancetests.Benchmarking

  private def benchMark[T](msg: String)(t: ⇒ T): T = {
    val debug = false
    Benchmarking.benchMark(msg, warmUpLoops = if (debug) 0 else 100, sampleLoops = if (debug) 1 else 50)(t)
    //Benchmarking.benchMark(msg)(t)
  }

  @Test
  def testAnalyze() {
    analyze(Seq("lib/scalatest-1.6.1.jar"))
  }

  type QueryRes[T] = TraversableView[T, Traversable[_]]
  def optimizerTable[T]: Seq[(String, Exp[T] => Exp[T])] = Seq((" - after optimization", Optimization.optimize _))

  def benchInterpret[T](msg: String,
                        v: Exp[QueryRes[T]],
                        extraOptims: Seq[(String, Exp[Nothing] => Exp[Nothing])] = Seq.empty): Traversable[T] =
  {
    def doRun(msg: String, v: Exp[QueryRes[T]]) = {
      showExpNoVal(v, msg)
      benchMark(msg)(v.interpret().force)
    }

    val res = doRun(msg, v)
    for ((msgExtra, optim) <- optimizerTable[QueryRes[T]] ++ extraOptims.asInstanceOf[Seq[(String, Exp[QueryRes[T]] => Exp[QueryRes[T]])]]) {
      val resOpt = doRun(msg + msgExtra, optim(v))
      resOpt should be (res)
    }

    res
  }

  def benchQuery[T](msg: String,
                    v: Exp[QueryRes[T]],
                    expectedResult: Traversable[T],
                    extraOptims: Seq[(String, Exp[Nothing] => Exp[Nothing])] = Seq.empty): Traversable[T] = {
    val res = benchInterpret(msg, v, extraOptims)
    res should be (expectedResult)
    res
  }

  private def analyzeConfusedInheritanceNative(classFiles: Seq[ClassFile]) = {
    val protectedFields = benchMark("CI_CONFUSED_INHERITANCE") {
      (for {
        classFile ← classFiles.view if classFile.isFinal
        field ← classFile.fields if field.isProtected
      } yield (classFile, field)).force
    }
    println("\tViolations: " + protectedFields.size)
    protectedFields
  }

  // The following code is meant to show how easy it is to write analyses;
  // it is not meant to demonstrate how to write such analyses in an efficient
  // manner.
  private def analyzeConfusedInheritance(classFiles: Seq[ClassFile]) {
    val protectedFields = analyzeConfusedInheritanceNative(classFiles)
    // FINDBUGS: CI: Class is final but declares protected field (CI_CONFUSED_INHERITANCE) // http://code.google.com/p/findbugs/source/browse/branches/2.0_gui_rework/findbugs/src/java/edu/umd/cs/findbugs/detect/ConfusedInheritance.java
    import BATLifting._
    val protectedFieldsLos = benchMark("CI_CONFUSED_INHERITANCE Los Setup") {
      (for {
        classFile ← classFiles.asSmartCollection if classFile.isFinal
        field ← classFile.fields if field.isProtected
      } yield (classFile, field))
    }
    //XXX Should we really use force in the benchmark?
    benchQuery("CI_CONFUSED_INHERITANCE Los", protectedFieldsLos, protectedFields)


    /*val protectedFieldsLos2 = benchMark("CI_CONFUSED_INHERITANCE Los Setup (materialize)") {
      (for {
        classFile ← classFiles.asSmartCollection if classFile.isFinal
        field ← classFile.fields if field.isProtected
      } yield (classFile, field)) materialize
    }
    val protectedFieldsLos2Res = benchMark("CI_CONFUSED_INHERITANCE Los interpret noop")(protectedFieldsLos2.interpret())
    protectedFieldsLos2Res should be (protectedFields.toSet)*/
    analyzeConfusedInheritanceNative(classFiles) should be (protectedFields)
    //protectedFieldsLos2Res should be (protectedFields2.toSet)
  }

  def analyzeUnusedFields(classFiles: Seq[ClassFile]) {
    // FINDBUGS: UuF: Unused field (UUF_UNUSED_FIELD)
    val unusedFields: Seq[(ClassFile, Traversable[String])] = benchMark("UUF_UNUSED_FIELD") {
      for {
        classFile ← classFiles if !classFile.isInterfaceDeclaration
        instructions = for {
          method ← classFile.methods if method.body.isDefined
          instruction ← method.body.get.code
        } yield instruction
        declaringClass = classFile.thisClass
        privateFields = (for (field ← classFile.fields if field.isPrivate) yield field.name).toSet
        usedPrivateFields = instructions withFilter {
          case GETFIELD(`declaringClass`, name, _) ⇒ true
          case GETSTATIC(`declaringClass`, name, _) ⇒ true
          case _ ⇒ false
        } map {
          case GETFIELD(`declaringClass`, name, _) ⇒ name
          case GETSTATIC(`declaringClass`, name, _) ⇒ name
        }
        unusedPrivateFields = privateFields -- usedPrivateFields //for (field ← privateFields if !usedPrivateFields.contains(field)) yield field
        if unusedPrivateFields.size > 0
      } yield (classFile, privateFields)
    }
    println("\tViolations: " + unusedFields.size)

    val unusedFields2: Seq[(ClassFile, Traversable[String])] = benchMark("UUF_UNUSED_FIELD-2") {
      for {
        classFile ← classFiles if !classFile.isInterfaceDeclaration
        instructions = for {
          method ← classFile.methods if method.body.isDefined
          instruction ← method.body.get.code
        } yield instruction
        declaringClass = classFile.thisClass
        privateFields = (for (field ← classFile.fields if field.isPrivate) yield field.name).toSet
        usedPrivateFields = //This is much slower
        (for (instruction ← instructions; GETFIELD(`declaringClass`, name, _) ← Seq(instruction)) yield name) union
          (for (instruction ← instructions; GETSTATIC(`declaringClass`, name, _) ← Seq(instruction)) yield name)
        unusedPrivateFields = privateFields -- usedPrivateFields //for (field ← privateFields if !usedPrivateFields.contains(field)) yield field
        if unusedPrivateFields.size > 0
      } yield (classFile, privateFields)
    }
    unusedFields2 should be (unusedFields)

    import BATLifting._
    import InstructionLifting._

    val unusedFieldsLos /*: Exp[Traversable[(ClassFile, Traversable[String])]]*/ = benchMark("UUF_UNUSED_FIELD Los Setup") {
      for {
        classFile ← classFiles.asSmartCollection if !classFile.isInterfaceDeclaration
        instructions ← Let(for {
          method ← classFile.methods if method.body.isDefined
          instruction ← method.body.get.code
        } yield instruction)
        declaringClass ← Let(classFile.thisClass)
        privateFields ← Let((for (field ← classFile.fields if field.isPrivate) yield field.name).toSet)
        usedPrivateFields ← Let(instructions filter {
          instruction ⇒
            val asGETFIELD = instruction.ifInstanceOf[GETFIELD]
            val asGETSTATIC = instruction.ifInstanceOf[GETSTATIC]
            asGETFIELD.isDefined && asGETFIELD.get.declaringClass === declaringClass ||
              asGETSTATIC.isDefined && asGETSTATIC.get.declaringClass === declaringClass
        } map {
          instruction ⇒
            val asGETFIELD = instruction.ifInstanceOf[GETFIELD]
            val asGETSTATIC = instruction.ifInstanceOf[GETSTATIC]
            // Note that we might not factor map (_.name) by writing:
            //   ((asGETFIELD orElse asGETSTATIC) map (_.name)).get
            // because Scala's type system is nominal and for the two branches different (_.name) methods (with the same
            // signature) are invoked.
            (asGETFIELD map (_.name) orElse (asGETSTATIC map (_.name))).get
              //XXX: should we emulate support for `if` in some way?
            /*if (asGETFIELD.isDefined)
              asGETFIELD.name
            else if (asGETSTATIC.isDefined)
              asGETSTATIC.name*/
        })
        unusedPrivateFields ← Let(privateFields -- usedPrivateFields) //for (field ← privateFields if !usedPrivateFields.contains(field)) yield field
        if unusedPrivateFields.size > 0
      } yield (classFile, privateFields)
    }
    benchQuery("UUF_UNUSED_FIELD Los", unusedFieldsLos, unusedFields)

    val unusedFields2Los /*: Exp[Traversable[(ClassFile, Traversable[String])]]*/ = benchMark("UUF_UNUSED_FIELD-2 Los Setup") {
      for {
        classFile ← classFiles.asSmartCollection if !classFile.isInterfaceDeclaration
        instructions ← Let(for {
          method ← classFile.methods if method.body.isDefined
          instruction ← method.body.get.code
        } yield instruction)
        declaringClass ← Let(classFile.thisClass)
        privateFields ← Let((for (field ← classFile.fields if field.isPrivate) yield field.name).toSet)
        usedPrivateFields ← Let(//This is much slower, also with Los
        (for (instruction ← instructions; asGETFIELD ← instruction.ifInstanceOf[GETFIELD] if asGETFIELD.declaringClass === declaringClass) yield asGETFIELD.name) union
          (for (instruction ← instructions; asGETSTATIC ← instruction.ifInstanceOf[GETSTATIC] if asGETSTATIC.declaringClass === declaringClass) yield asGETSTATIC.name))
        unusedPrivateFields ← Let(privateFields -- usedPrivateFields) //for (field ← privateFields if !usedPrivateFields.contains(field)) yield field
        if unusedPrivateFields.size > 0
      } yield (classFile, privateFields)
    }
    benchQuery("UUF_UNUSED_FIELD-2 Los", unusedFields2Los, unusedFields)

    val unusedFields3Los /*: Exp[Traversable[(ClassFile, Traversable[String])]]*/ = benchMark("UUF_UNUSED_FIELD-3 Los Setup") {
      for {
        classFile ← classFiles.asSmartCollection if !classFile.isInterfaceDeclaration
        instructions ← Let(for {
          method ← classFile.methods if method.body.isDefined
          instruction ← method.body.get.code
        } yield instruction)
        declaringClass ← Let(classFile.thisClass)
        privateFields ← Let((for (field ← classFile.fields if field.isPrivate) yield field.name).toSet)
        usedPrivateFields ← Let(//This is much slower, but typeFilter is faster
          (for (instruction ← instructions.typeFilter[GETFIELD] if instruction.declaringClass === declaringClass) yield instruction.name) union
            (for (instruction ← instructions.typeFilter[GETSTATIC] if instruction.declaringClass === declaringClass) yield instruction.name))
        unusedPrivateFields ← Let(privateFields -- usedPrivateFields) //for (field ← privateFields if !usedPrivateFields.contains(field)) yield field
        if unusedPrivateFields.size > 0
      } yield (classFile, privateFields)
    }
    println(unusedFields3Los)
    benchQuery("UUF_UNUSED_FIELD-3 Los", unusedFields3Los, unusedFields, Seq((" - with Optim: Size To Empty", Optimization.sizeToEmpty _)))

    /*val unusedFields3LosOpt = Optimization optimize unusedFields3Los
    println(unusedFields3LosOpt)
    val unusedFields3LosOptRes = benchInterpret("UUF_UNUSED_FIELD-3 Opt Los", unusedFields3LosOpt)
    unusedFields3LosOptRes should be (unusedFields)

    val unusedFields3LosOptSzToEm = Optimization sizeToEmpty unusedFields3LosOpt
    val unusedFields3LosOptSzToEmRes = benchInterpret("UUF_UNUSED_FIELD-3 Opt Size To Empty Los", unusedFields3LosOptSzToEm)
    unusedFields3LosOptSzToEmRes should be (unusedFields)*/
  }

  def analyzeExplicitGC(classFiles: Seq[ClassFile]) {
    // FINDBUGS: Dm: Explicit garbage collection; extremely dubious except in benchmarking code (DM_GC)
    val garbageCollectingMethods: Seq[(ClassFile, Method, Instruction)] = benchMark("DM_GC") {
      val instructions = for {
        classFile ← classFiles
        method ← classFile.methods if method.body.isDefined
        instruction ← method.body.get.code
      } yield (classFile, method, instruction)
      instructions.filter {
        case (_, _, instruction) ⇒
          instruction match {
            case INVOKESTATIC(ObjectType("java/lang/System"), "gc", MethodDescriptor(Seq(), VoidType)) |
                 INVOKEVIRTUAL(ObjectType("java/lang/Runtime"), "gc", MethodDescriptor(Seq(), VoidType)) ⇒ true
            case _ ⇒ false
          }
      }
    }
    println("\tViolations: " + garbageCollectingMethods.size)

    import BATLifting._
    import InstructionLifting._

    val garbageCollectingMethodsLos = benchMark("DM_GC Los Setup") {
      val instructions = for {
        classFile ← classFiles.asSmartCollection
        method ← classFile.methods if method.body.isDefined
        instruction ← method.body.get.code
      } yield (classFile, method, instruction)
      instructions.withFilter {
        triple ⇒
          val instruction = triple._3
          val asINVOKESTATIC = instruction.ifInstanceOf[INVOKESTATIC]
          val asINVOKEVIRTUAL = instruction.ifInstanceOf[INVOKEVIRTUAL]
          val desc = MethodDescriptor(Seq(), VoidType)

          asINVOKESTATIC.isDefined && asINVOKESTATIC.get.declaringClass === ObjectType("java/lang/System") && asINVOKESTATIC.get.name == "gc" &&
            asINVOKESTATIC.get.methodDescriptor == desc ||
            asINVOKEVIRTUAL.isDefined && asINVOKEVIRTUAL.get.declaringClass === ObjectType("java/lang/Runtime") && asINVOKEVIRTUAL.get.name == "gc" &&
              asINVOKEVIRTUAL.get.methodDescriptor == desc
      }
    }
    benchQuery("DM_GC Los", garbageCollectingMethodsLos, garbageCollectingMethods)
  }

  def analyzePublicFinalizer(classFiles: Seq[ClassFile]) {
    // FINDBUGS: FI: Finalizer should be protected, not public (FI_PUBLIC_SHOULD_BE_PROTECTED)
    val classesWithPublicFinalizeMethods = benchMark("FI_PUBLIC_SHOULD_BE_PROTECTED") {
      for (
        classFile ← classFiles
        if classFile.methods.exists(method ⇒ method.name == "finalize" && method.isPublic && method.descriptor.returnType == VoidType && method.descriptor.parameterTypes.size == 0)
      ) yield classFile
    }
    println("\tViolations: " + classesWithPublicFinalizeMethods.length)
    
    import BATLifting._

    val classesWithPublicFinalizeMethodsLos = benchMark("FI_PUBLIC_SHOULD_BE_PROTECTED Los Setup") {
      for (
        classFile ← classFiles.asSmartCollection
        if classFile.methods.exists(method ⇒ method.name === "finalize" && method.isPublic && method.descriptor.returnType === VoidType && method.descriptor.parameterTypes.size === 0)
      ) yield classFile
    }
    benchQuery("FI_PUBLIC_SHOULD_BE_PROTECTED Los", classesWithPublicFinalizeMethodsLos, classesWithPublicFinalizeMethods)
  }

  def analyzeSerializableNoConstructor(classHierarchy: ClassHierarchy, getClassFile: Map[ObjectType, ClassFile]) {
    // FINDBUGS: Se: Class is Serializable but its superclass doesn't define a void constructor (SE_NO_SUITABLE_CONSTRUCTOR)
    val serializableClasses = classHierarchy.subclasses(ObjectType("java/io/Serializable")).getOrElse(Set.empty)
    val classesWithoutDefaultConstructor = benchMark("SE_NO_SUITABLE_CONSTRUCTOR") {
      for {
        superclass ← classHierarchy.superclasses(serializableClasses) if getClassFile.isDefinedAt(superclass) && // the class file of some supertypes (defined in libraries, which we do not analyze) may not be available
        {
          val superClassFile = getClassFile(superclass)
          !superClassFile.isInterfaceDeclaration &&
            !superClassFile.constructors.exists(_.descriptor.parameterTypes.length == 0)
        }
      } yield superclass // there can be at most one method
    }
    println("\tViolations: " + classesWithoutDefaultConstructor.size)

    import BATLifting._
    val classesWithoutDefaultConstructorLos = benchMark("SE_NO_SUITABLE_CONSTRUCTOR Los Setup") {
      for {
        superclass ← classHierarchy.superclasses(serializableClasses).asSmartCollection if getClassFile.asSmartCollection.isDefinedAt(superclass) && // the class file of some supertypes (defined in libraries, which we do not analyze) may not be available
        {
          val superClassFile = (getClassFile.asSmartCollection)(superclass)
          !superClassFile.isInterfaceDeclaration &&
            !superClassFile.constructors.exists(_.descriptor.parameterTypes.length == 0)
        }
      } yield superclass // there can be at most one method
    }
    benchQuery("SE_NO_SUITABLE_CONSTRUCTOR Los", classesWithoutDefaultConstructorLos, classesWithoutDefaultConstructor)
  }

  def analyzeCatchIllegalMonitorStateException(classFiles: Seq[ClassFile]) {
    // FINDBUGS: (IMSE_DONT_CATCH_IMSE) http://code.google.com/p/findbugs/source/browse/branches/2.0_gui_rework/findbugs/src/java/edu/umd/cs/findbugs/detect/DontCatchIllegalMonitorStateException.java
    val IllegalMonitorStateExceptionType = ObjectType("java/lang/IllegalMonitorStateException")
    val catchesIllegalMonitorStateException = benchMark("IMSE_DONT_CATCH_IMSE") {
      for {
        classFile ← classFiles if classFile.isClassDeclaration
        method ← classFile.methods if method.body.isDefined
        exceptionHandler ← method.body.get.exceptionTable if exceptionHandler.catchType == IllegalMonitorStateExceptionType
      } yield (classFile, method)
    }
    println("\tViolations: " + catchesIllegalMonitorStateException.size)

    import BATLifting._
    val catchesIllegalMonitorStateExceptionLos = benchMark("IMSE_DONT_CATCH_IMSE Los Setup") {
      for {
        classFile ← classFiles.asSmartCollection if classFile.isClassDeclaration
        method ← classFile.methods if method.body.isDefined
        exceptionHandler ← method.body.get.exceptionTable if exceptionHandler.catchType === IllegalMonitorStateExceptionType
      } yield (classFile, method)
    }
    benchQuery("IMSE_DONT_CATCH_IMSE Los", catchesIllegalMonitorStateExceptionLos, catchesIllegalMonitorStateException)
  }

  def analyze(zipFiles: Seq[String]) {
    val classHierarchy = new ClassHierarchy {}

    val classFiles = Benchmarking.benchMark("Reading all class files", warmUpLoops = 1, execLoops = 1) {
      for (zipFile ← zipFiles; classFile ← Java6Framework.ClassFiles(zipFile)) yield classFile
    }
    val classFilesCount = classFiles.length
    // This operation is not incrementalizable by itself. If classHierarchy supports removing classes, we might
    // provide a way to setup a listener easily.
    for (classFile ← classFiles)
      classHierarchy.update(classFile)
    //As an alternative, classHierarchy might support IVM directly.
    //classHierarchy.update(classFiles)

    val getClassFile = classFiles.map(cf ⇒ (cf.thisClass, cf)).toMap
    println("Number of class files: " + classFilesCount)

    analyzeConfusedInheritance(classFiles)
    analyzeUnusedFields(classFiles)
    analyzeExplicitGC(classFiles)
    analyzePublicFinalizer(classFiles)
    analyzeSerializableNoConstructor(classHierarchy, getClassFile)
    analyzeCatchIllegalMonitorStateException(classFiles)
  }
}
