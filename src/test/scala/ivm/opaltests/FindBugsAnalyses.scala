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

import util.perf.{ Counting, PerformanceEvaluation }
import util.graphs.{ Node, toDot }
import reader.Java6Framework

import org.junit.Test
import org.scalatest.junit.{ShouldMatchersForJUnit, JUnitSuite}
import expressiontree.Lifting
import Lifting._

/**
 * Implementation of some simple static analyses to demonstrate the flexibility
 * and power offered by Scala and BAT when analyzing class files.
 *
 * The implemented static analyses are insprired by Findbugs
 * (http://findbugs.sourceforge.net/bugDescriptions.html).
 *
 * @author Michael Eichberg
 */

object FindBugsAnalyses extends FindBugsAnalyses {
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

class FindBugsAnalyses extends JUnitSuite with ShouldMatchersForJUnit {
  private val CountingPerformanceEvaluator = new PerformanceEvaluation with Counting
  import CountingPerformanceEvaluator._
  import ivm.performancetests.Benchmarking

  private def benchMark[T](msg: String)(t: => T): T = {
    val debug = false
    Benchmarking.benchMark(msg, warmUpLoops = if (debug) 1 else 100, sampleLoops = if (debug) 2 else 50)(t)
    //Benchmarking.benchMark(msg)(t)
  }
  @Test
  def testAnalyze() {
    analyze(Seq("lib/scalatest-1.6.1.jar"))
  }

  private def analyzeConfusedInheritanceNative(classFiles: Seq[ClassFile]) = {
    val protectedFields = benchMark("CI_CONFUSED_INHERITANCE-Native") {
      (for (
        classFile ← classFiles.view if classFile.isFinal;
        field ← classFile.fields if field.isProtected
      ) yield (classFile, field))
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
    val protectedFieldsLos2 = benchMark("CI_CONFUSED_INHERITANCE-Los-setup") {
      (for (
        classFile ← classFiles.asSmartCollection if classFile.isFinal;
        field ← classFile.fields if field.isProtected
      ) yield (classFile, field))
    }
    val protectedFieldsLosRes2 = benchMark("CI_CONFUSED_INHERITANCE-Los") {
      protectedFieldsLos2.interpret()
    }
    assert(protectedFieldsLosRes2.size == protectedFields.size)

    val protectedFieldsLos = benchMark("CI_CONFUSED_INHERITANCE-Los-setup-materialize") {
      (for (
        classFile ← classFiles.asSmartCollection if classFile.isFinal;
        field ← classFile.fields if field.isProtected
      ) yield (classFile, field)) materialize
    }
    val protectedFieldsLosRes = benchMark("CI_CONFUSED_INHERITANCE-Los-interpret-noop") {
      protectedFieldsLos.interpret()
    }
    assert(protectedFieldsLosRes.size == protectedFields.size)
    val protectedFields2 = analyzeConfusedInheritanceNative(classFiles)
    assert(protectedFieldsLosRes2.size == protectedFields2.size)
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
        /*usedPrivateFields2 = //This seemed much slower in a quarter-of-scientific test - it's tested below!
(for (instruction ← instructions; GETFIELD(`declaringClass`, name, _) <- Seq(instruction)) yield name) union
(for (instruction ← instructions; GETSTATIC(`declaringClass`, name, _) <- Seq(instruction)) yield name)*/
        usedPrivateFields = instructions filter {
          case GETFIELD(`declaringClass`, name, _) => true
          case GETSTATIC(`declaringClass`, name, _) => true
          case _ => false
        } map {
          case GETFIELD(`declaringClass`, name, _) => name
          case GETSTATIC(`declaringClass`, name, _) => name
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
        usedPrivateFields = //This seemed much slower in a quarter-of-scientific test
        (for (instruction ← instructions; GETFIELD(`declaringClass`, name, _) <- Seq(instruction)) yield name) union
          (for (instruction ← instructions; GETSTATIC(`declaringClass`, name, _) <- Seq(instruction)) yield name)
        unusedPrivateFields = privateFields -- usedPrivateFields //for (field ← privateFields if !usedPrivateFields.contains(field)) yield field
        if unusedPrivateFields.size > 0
      } yield (classFile, privateFields)
    }
    println("\tViolations: " + unusedFields2.size)
  }

  def analyzeExplicitGC(classFiles: Seq[ClassFile]) {
    // FINDBUGS: Dm: Explicit garbage collection; extremely dubious except in benchmarking code (DM_GC)
    val garbageCollectingMethods: Seq[(ClassFile, Method, Instruction)] = benchMark("DM_GC") {
      val instructions = for (
        classFile ← classFiles;
        method ← classFile.methods if method.body.isDefined;
        instruction ← method.body.get.code
      ) yield (classFile, method, instruction)
      instructions.filter {
        case (a, b, instruction) ⇒
          instruction match {
            case INVOKESTATIC(ObjectType("java/lang/System"), "gc", MethodDescriptor(Seq(), VoidType)) |
                 INVOKEVIRTUAL(ObjectType("java/lang/Runtime"), "gc", MethodDescriptor(Seq(), VoidType)) ⇒ true
            case _ ⇒ false
          }
      }
    }
    println("\tViolations: " + garbageCollectingMethods.size)
  }

  def analyzePublicFinalizer(classFiles: Seq[ClassFile]) {
    // FINDBUGS: FI: Finalizer should be protected, not public (FI_PUBLIC_SHOULD_BE_PROTECTED)
    val classesWithPublicFinalizeMethods = benchMark("FI_PUBLIC_SHOULD_BE_PROTECTED") {
      for (
        classFile ← classFiles if classFile.methods.exists(method ⇒ method.name == "finalize" && method.isPublic && method.descriptor.returnType == VoidType && method.descriptor.parameterTypes.size == 0)
      ) yield classFile
    }
    println("\tViolations: " + classesWithPublicFinalizeMethods.length)
  }

  def analyzeSerializableNoConstructor(classHierarchy: ClassHierarchy, getClassFile: Map[ObjectType, ClassFile]) {
    // FINDBUGS: Se: Class is Serializable but its superclass doesn't define a void constructor (SE_NO_SUITABLE_CONSTRUCTOR)
    val serializableClasses = classHierarchy.subclasses(ObjectType("java/io/Serializable"))
    val classesWithoutDefaultConstructor = benchMark("SE_NO_SUITABLE_CONSTRUCTOR") {
      for (
        superclass ← classHierarchy.superclasses(serializableClasses) if getClassFile.isDefinedAt(superclass) && // the class file of some supertypes (defined in libraries, which we do not analyze) may not be available
        {
          val superClassFile = getClassFile(superclass)
          !superClassFile.isInterfaceDeclaration &&
            !superClassFile.constructors.exists(_.descriptor.parameterTypes.length == 0)
        }
      ) yield superclass // there can be at most one method
    }
    println("\tViolations: " + classesWithoutDefaultConstructor.size)
  }

  def analyzeCatchIllegalMonitorStateException(classFiles: Seq[ClassFile]) {
    // FINDBUGS: (IMSE_DONT_CATCH_IMSE) http://code.google.com/p/findbugs/source/browse/branches/2.0_gui_rework/findbugs/src/java/edu/umd/cs/findbugs/detect/DontCatchIllegalMonitorStateException.java
    val IllegalMonitorStateExceptionType = ObjectType("java/lang/IllegalMonitorStateException")
    val catchesIllegalMonitorStateException = benchMark("IMSE_DONT_CATCH_IMSE") {
      for (
        classFile ← classFiles if classFile.isClassDeclaration;
        method ← classFile.methods if method.body.isDefined;
        exceptionHandler ← method.body.get.exceptionTable if exceptionHandler.catchType == IllegalMonitorStateExceptionType
      ) yield (classFile, method)
    }
    println("\tViolations: " + catchesIllegalMonitorStateException.size)
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
