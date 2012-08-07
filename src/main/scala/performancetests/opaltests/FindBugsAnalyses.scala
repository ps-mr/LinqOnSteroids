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

import expressiontree.{CollectionUtils, Exp, Lifting, BATLifting, Util}
import expressiontree._ //We import this just for being able to resolve identifiers in code dumps.
import Lifting._
import Util.ExtraImplicits._
import optimization.Optimization

import org.scalatest.{FunSuite, BeforeAndAfterAll}
import org.scalatest.matchers.ShouldMatchers

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
  private def printUsage(): Unit = {
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
      if (!file.canRead || file.isDirectory) {
        println("The file: " + file + " cannot be read.")
        printUsage()
        sys.exit(1)
      }
    }

    //Use toList to convert args to an immutable sequence - arrays can be converted implicitly only to generic sequences (in particular, mutable ones)!
    (new FindBugsAnalyses(args.toList)).analyze()
  }
  type QueryAnd[+T] = ((ClassFile, Method, Code), T)
}

class FindBugsAnalyses(val zipFiles: Seq[String])
  extends FBAnalysesBase
  with FBUnusedFields with FBExplicitGC with FBProtectedFields with FBPublicFinalizer
  with FBSerializableNoConstructor with FBCatchIllegalMonitorStateException with FBCovariantCompareToMethods
  with FBAbstractClassesThatDefinesCovariantEquals with FBMethodsThatCallRunFinalizersOnExit
  with FunSuite with BeforeAndAfterAll with ShouldMatchers with QueryBenchmarking
{
  import FindBugsAnalyses.QueryAnd

  //override val defaultExecLoops = 10

  //def this() = this(Seq("src/test/resources/scalatest-1.6.1.jar"))
  def this() = this(Seq("src/test/resources/Bugs.zip"))

  //This is to have a run comparable with FindBugs
  //override def onlyOptimized = true
  //Standard execution
  override def onlyOptimized = false

  /* XXX:
   * This test is currently pointless. Either I do it with a single query, where it'll benchmark cache lookup time;
   * or I do it with multiple queries, and it'll fill the compilation cache with alpha-equivalent but different queries
   * and measure GC performance on a memory leaking program which is calling the Scala compiler many times.
   * The cache should replace constants with different nodes (say, NamedVar's) and then check alpha-equivalence using
   * standard equality.
   */
/*
  test("Compilation") {
    //benchMark("Compiling methodBodiesSQuOpt"){Compile.toValue(methodBodiesSQuOpt())} should be (methodBodiesSQuOpt().interpret())
/*
    val query = methodBodiesSQuOpt()
    benchMark("Compiling methodBodiesSQuOpt"){Compile.toValue(query)} should be (query.interpret())
*/
  }
*/

  // The following code is meant to show how easy it is to write analyses;
  // it is not meant to demonstrate how to write such analyses in an efficient
  // manner.
  test("ProtectedField") {
    analyzeProtectedFields()
  }

  test("UnusedFields") {
    analyzeUnusedFields()
  }

  test("ExplicitGC") {
    analyzeExplicitGC()
  }

  test("PublicFinalizer") {
    analyzePublicFinalizer()
  }

  test("PublicFinalizer2") {
    analyzePublicFinalizer2()
  }

  test("SerializableNoConstructor") {
    analyzeSerializableNoConstructor()
  }

  test("CatchIllegalMonitorStateException") {
    analyzeCatchIllegalMonitorStateException()
  }

  test("CovariantCompareToMethods") {
    analyzeCovariantCompareToMethods()
  }

  test("AbstractClassesThatDefinesCovariantEquals") {
    analyzeAbstractClassesThatDefinesCovariantEquals()
  }

  test("MethodsThatCallRunFinalizersOnExit") {
    analyzeMethodsThatCallRunFinalizersOnExit()
  }

  test("CloneableNoClone") {
    analyzeCloneableNoClone()
  }
  def analyzeCloneableNoClone() {
    // FINDBUGS: CN: Class implements Cloneable but does not define or use clone method (CN_IDIOM)
    benchQueryComplete("NO_CLONE"){//  CN_IDIOM") {
      // Weakness: If we only analyze a project's class files, but omit the JDK, we will not identify
      // cloneable classes in projects, where we extend a predefined
      // class (of the JDK) that indirectly inherits from Cloneable. (If an analysis does not not include the
      // JDK, we have only a partial view of the project's class hierarchy.)
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
        allCloneable ← classHierarchy.subtypes(ObjectType("java/lang/Cloneable")).toList.asSmart
        cloneable ← allCloneable
        classFile ← getClassFile.get(cloneable)
        if !(classFile.methods exists (method => method.descriptor ==# MethodDescriptor(Seq(), ObjectType.Object) && method.name ==# "clone"))
      } yield classFile.thisClass.className
    }
  }

  test("SuperCloneMissing") {
    analyzeCloneDoesNotCallSuperClone()
  }
  def analyzeCloneDoesNotCallSuperClone() {
    //XXX This analysis was changed for BAT, and now adapted here; retest.
    benchQueryComplete("SUPER_CLONE_MISSING"){ // FB: CN_IDIOM_NO_SUPER_CALL") {
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
        classFile ← classFiles.asSmart
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

  test("NotCloneable") {
    analyzeCloneButNotCloneable()
  }
  def analyzeCloneButNotCloneable() {
    benchQueryComplete("NOT_CLONEABLE"){ // FB: CN_IMPLEMENTS_CLONE_BUT_NOT_CLONEABLE") {
        // FINDBUGS: CN: Class defines clone() but doesn't implement Cloneable (CN_IMPLEMENTS_CLONE_BUT_NOT_CLONEABLE)
      for {
        classFile ← classFiles
        if !classFile.isAnnotationDeclaration && classFile.superClass.isDefined
        method @ Method(_, "clone", MethodDescriptor(CSeq(), ObjectType.Object), _) ← classFile.methods
        if !classHierarchy.isSubtypeOf(classFile.thisClass, ObjectType("java/lang/Cloneable")).getOrElse(false)
      } yield (classFile.thisClass.className, method.name)
        //println("\tViolations: " /*+cloneButNotCloneable.mkString(", ")*/ +cloneButNotCloneable.size)
    } {
      import BATLifting._
      import InstructionLifting._
      for {
        classFile ← classFiles.asSmart
        if !classFile.isAnnotationDeclaration && classFile.superClass.isDefined
        method ← classFile.methods
        if method.descriptor ==# MethodDescriptor(Seq(), ObjectType.Object) && method.name ==# "clone"
        //Shouldn't we have a lifter for this? Yep.
        if !asExp(classHierarchy).isSubtypeOf(classFile.thisClass, ObjectType("java/lang/Cloneable")).getOrElse(false)
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
    classFile ← classFiles.asSmart
    method ← classFile.methods
  } yield (classFile, method)).indexBy(_._2.name)

  val excHandlerTypeIdx: Exp[Map[ObjectType, Traversable[(ClassFile, Method, Code, ExceptionHandler, ObjectType)]]] = (for {
    classFile ← classFiles.asSmart if classFile.isClassDeclaration
    method ← classFile.methods
    body ← method.body
    exceptionHandler ← body.exceptionHandlers
    catchType ← exceptionHandler.catchType
  } yield (classFile, method, body, exceptionHandler, catchType)) indexBy (_._5)

  val typeIdxBase: Exp[Seq[QueryAnd[Instruction]]] = for {
    classFile ← classFiles.asSmart
    method ← classFile.methods
    body ← method.body
    instruction ← body.instructions
  } yield (asExp((classFile, method, body)), instruction)
  val typeIdx: Exp[TypeMapping[Seq, QueryAnd, Instruction]] = typeIdxBase.groupByTupleType2

  Optimization.pushEnableDebugLog(false)

  benchMark("Method-name index creation (for e.g. FINALIZER_NOT_PROTECTED)"/* FB:  FI_PUBLIC_SHOULD_BE_PROTECTED*/)(Optimization.addIndex(methodNameIdx, Some(
    CollectionUtils.groupBy(for {
      classFile ← classFiles
      method ← classFile.methods
    } yield (classFile, method))(_._2.name))))
  benchMark("Exception-handler-type index creation (for e.g. DONT_CATCH_IMSE)"/* FB: DONT_CATCH_IMSE*/)(Optimization.addIndex(excHandlerTypeIdx))
  benchMark("Instructions type-index creation")(Optimization.addIndex(typeIdx))

  Optimization.popEnableDebugLog()

  //def setupIndexes() {
    /*
    import BATLifting._
    /*
    (for {
      classFile ← classFiles.asSmart
      method ← classFile.methods
    } yield (classFile, method)).size
    */

    /*
    methodNameIdx = (for {
      classFile ← classFiles.asSmart
      method ← classFile.methods
    } yield (classFile, method)).indexBy(_._2.name)

    val methodNameIdxVal = (for {
      classFile ← classFiles
      method ← classFile.methods
    } yield (classFile, method)).indexBy(_._2.name)

    val idxBase = for {
      classFile ← classFiles.asSmart if classFile.isClassDeclaration
      method ← classFile.methods
      body ← method.body
      exceptionHandler ← body.exceptionHandlers
    } yield (classFile, method, body, exceptionHandler)
    excHandlerTypeIdx = idxBase indexBy (_._4.catchType)
    */

    */
  //}

  def tearDownIndexes() {
    if (!onlyOptimized) {
      Optimization.removeIndex(methodNameIdx)
      Optimization.removeIndex(excHandlerTypeIdx)
      Optimization.removeIndex(typeIdx)
    }
  }

  def analyze() {
    analyzeProtectedFields()
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
