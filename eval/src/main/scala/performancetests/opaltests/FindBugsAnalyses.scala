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

//TODO: zipFiles -> archiveFiles (globally)

case class FBConfig(zipFiles: List[String] = Nil,
  /** This is to compare the runtime of optimized queries with FindBugs */
  onlyOptimized: Boolean = false,
  /** This is to compare the runtime of non-optimized or baseline queries with FindBugs */
  onlyBaseline: Boolean = false,
  onlyInFindBugs: Boolean = false,
  debugBench: Boolean = false,
  executionCycles: Int = 1)

object FindBugsAnalyses {
  import scopt.immutable.OptionParser
  val parser = new OptionParser[FBConfig]("FindBugsAnalyses", "0.1") {
    def options = Seq(
      //XXX: these should all use flag, not booleanOpt.
      flag("onlyOptimized", "") { _.copy(onlyOptimized = true) },
      flag("onlyBaseline", "") { _.copy(onlyBaseline = true) },
      flag("onlyInFindBugs", "") { _.copy(onlyInFindBugs = true) },
      flag("debugBench", "") { _.copy(debugBench = true) },
      intOpt("executionCycles",
        "how many cycles of each benchmark should be timed as one unit? Default 1") {
        (v, c) => c.copy(executionCycles = v)
      },
      arglistOpt("<input subtitle>", "(std. input if not specified)") {(v, c) =>
        c.copy(zipFiles = c.zipFiles :+ v)})
  }

  private def printUsage(): Unit = {
    println("Usage: java … ClassHierarchy <ZIP or JAR file containing class files>+")
    println("(c) 2011 Michael Eichberg (eichberg@informatik.tu-darmstadt.de)")
  }

  def main(args: Array[String]) {
    parser.parse(args, FBConfig()) map { config =>
      import config.zipFiles
      if (zipFiles.length == 0 || !zipFiles.forall(arg ⇒ arg.endsWith(".zip") || arg.endsWith(".jar"))) {
        printUsage
        sys.exit(1)
      }

      for (arg ← zipFiles) {
        val file = new java.io.File(arg)
        if (!file.canRead || file.isDirectory) {
          println("The file: " + file + " cannot be read.")
          printUsage()
          sys.exit(1)
        }
      }

      (new FindBugsAnalyses(config)).analyze()
    } getOrElse {
      sys.exit(1)
    }
  }
  type QueryAnd[+T] = ((ClassFile, Method, Code), T)
}

class FindBugsAnalyses(val zipFiles: List[String], override val onlyOptimized: Boolean, override val onlyBaseline: Boolean, onlyInFindBugs: Boolean, override val debugBench: Boolean, executionCycles: Int)
  extends FBAnalysesBase
  with FBUnusedFields with FBExplicitGC with FBProtectedFields with FBPublicFinalizer
  with FBSerializableNoConstructor with FBCatchIllegalMonitorStateException with FBCovariantCompareToMethods
  with FBAbstractClassesThatDefinesCovariantEquals with FBMethodsThatCallRunFinalizersOnExit
  with analyses.BX_BOXING_IMMEDIATELY_UNBOXED_TO_PERFORM_COERCION
  with analyses.DMI_LONG_BITS_TO_DOUBLE_INVOKED_ON_INT
  with analyses.DP_DO_INSIDE_DO_PRIVILEGED
  with analyses.FI_USELESS
  with analyses.ITA_INEFFICIENT_TO_ARRAY
  with analyses.MS_PKGPROTECT
  with analyses.MS_SHOULD_BE_FINAL
  with analyses.SE_BAD_FIELD_INNER_CLASS
  with analyses.SIC_INNER_SHOULD_BE_STATIC_ANON
  with analyses.SW_SWING_METHODS_INVOKED_IN_SWING_THREAD
  with analyses.UR_UNINIT_READ_CALLED_FROM_SUPER_CONSTRUCTOR
  with QueryBenchmarking
{
  import FindBugsAnalyses.QueryAnd

  override val defaultExecLoops = executionCycles

  def this(config: FBConfig) =
      this(config.zipFiles, config.onlyOptimized, config.onlyBaseline, config.onlyInFindBugs, config.debugBench, config.executionCycles)

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

  //  CN_IDIOM optimizes no abstractions away (WithAbstractions == WithoutAbstractions
  def analyzeCloneableNoClone() {
    // FINDBUGS: CN: Class implements Cloneable but does not define or use clone method (CN_IDIOM)
          // Weakness: If we only analyze a project's class files, but omit the JDK, we will not identify
          // cloneable classes in projects, where we extend a predefined
          // class (of the JDK) that indirectly inherits from Cloneable. (If an analysis does not not include the
          // JDK, we have only a partial view of the project's class hierarchy.)
    def analyzeBaseWithoutAbstractions() = {
          for {
            allCloneable ← classHierarchy.subtypes(ObjectType("java/lang/Cloneable")).toList
            cloneable ← allCloneable
            classFile ← getClassFile.get(cloneable).toList
            if !(classFile.methods exists {
              case Method(_, "clone", MethodDescriptor(CSeq(), ObjectType.Object), _) ⇒ true
              case _ ⇒ false
            })
          } yield classFile.thisClass.className
    }

    def analyzeBaseWithAbstractions() = {
      for {
              allCloneable ← classHierarchy.subtypes(ObjectType("java/lang/Cloneable")).toList
              cloneable ← allCloneable
              classFile ← getClassFile.get(cloneable).toList
              if !(classFile.methods exists {
                case Method(_, "clone", MethodDescriptor(CSeq(), ObjectType.Object), _) ⇒ true
                case _ ⇒ false
              })
            } yield classFile.thisClass.className
    }

    def analyzeSQuOptWithoutAbstractions() = {
      import BATLifting._
      for {
        allCloneable ← classHierarchy.subtypes(ObjectType("java/lang/Cloneable")).toList.asSquopt
        cloneable ← allCloneable
        classFile ← getClassFile.get(cloneable)
        if !(classFile.methods exists (method => method.name ==# "clone" && method.descriptor ==# MethodDescriptor(Seq(), ObjectType.Object)))
      } yield classFile.thisClass.className
    }

    def analyzeSQuOptWithAbstractions() = {
      import BATLifting._
      for {
        allCloneable ← classHierarchy.subtypes(ObjectType("java/lang/Cloneable")).toList.asSquopt
        cloneable ← allCloneable
        classFile ← getClassFile.get(cloneable)
        if !(classFile.methods exists (method => method.name ==# "clone" && method.descriptor ==# MethodDescriptor(Seq(), ObjectType.Object)))
      } yield classFile.thisClass.className
    }

    benchQueryComplete("NO_CLONE")(
      analyzeBaseWithoutAbstractions(),
      analyzeBaseWithAbstractions()
    )(
      analyzeSQuOptWithoutAbstractions(),
      analyzeSQuOptWithAbstractions
    )
  }

  // FB: CN_IDIOM_NO_SUPER_CALL
  def analyzeCloneDoesNotCallSuperClone() {

    def analyzeBaseWithoutAbstractions() =
    {
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
      } yield (classFile.thisClass, method.name, method.descriptor)
    }

    def analyzeBaseWithAbstractions() =
    {
      for { schema.ConcreteMethodRecord(classFile, method @ Method(_, "clone", MethodDescriptor(CSeq(), ObjectType.Object), _), body) ← methodBodiesModularNative
            if !classFile.isInterfaceDeclaration && !classFile.isAnnotationDeclaration && classFile.superClass.isDefined &&
               !(body.instructions exists {
                  case INVOKESPECIAL(superClass, "clone", MethodDescriptor(CSeq(), ObjectType.Object)) ⇒ superClass == classFile.superClass.get
                  case _ ⇒ false
                })
      } yield (classFile.thisClass, method.name, method.descriptor)
    }


    def analyzeSQuOptWithoutAbstractions() = {
      import BATLifting._
      import InstructionLifting._
      for {
        classFile ← classFiles.asSquopt
        if !classFile.isInterfaceDeclaration && !classFile.isAnnotationDeclaration
        superClass ← classFile.superClass
        method ← classFile.methods
        if method.descriptor ==# MethodDescriptor(Seq(), ObjectType.Object) && method.name ==# "clone"
        body ← method.body
        if !(body.instructions.typeFilter[INVOKESPECIAL] exists {
            instr =>
            instr.name ==# "clone" && instr.methodDescriptor ==# MethodDescriptor(Seq(), ObjectType.Object) && instr.declaringClass ==# superClass
        })
      } yield (classFile.thisClass, method.name, method.descriptor)
    }

    def analyzeSQuOptWithAbstractions() = {
      import BATLifting._
      import InstructionLifting._
      import schema.squopt._
      for { methodRecord ← methodBodiesModularSQuOpt
            if !methodRecord.classFile.isInterfaceDeclaration &&
               !methodRecord.classFile.isAnnotationDeclaration &&
                methodRecord.classFile.superClass.isDefined &&
                methodRecord.method.descriptor ==# MethodDescriptor(Seq(), ObjectType.Object) &&
                methodRecord.method.name ==# "clone" &&
                !( methodRecord.body.instructions.typeFilter[INVOKESPECIAL] exists { instr =>
                            instr.name ==# "clone" && instr.methodDescriptor ==# MethodDescriptor(Seq(), ObjectType.Object) &&
                            instr.declaringClass ==# methodRecord.classFile.superClass.get
                        })
      } yield (methodRecord.classFile.thisClass, methodRecord.method.name, methodRecord.method.descriptor)
    }

    //XXX This analysis was changed for BAT, and now adapted here; retest.
    benchQueryComplete("SUPER_CLONE_MISSING")( // FB: CN_IDIOM_NO_SUPER_CALL")
      // FINDBUGS: CN: clone method does not call super.clone() (CN_IDIOM_NO_SUPER_CALL)
      analyzeBaseWithoutAbstractions(),
      analyzeBaseWithAbstractions()
    )(
      analyzeSQuOptWithoutAbstractions(),
      analyzeSQuOptWithAbstractions()
    )
  }

  // FB: CN_IMPLEMENTS_CLONE_BUT_NOT_CLONEABLE
  def analyzeCloneButNotCloneable() {


      def analyzeBaseWithoutAbstractions() = {
        for {
          classFile ← classFiles
          if !classFile.isAnnotationDeclaration && classFile.superClass.isDefined
          method @ Method(_, "clone", MethodDescriptor(CSeq(), ObjectType.Object), _) ← classFile.methods
          if !classHierarchy.isSubtypeOf(classFile.thisClass, ObjectType("java/lang/Cloneable")).getOrElse(false)
        } yield (classFile.thisClass.className, method.name)
      }

      def analyzeBaseWithAbstractions() = {
        import schema._
        for {
          MethodRecord(classFile, method @ Method(_, "clone", MethodDescriptor(CSeq(), ObjectType.Object), _)) ← methodsNative
          if !classFile.isAnnotationDeclaration && classFile.superClass.isDefined &&
             !classHierarchy.isSubtypeOf(classFile.thisClass, ObjectType("java/lang/Cloneable")).getOrElse(false)
        } yield (classFile.thisClass.className, method.name)
      }


      def analyzeSQuOptWithoutAbstractions() = {
        import BATLifting._
        import InstructionLifting._
        for {
          classFile ← classFiles.asSquopt
          if !classFile.isAnnotationDeclaration && classFile.superClass.isDefined
          method ← classFile.methods
          if method.descriptor ==# MethodDescriptor(Seq(), ObjectType.Object) && method.name ==# "clone"
          if !asExp(classHierarchy).isSubtypeOf(classFile.thisClass, ObjectType("java/lang/Cloneable")).getOrElse(false)
        } yield (classFile.thisClass.className, method.name)
      }

      def analyzeSQuOptWithAbstractions() = {
        import BATLifting._
        import InstructionLifting._
        import schema.squopt._
        for {
          methodRecord ← methodsSQuOpt
          if !methodRecord.classFile.isAnnotationDeclaration && methodRecord.classFile.superClass.isDefined &&
              methodRecord.method.descriptor ==# MethodDescriptor(Seq(), ObjectType.Object) &&
              methodRecord.method.name ==# "clone" &&
             !asExp(classHierarchy).isSubtypeOf(methodRecord.classFile.thisClass, ObjectType("java/lang/Cloneable")).getOrElse(false)
        } yield (methodRecord.classFile.thisClass.className, methodRecord.method.name)
      }

    benchQueryComplete("NOT_CLONEABLE")(
      analyzeBaseWithoutAbstractions(),
      analyzeBaseWithAbstractions()
    )(
      analyzeSQuOptWithoutAbstractions(),
      analyzeSQuOptWithAbstractions()
    )
  }

  //Template for new tests:
  /*
  //In FindBugsAnalysesTest
  test("") {
    analyze()
  }
  //In a separate trait extended by FindBugsAnalyses
  def analyze() {
    benchQueryComplete("") {

    } {
      import BATLifting._
      import InstructionLifting._

    }
  }
   */

  // Debug code to count max method length - relevant for the issue on Streams.
  /*
  println((for {
    classFile ← classFiles
    method ← classFile.methods
    body ← method.body
  } yield (body.instructions.filter(_ != null).length, classFile.thisClass.className, method.name)).max[(Int, String, String)](Ordering.by(_._1)))
  */

  import BATLifting._

  val methodNameIdx: Exp[Map[String, Seq[(ClassFile, Method)]]] = (for {
    classFile ← classFiles.asSquopt
    method ← classFile.methods
  } yield (classFile, method)).indexBy(_._2.name)

  val excHandlerTypeIdx: Exp[Map[ObjectType, Traversable[(ClassFile, Method, Code, ExceptionHandler, ObjectType)]]] = (for {
    classFile ← classFiles.asSquopt if classFile.isClassDeclaration
    method ← classFile.methods
    body ← method.body
    exceptionHandler ← body.exceptionHandlers
    catchType ← exceptionHandler.catchType
  } yield (classFile, method, body, exceptionHandler, catchType)) indexBy (_._5)

  val typeIdxBase: Exp[Seq[QueryAnd[Instruction]]] = for {
    classFile ← classFiles.asSquopt
    method ← classFile.methods
    body ← method.body
    instruction ← body.instructions
  } yield (asExp((classFile, method, body)), instruction)
  val typeIdx: Exp[TypeMapping[Seq, QueryAnd, Instruction]] = typeIdxBase.groupByTupleType2

  if (!onlyBaseline) {
    Optimization.pushEnableDebugLog(false)

    //XXX now we have compilation, so stop playing this game. No native index please!
    benchMark("Method-name index creation (for e.g. FINALIZER_NOT_PROTECTED)"/* FB:  FI_PUBLIC_SHOULD_BE_PROTECTED*/)(Optimization.addIndex(methodNameIdx))
    benchMark("Exception-handler-type index creation (for e.g. DONT_CATCH_IMSE)"/* FB: DONT_CATCH_IMSE*/)(Optimization.addIndex(excHandlerTypeIdx))
    benchMark("Instructions type-index creation")(Optimization.addIndex(typeIdx))

    Optimization.popEnableDebugLog()
  }

  def tearDownIndexes() {
    if (!onlyOptimized && !onlyBaseline) {
      Optimization.removeIndex(methodNameIdx)
      Optimization.removeIndex(excHandlerTypeIdx)
      Optimization.removeIndex(typeIdx)
    }
  }

  def analyze() {
    if (!onlyInFindBugs) {
      analyzeUR_UNINIT_READ_CALLED_FROM_SUPER_CONSTRUCTOR()
      analyzeBOXING_IMMEDIATELY_UNBOXED_TO_PERFORM_COERCION()
      analyzeDMI_LONG_BITS_TO_DOUBLE_INVOKED_ON_INT()
      analyzeDP_DO_INSIDE_DO_PRIVILEGED()
      analyzeFI_USELESS()
      analyzeITA_INEFFICIENT_TO_ARRAY()
      analyzeMS_PKGPROTECT()
      analyzeMS_SHOULD_BE_FINAL()
      analyzeSE_BAD_FIELD_INNER_CLASS()
      analyzeSW_SWING_METHODS_INVOKED_IN_SWING_THREAD()
      analyzeSIC_INNER_SHOULD_BE_STATIC_ANON()
    }
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
}
