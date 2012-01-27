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

import expressiontree.{Lifting, FuncExp, TernaryOpExp, Exp, Util}
import Lifting._
import Util.ExtraImplicits._
import optimization.Optimization
import tests.TestUtil
import performancetests.Benchmarking
import org.scalatest.{FunSuite, BeforeAndAfterAll}
import org.scalatest.matchers.ShouldMatchers

/**
 * Implementation of some simple static analyses to demonstrate the flexibility
 * and power offered by Scala and BAT when analyzing class files.
 *
 * The implemented static analyses are insprired by Findbugs
 * (http://findbugs.sourceforge.net/bugDescriptions.html).
 *
 * @author Michael Eichberg
 */

object Sugar {
  case class IfThenElse[T](cond: Exp[Boolean], thenBody: Exp[T], elseBody: Exp[T]) extends TernaryOpExp[Boolean, T, T, T](cond, thenBody, elseBody) {
    def interpret() = if (cond.interpret()) thenBody.interpret() else elseBody.interpret()
    def copy(cond: Exp[Boolean], thenBody: Exp[T], elseBody: Exp[T]) = IfThenElse(cond, thenBody, elseBody)
  }

  /*class Elseable[T](cond: Exp[Boolean], thenBody: Exp[T]) {
    def else_[U >: T](elseBody: Exp[U]): Exp[U] = IfThenElse(cond, thenBody, elseBody)
    //def elseif_(newCond: Exp[Boolean], newThenBody: Exp[T]) = new Elseable(newCond, newThenBody)
  }
  def if_[T](cond: Exp[Boolean])(thenBody: Exp[T]) = new Elseable(cond, thenBody)*/

  case class Elseable[T](conds: Seq[Exp[Boolean]], bodies: Seq[Exp[T]]) {
    def else_[U >: T](elseBody: Exp[U]): Exp[U] =
      (conds, bodies).zipped.foldRight(elseBody) {
        case ((cond, thenBody), curr) => IfThenElse(cond, thenBody, curr)
      }
    //This overload allows chaining if-else if. The idea comes from:
    //http://blog.razie.com/2011/08/scala-dsl-technique-if-else-constructs.html
    def else_[U >: T](branch: Elseable[U]) = Elseable(conds ++ branch.conds, bodies ++ branch.bodies)
    //def elseif_[U >: T](newCond: Exp[Boolean])(newThenBody: Exp[U]) = new Elseable(conds :+ newCond, bodies :+ newThenBody)
  }
  def if_[T](cond: Exp[Boolean])(thenBody: Exp[T]) = Elseable(Seq(cond), Seq(thenBody))
}

class TestSugar extends FunSuite with ShouldMatchers {
  import Sugar._
  test("if-1") {
    (if_ (asExp(1) + 2 === 4) {1} else_ if_ (asExp(1) + 2 === 3) {2} else_ 3).expResult() should be (2)
  }

  test("if should work in a function body") {
    val f: FuncExp[Int, Int] = FuncExp(x => (if_ (x % 3 === 0) {0} else_ if_ (x % 3 === 1) {1} else_ 2))
    for (i <- 0 to 2)
     f(i).expResult() should be (i)
  }
}

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
  }
}

class FindBugsAnalyses extends FunSuite with BeforeAndAfterAll with ShouldMatchers with TestUtil with Benchmarking {
  import Sugar._

  val classHierarchy = new ClassHierarchy {}
  var classFiles: Seq[ClassFile] = _
  var getClassFile: Map[ObjectType, ClassFile] = _

  def optimizerTable[T]: Seq[(String, Exp[T] => Exp[T])] = Seq((" - after optimization", identity _))

  def benchInterpret[T, Coll <: Traversable[T]](msg: String,
                        v: Exp[Coll],
                        extraOptims: Seq[(String, Exp[Nothing] => Exp[Nothing])] = Seq.empty)(implicit f: Forceable[T, Coll]): Traversable[T] =
  {
    def doRun(msg: String, v: Exp[Coll]) = {
      showExpNoVal(v, msg)
      benchMark(msg)(v.expResult().force)
    }

    val res = doRun(msg, v)
    for ((msgExtra, optim) <- optimizerTable[Coll] ++ extraOptims.asInstanceOf[Seq[(String, Exp[Coll] => Exp[Coll])]]) {
      val resOpt = doRun(msg + msgExtra, optim(Optimization.optimize(v)))
      resOpt should be (res)
    }

    res
  }

  def benchQuery[T, Coll <: Traversable[T]](msg: String,
                    v: Exp[Coll],
                    expectedResult: Traversable[T],
                    extraOptims: Seq[(String, Exp[Nothing] => Exp[Nothing])] = Seq.empty)(implicit f: Forceable[T, Coll]): Traversable[T] = {
    val res = benchInterpret[T, Coll](msg, v, extraOptims)
    res should be (expectedResult)
    res
  }

  private def analyzeConfusedInheritanceNative() = {
    val protectedFields = benchMark("CI_CONFUSED_INHERITANCE") {
      for {
        classFile ← classFiles if classFile.isFinal
        field ← classFile.fields if field.isProtected
      } yield (classFile, field)
    }
    println("\tViolations: " + protectedFields.size)
    protectedFields
  }

  // The following code is meant to show how easy it is to write analyses;
  // it is not meant to demonstrate how to write such analyses in an efficient
  // manner.
  test("ConfusedInheritance") {
    analyzeConfusedInheritance()
  }
  def analyzeConfusedInheritance() {
    val protectedFields = analyzeConfusedInheritanceNative()
    // FINDBUGS: CI: Class is final but declares protected field (CI_CONFUSED_INHERITANCE) // http://code.google.com/p/findbugs/source/browse/branches/2.0_gui_rework/findbugs/src/java/edu/umd/cs/findbugs/detect/ConfusedInheritance.java
    import BATLifting._
    val protectedFieldsLos = benchMark("CI_CONFUSED_INHERITANCE Los Setup") {
      Query(for {
        classFile ← classFiles.asSmartCollection if classFile.isFinal
        field ← classFile.fields if field.isProtected
      } yield (classFile, field))
    }
    benchQuery("CI_CONFUSED_INHERITANCE Los", protectedFieldsLos, protectedFields)

    val protectedFieldsLikeLos = benchMark("CI_CONFUSED_INHERITANCE Native Like Los") {
      (for {
        classFile ← classFiles.view if classFile.isFinal
        field ← classFile.fields.view if field.isProtected
      } yield (classFile, field)).force
    }

    protectedFieldsLikeLos should be (protectedFields)


    /*val protectedFieldsLos2 = benchMark("CI_CONFUSED_INHERITANCE Los Setup (materialize)") {
      (for {
        classFile ← classFiles.asSmartCollection if classFile.isFinal
        field ← classFile.fields if field.isProtected
      } yield (classFile, field)) materialize
    }
    val protectedFieldsLos2Res = benchMark("CI_CONFUSED_INHERITANCE Los interpret noop")(protectedFieldsLos2.interpret())
    protectedFieldsLos2Res should be (protectedFields.toSet)*/
    analyzeConfusedInheritanceNative() should be (protectedFields)
    //protectedFieldsLos2Res should be (protectedFields2.toSet)
  }

  test("UnusedFields") {
    analyzeUnusedFields()
  }
  def analyzeUnusedFields() {
    // FINDBUGS: UuF: Unused field (UUF_UNUSED_FIELD)
    val unusedFields: Seq[(ClassFile, Set[String])] = benchMark("UUF_UNUSED_FIELD") {
      for {
        classFile ← classFiles if !classFile.isInterfaceDeclaration
        instructions = for {
          method ← classFile.methods if method.body.isDefined
          instruction ← method.body.get.code
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
    }
    println("\tViolations: " + unusedFields.size)

    val unusedFields2: Seq[(ClassFile, Set[String])] = benchMark("UUF_UNUSED_FIELD-2") {
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

    val unusedFields3: Seq[(ClassFile, Set[String])] = benchMark("UUF_UNUSED_FIELD-3") {
      for {
        classFile ← classFiles if !classFile.isInterfaceDeclaration
        declaringClass = classFile.thisClass
        privateFields = (for (field ← classFile.fields if field.isPrivate) yield field.name).toSet
        unusedPrivateFields = privateFields -- (for {
          method ← classFile.methods if method.body.isDefined
          instruction ← method.body.get.code
          usedPrivateField ← instruction match {
            case GETFIELD(`declaringClass`, name, _) ⇒ Some(name)
            case GETSTATIC(`declaringClass`, name, _) ⇒ Some(name)
            case _ ⇒ None
          }
        } yield usedPrivateField) //for (field ← privateFields if !usedPrivateFields.contains(field)) yield field
        if unusedPrivateFields.size > 0
      } yield (classFile, privateFields)
    }

    unusedFields3 should be (unusedFields)

    val optims = Seq.empty

    import BATLifting._
    import InstructionLifting._

    val unusedFieldsLos /*: Exp[Traversable[(ClassFile, Traversable[String])]]*/ = benchMark("UUF_UNUSED_FIELD Los Setup") {
      Query(for {
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
            // Note that we might not factor map (_.name) by writing:
            //   ((asGETFIELD orElse asGETSTATIC) map (_.name)).get
            // because Scala's type system is nominal and for the two branches different (_.name) methods (with the same
            // signature) are invoked.
            (instruction.ifInstanceOf[GETFIELD] map (_.name) orElse (instruction.ifInstanceOf[GETSTATIC] map (_.name))).get
              //XXX: should we emulate support for `if` in some way? Yes of course!
            /*if (asGETFIELD.isDefined)
              asGETFIELD.name
            else if (asGETSTATIC.isDefined)
              asGETSTATIC.name*/
        })
        unusedPrivateFields ← Let(privateFields -- usedPrivateFields) //for (field ← privateFields if !usedPrivateFields.contains(field)) yield field
        if unusedPrivateFields.size > 0
      } yield (classFile, privateFields))
    }
    benchQuery("UUF_UNUSED_FIELD Los", unusedFieldsLos, unusedFields, optims)

    val unusedFieldsLos1_1 /*: Exp[Traversable[(ClassFile, Traversable[String])]]*/ = benchMark("UUF_UNUSED_FIELD Los-1.1 Setup") {
      Query(for {
        classFile ← classFiles.asSmartCollection if !classFile.isInterfaceDeclaration
        instructions ← Let(for {
          method ← classFile.methods if method.body.isDefined
          instruction ← method.body.get.code
        } yield instruction)
        declaringClass ← Let(classFile.thisClass)
        privateFields ← Let((for (field ← classFile.fields if field.isPrivate) yield field.name).toSet)
        usedPrivateFields ← Let(instructions filter {
          instruction ⇒
            ((for {
              getFIELD <- instruction.ifInstanceOf[GETFIELD]
            } yield getFIELD.declaringClass === declaringClass) orElse
              (for {
                getSTATIC <- instruction.ifInstanceOf[GETSTATIC]
              } yield getSTATIC.declaringClass === declaringClass)).orElse(Some(false)).get
        } map {
          instruction ⇒
            // Note that we might not factor map (_.name) by writing:
            //   ((asGETFIELD orElse asGETSTATIC) map (_.name)).get
            // because Scala's type system is nominal and for the two branches different (_.name) methods (with the same
            // signature) are invoked.
            (instruction.ifInstanceOf[GETFIELD] map (_.name) orElse (instruction.ifInstanceOf[GETSTATIC] map (_.name))).get
          //XXX: should we emulate support for `if` in some way? Yes of course!
          /*if (asGETFIELD.isDefined)
          asGETFIELD.name
        else if (asGETSTATIC.isDefined)
          asGETSTATIC.name*/
        })
        unusedPrivateFields ← Let(privateFields -- usedPrivateFields) //for (field ← privateFields if !usedPrivateFields.contains(field)) yield field
        if unusedPrivateFields.size > 0
      } yield (classFile, privateFields))
    }
    benchQuery("UUF_UNUSED_FIELD Los-1.1", unusedFieldsLos1_1, unusedFields, optims)

    val unusedFieldsLos1_2 /*: Exp[Traversable[(ClassFile, Traversable[String])]]*/ = benchMark("UUF_UNUSED_FIELD Los-1.2 Setup") {
      Query(for {
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
            if_ (asGETFIELD.isDefined) {
              asGETFIELD.get.declaringClass === declaringClass
            } else_ if_ (asGETSTATIC.isDefined) {
                asGETSTATIC.get.declaringClass === declaringClass
            } else_ {
              false
            }
        } map {
          instruction ⇒
          // Note that we might not factor map (_.name) by writing:
          //   ((asGETFIELD orElse asGETSTATIC) map (_.name)).get
          // because Scala's type system is nominal and for the two branches different (_.name) methods (with the same
          // signature) are invoked.
            (instruction.ifInstanceOf[GETFIELD] map (_.name) orElse (instruction.ifInstanceOf[GETSTATIC] map (_.name))).get
          //XXX: should we emulate support for `if` in some way? Yes of course!
          /*if (asGETFIELD.isDefined)
          asGETFIELD.name
        else if (asGETSTATIC.isDefined)
          asGETSTATIC.name*/
        })
        unusedPrivateFields ← Let(privateFields -- usedPrivateFields) //for (field ← privateFields if !usedPrivateFields.contains(field)) yield field
        if unusedPrivateFields.size > 0
      } yield (classFile, privateFields))
    }
    benchQuery("UUF_UNUSED_FIELD Los-1.2", unusedFieldsLos1_2, unusedFields, optims)

    val unusedFieldsLos1bis /*: Exp[Traversable[(ClassFile, Traversable[String])]]*/ = benchMark("UUF_UNUSED_FIELD Los-1bis Setup") {
      Query(for {
        classFile ← classFiles.asSmartCollection if !classFile.isInterfaceDeclaration
        declaringClass ← Let(classFile.thisClass)
        usedPrivateFields ← Let(for {
          method ← classFile.methods if method.body.isDefined
          instruction ← method.body.get.code
          usedPrivateField ← (for {
            asGETFIELD <- instruction.ifInstanceOf[GETFIELD]
            if asGETFIELD.declaringClass === declaringClass
          } yield asGETFIELD.name) orElse
            (for {
              asGETSTATIC <- instruction.ifInstanceOf[GETSTATIC]
              if asGETSTATIC.declaringClass === declaringClass
            } yield asGETSTATIC.name)
            //(instruction.ifInstanceOf[GETFIELD].filter(_.declaringClass === declaringClass).map(_.name)) orElse (instruction.ifInstanceOf[GETSTATIC].filter(_.declaringClass === declaringClass).map(_.name))
        } yield usedPrivateField)
        privateFields ← Let((for (field ← classFile.fields if field.isPrivate) yield field.name).toSet)
        unusedPrivateFields ← Let(privateFields -- usedPrivateFields) //for (field ← privateFields if !usedPrivateFields.contains(field)) yield field
        if unusedPrivateFields.size > 0
      } yield (classFile, privateFields))
    }
    benchQuery("UUF_UNUSED_FIELD Los-1bis", unusedFieldsLos1bis, unusedFields, optims)

    val unusedFields2Los /*: Exp[Traversable[(ClassFile, Traversable[String])]]*/ = benchMark("UUF_UNUSED_FIELD-2 Los Setup") {
      Query(for {
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
      } yield (classFile, privateFields))
    }
    benchQuery("UUF_UNUSED_FIELD-2 Los", unusedFields2Los, unusedFields, optims)

    val unusedFields3Los /*: Exp[Traversable[(ClassFile, Traversable[String])]]*/ = benchMark("UUF_UNUSED_FIELD-3 Los Setup") {
      Query(for {
        classFile ← classFiles.asSmartCollection if !classFile.isInterfaceDeclaration
        instructions ← Let(for {
          method ← classFile.methods if method.body.isDefined
          instruction ← method.body.get.code
        } yield instruction)
        declaringClass ← Let(classFile.thisClass)
        privateFields ← Let((for (field ← classFile.fields if field.isPrivate) yield field.name).toSet)
        usedPrivateFields ← Let(//This is much slower, but typeFilter is faster. We need a typeFilter node which accepts a function to run.
          (for (instruction ← instructions.typeFilter[GETFIELD] if instruction.declaringClass === declaringClass) yield instruction.name) union
            (for (instruction ← instructions.typeFilter[GETSTATIC] if instruction.declaringClass === declaringClass) yield instruction.name))
        unusedPrivateFields ← Let(privateFields -- usedPrivateFields) //for (field ← privateFields if !usedPrivateFields.contains(field)) yield field
        if unusedPrivateFields.size > 0
      } yield (classFile, privateFields))
    }
    println(unusedFields3Los)
    benchQuery("UUF_UNUSED_FIELD-3 Los", unusedFields3Los, unusedFields, optims)

    /*val unusedFields3LosOpt = Optimization optimize unusedFields3Los
    println(unusedFields3LosOpt)
    val unusedFields3LosOptRes = benchInterpret("UUF_UNUSED_FIELD-3 Opt Los", unusedFields3LosOpt)
    unusedFields3LosOptRes should be (unusedFields)

    val unusedFields3LosOptSzToEm = Optimization sizeToEmpty unusedFields3LosOpt
    val unusedFields3LosOptSzToEmRes = benchInterpret("UUF_UNUSED_FIELD-3 Opt Size To Empty Los", unusedFields3LosOptSzToEm)
    unusedFields3LosOptSzToEmRes should be (unusedFields)*/
  }

  test("ExplicitGC") {
    analyzeExplicitGC()
  }
  def analyzeExplicitGC() {
    // FINDBUGS: Dm: Explicit garbage collection; extremely dubious except in benchmarking code (DM_GC)
    val garbageCollectingMethods: Seq[(ClassFile, Method, Instruction)] = benchMark("DM_GC") {
      for {
        classFile ← classFiles
        method ← classFile.methods if method.body.isDefined
        instruction ← method.body.get.code
        if (instruction match {
          case INVOKESTATIC(ObjectType("java/lang/System"), "gc", MethodDescriptor(Seq(), VoidType)) |
               INVOKEVIRTUAL(ObjectType("java/lang/Runtime"), "gc", MethodDescriptor(Seq(), VoidType)) ⇒ true
          case _ ⇒ false
        })
      } yield (classFile, method, instruction)
    }

    println("\tViolations: " + garbageCollectingMethods.size)

    val garbageCollectingMethodsLosLike = benchMark("DM_GC Native Like Los") {
      for {
        classFile ← classFiles
        method ← classFile.methods if method.body.isDefined
        instruction ← method.body.get.code
        if ({
          val asINVOKESTATIC = instruction.ifInstanceOf[INVOKESTATIC]
          val asINVOKEVIRTUAL = instruction.ifInstanceOf[INVOKEVIRTUAL]
          val desc = MethodDescriptor(Seq(), VoidType)

          asINVOKESTATIC.isDefined && asINVOKESTATIC.get.declaringClass == ObjectType("java/lang/System") && asINVOKESTATIC.get.name == "gc" &&
            asINVOKESTATIC.get.methodDescriptor == desc ||
            asINVOKEVIRTUAL.isDefined && asINVOKEVIRTUAL.get.declaringClass == ObjectType("java/lang/Runtime") && asINVOKEVIRTUAL.get.name == "gc" &&
              asINVOKEVIRTUAL.get.methodDescriptor == desc
        })
      } yield (classFile, method, instruction)
    }
    garbageCollectingMethodsLosLike should be (garbageCollectingMethods)

    val garbageCollectingMethodsLosLike2 = benchMark("DM_GC Native More Like Los") {
      for {
        classFile ← classFiles
        method ← classFile.methods.view if method.body.isDefined
        instruction ← method.body.get.code.view
        if ({
          val asINVOKESTATIC = instruction.ifInstanceOf[INVOKESTATIC]
          val asINVOKEVIRTUAL = instruction.ifInstanceOf[INVOKEVIRTUAL]
          val desc = MethodDescriptor(Seq(), VoidType)

          asINVOKESTATIC.isDefined && asINVOKESTATIC.get.declaringClass == ObjectType("java/lang/System") && asINVOKESTATIC.get.name == "gc" &&
            asINVOKESTATIC.get.methodDescriptor == desc ||
            asINVOKEVIRTUAL.isDefined && asINVOKEVIRTUAL.get.declaringClass == ObjectType("java/lang/Runtime") && asINVOKEVIRTUAL.get.name == "gc" &&
              asINVOKEVIRTUAL.get.methodDescriptor == desc
        })
      } yield (classFile, method, instruction)
    }
    garbageCollectingMethodsLosLike2 should be (garbageCollectingMethods)
    import BATLifting._
    import InstructionLifting._

    val garbageCollectingMethodsLos = benchMark("DM_GC Los Setup") {
      Query(for {
        classFile ← classFiles.asSmartCollection
        method ← classFile.methods if method.body.isDefined
        instruction ← method.body.get.code
        if ({
          val asINVOKESTATIC = instruction.ifInstanceOf[INVOKESTATIC]
          val asINVOKEVIRTUAL = instruction.ifInstanceOf[INVOKEVIRTUAL]
          val desc = MethodDescriptor(Seq(), VoidType)

          asINVOKESTATIC.isDefined && asINVOKESTATIC.get.declaringClass === ObjectType("java/lang/System") && asINVOKESTATIC.get.name == "gc" &&
            asINVOKESTATIC.get.methodDescriptor == desc ||
            asINVOKEVIRTUAL.isDefined && asINVOKEVIRTUAL.get.declaringClass === ObjectType("java/lang/Runtime") && asINVOKEVIRTUAL.get.name == "gc" &&
              asINVOKEVIRTUAL.get.methodDescriptor == desc
        })
      } yield (classFile, method, instruction))
    }
    benchQuery("DM_GC Los", garbageCollectingMethodsLos, garbageCollectingMethods)
  }

  test("PublicFinalizer") {
    analyzePublicFinalizer()
  }
  def analyzePublicFinalizer() {
    // FINDBUGS: FI: Finalizer should be protected, not public (FI_PUBLIC_SHOULD_BE_PROTECTED)
    val classesWithPublicFinalizeMethods = benchMark("FI_PUBLIC_SHOULD_BE_PROTECTED") {
      for (
        classFile ← classFiles
        if classFile.methods.exists(method ⇒ method.name == "finalize" && method.isPublic && method.descriptor.returnType == VoidType && method.descriptor.parameterTypes.size == 0)
      ) yield classFile
    }
    println("\tViolations: " + classesWithPublicFinalizeMethods.length)

    val classesWithPublicFinalizeMethodsLikeLos = benchMark("FI_PUBLIC_SHOULD_BE_PROTECTED Native Like Los") {
      (for (
        classFile ← classFiles.view
        if classFile.methods.exists(method ⇒ method.name == "finalize" && method.isPublic && method.descriptor.returnType == VoidType && method.descriptor.parameterTypes.size == 0)
      ) yield classFile).force
    }

    import BATLifting._

    val classesWithPublicFinalizeMethodsLos = benchMark("FI_PUBLIC_SHOULD_BE_PROTECTED Los Setup") {
      Query(for (
        classFile ← classFiles.asSmartCollection
        if classFile.methods.exists(method ⇒ method.name === "finalize" && method.isPublic && method.descriptor.returnType === VoidType && method.descriptor.parameterTypes.size === 0)
      ) yield classFile)
    }
    benchQuery("FI_PUBLIC_SHOULD_BE_PROTECTED Los", classesWithPublicFinalizeMethodsLos, classesWithPublicFinalizeMethods)
  }

  test("SerializableNoConstructor") {
    analyzeSerializableNoConstructor()
  }
  def analyzeSerializableNoConstructor() {
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

    val classesWithoutDefaultConstructorLikeLos = benchMark("SE_NO_SUITABLE_CONSTRUCTOR Native Like Los") {
      (for {
        superclass ← classHierarchy.superclasses(serializableClasses).view if getClassFile.isDefinedAt(superclass) && // the class file of some supertypes (defined in libraries, which we do not analyze) may not be available
        {
          val superClassFile = getClassFile(superclass)
          !superClassFile.isInterfaceDeclaration &&
            !superClassFile.constructors.exists(_.descriptor.parameterTypes.length == 0)
        }
      } yield superclass).force // there can be at most one method
    }
    classesWithoutDefaultConstructorLikeLos should be (classesWithoutDefaultConstructorLikeLos)

    import BATLifting._
    val classesWithoutDefaultConstructorLos = benchMark("SE_NO_SUITABLE_CONSTRUCTOR Los Setup") {
      Query(for {
        superclass ← classHierarchy.superclasses(serializableClasses).asSmartCollection if getClassFile.asSmartCollection.isDefinedAt(superclass) && // the class file of some supertypes (defined in libraries, which we do not analyze) may not be available
        {
          val superClassFile = (getClassFile.asSmartCollection)(superclass)
          !superClassFile.isInterfaceDeclaration &&
            !superClassFile.constructors.exists(_.descriptor.parameterTypes.length == 0)
        }
      } yield superclass) // there can be at most one method
    }
    benchQuery("SE_NO_SUITABLE_CONSTRUCTOR Los", classesWithoutDefaultConstructorLos, classesWithoutDefaultConstructor)
  }

  test("CatchIllegalMonitorStateException") {
    analyzeCatchIllegalMonitorStateException()
  }
  def analyzeCatchIllegalMonitorStateException() {
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

    val catchesIllegalMonitorStateExceptionLikeLos = benchMark("IMSE_DONT_CATCH_IMSE Native Like Los") {
      (for {
        classFile ← classFiles.view if classFile.isClassDeclaration
        method ← classFile.methods.view if method.body.isDefined
        exceptionHandler ← method.body.get.exceptionTable.view if exceptionHandler.catchType == IllegalMonitorStateExceptionType
      } yield (classFile, method)).force
    }

    import BATLifting._
    val catchesIllegalMonitorStateExceptionLos = benchMark("IMSE_DONT_CATCH_IMSE Los Setup") {
      Query(for {
        classFile ← classFiles.asSmartCollection if classFile.isClassDeclaration
        method ← classFile.methods if method.body.isDefined
        exceptionHandler ← method.body.get.exceptionTable if exceptionHandler.catchType === IllegalMonitorStateExceptionType
      } yield (classFile, method))
    }
    benchQuery("IMSE_DONT_CATCH_IMSE Los", catchesIllegalMonitorStateExceptionLos, catchesIllegalMonitorStateException)
  }

  def setupAnalysis(zipFiles: Seq[String]) {
    classFiles = benchMark("Reading all class files", execLoops = 1, warmUpLoops = 0, sampleLoops = 1) {
      for (zipFile ← zipFiles; classFile ← Java6Framework.ClassFiles(zipFile)) yield classFile
    }
    // This operation is not incrementalizable by itself. If classHierarchy supports removing classes, we might
    // provide a way to setup a listener easily.
    for (classFile ← classFiles)
      classHierarchy.update(classFile)
    //As an alternative, classHierarchy might support IVM directly.
    //classHierarchy.update(classFiles)

    getClassFile = classFiles.map(cf ⇒ (cf.thisClass, cf)).toMap
    println("Number of class files: " + classFiles.length)
  }

  def analyze(zipFiles: Seq[String]) {
    setupAnalysis(zipFiles)

    analyzeConfusedInheritance()
    analyzeUnusedFields()
    analyzeExplicitGC()
    analyzePublicFinalizer()
    analyzeSerializableNoConstructor()
    analyzeCatchIllegalMonitorStateException()
  }

  override def beforeAll() {
    setupAnalysis(Seq("lib/scalatest-1.6.1.jar"))
  }
}
