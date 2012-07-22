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
import expressiontree._ //We import this just for being able to resolve identifiers in code dumps.
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
  type QueryAnd[+T] = ((ClassFile, Method, Code), T)
}

class FindBugsAnalyses(zipFiles: Seq[String]) extends FunSuite with BeforeAndAfterAll with ShouldMatchers with QueryBenchmarking {
  import FindBugsAnalyses.QueryAnd

  def methodsNative() = {
    for {
      classFile ← classFiles
      method ← classFile.methods
    } yield (classFile, method)
  }

  def methodsSQuOpt() = {
    import BATLifting._
    for {
      classFile ← classFiles.asSmart
      method ← classFile.methods
    } yield (classFile, method)
  }

  def methodBodiesSQuOpt() = {
    import BATLifting._
    //import dbschema._ //{squopt => _, _}
    import dbschema.squopt._
    for {
      classFile ← classFiles.asSmart
      method ← classFile.methods
      body ← method.body
    } yield MethodRecord(classFile, method, body)
  }

  def methodBodiesModularNative() = {
    import dbschema._
    for {
      (classFile, method) <- methodsNative()
      body <- method.body
    } yield MethodRecord(classFile, method, body) //MethodRecord(cfM._1, cfM._2, body)
  }

  def methodBodiesModularSQuOpt() = {
    import BATLifting._
    import dbschema.squopt._
    for {
      cfM <- methodsSQuOpt()
      classFile <- Let(cfM._1)
      method ← Let(cfM._2)
      body <- method.body
    } yield MethodRecord(classFile, method, body) //MethodRecord(cfM._1, cfM._2, body)
  }

  //def this() = this(Seq("src/test/resources/scalatest-1.6.1.jar"))
  def this() = this(Seq("src/test/resources/Bugs.zip"))

  //This is to have a run comparable with FindBugs
  //override def onlyOptimized = true
  //Standard execution
  override def onlyOptimized = false

  val classFiles: Seq[ClassFile] = benchMark("Reading all class files", execLoops = 1, warmUpLoops = 0, sampleLoops = 1) {
    for (zipFile ← zipFiles; classFile ← Java6Framework.ClassFiles(zipFile)) yield classFile
  }
  val getClassFile: Map[ObjectType, ClassFile] = classFiles.map(cf ⇒ (cf.thisClass, cf)).toMap

  // Now the classHierarchy is built functionally - hence, the result could be incrementally maintained (at least for the
  // addition of classes).
  val classHierarchy = (new ClassHierarchy /: classFiles)(_ + _)

  println("Number of class files: " + classFiles.length)
  println("Numer of methods: " + methodsSQuOpt().interpret().size)

  test("Compilation") {
    benchMark("Compiling methodBodiesSQuOpt"){Compile.toValue(methodBodiesSQuOpt())} should be (methodBodiesSQuOpt().interpret())
  }

  // The following code is meant to show how easy it is to write analyses;
  // it is not meant to demonstrate how to write such analyses in an efficient
  // manner.
  test("ProtectedField") {
    analyzeConfusedInheritance()
  }
  def analyzeConfusedInheritance() {
    // FINDBUGS: CI: Class is final but declares protected field (CI_CONFUSED_INHERITANCE) // http://code.google.com/p/findbugs/source/browse/branches/2.0_gui_rework/findbugs/src/java/edu/umd/cs/findbugs/detect/ConfusedInheritance.java
    benchQueryComplete("PROTECTED_FIELD"){ //FB:"CI_CONFUSED_INHERITANCE") {
      for {
        classFile ← classFiles if classFile.isFinal
        field ← classFile.fields if field.isProtected
      } yield (classFile, field)
    } {
      import BATLifting._
      for {
        classFile ← classFiles.asSmart if classFile.isFinal
        field ← classFile.fields if field.isProtected
      } yield (classFile, field)
    }
  }

  /*
  //actual:
  FlatMap(
    Filter(
      ConstByIdentity(List(ClassFile(0,50,33,ObjectType(className="bugs/SuperA"),Some(ObjectType(className="java/lang/Obje..."))))),
      v110 => Not(ClassFile_isInterfaceDeclaration14(v110))),
    v111 => App(
      v544 => MapNode(
        Filter(
          ExpSeq(List(Diff(v544,
            App(
              v543 =>
                TypeCaseExp(v543,
                  ArrayBuffer(
                    TypeCase(class de.tud.cs.st.bat.resolved.GETFIELD,
                      v462 => Eq(Call1('declaringClass, v462),ClassFile_thisClass3(v111)),v463 => Call1('name, v463)),
                    TypeCase(class de.tud.cs.st.bat.resolved.GETSTATIC,
                      v464 => Eq(Call1('declaringClass, v464),ClassFile_thisClass3(v111)),v465 => Call1('name, v465)))),
              FlatMap(ClassFile_methods7(v111),v461 => FlatMap(Call1('Option_option2Iterable, Method_body12(v461)),v460 => Code_instructions2(v460))))
            ))),
          v467 => Not(IsEmpty(v467))),
        v542 => LiftTuple2(v111,v544)),
      Call1('TraversableLike$toSet, MapNode(Filter(ClassFile_fields6(v111),v447 => ClassMember_isPrivate2(v447)),v541 => Field_name1(v541)))))
  //expected:
  FlatMap(
    Filter(
      ConstByIdentity(List(ClassFile(0,50,33,ObjectType(className="bugs/SuperA"),Some(ObjectType(className="java/lang/Obje..."))))),
      v128 => Not(ClassFile_isInterfaceDeclaration14(v128))),
    v129 => App(
      v344 => MapNode(
        Filter(
          ExpSeq(List(Diff(v344,
            TypeCaseExp(
              FlatMap(ClassFile_methods7(v129),v266 => FlatMap(Call1('Option_option2Iterable, Method_body12(v266)),v265 => Code_instructions2(v265))),
              ArrayBuffer(
                TypeCase(class de.tud.cs.st.bat.resolved.GETFIELD,
                  v267 => Eq(Call1('declaringClass, v267),ClassFile_thisClass3(v129)),v268 => Call1('name, v268)),
                TypeCase(class de.tud.cs.st.bat.resolved.GETSTATIC,
                  v269 => Eq(Call1('declaringClass, v269),ClassFile_thisClass3(v129)),v270 => Call1('name, v270))))
            ))),
        v271 => Not(IsEmpty(v271))),
      v343 => LiftTuple2(v129,v344)),
    Call1('TraversableLike$toSet, MapNode(Filter(ClassFile_fields6(v129),v263 => ClassMember_isPrivate2(v263)),v342 => Field_name1(v342)))))
  */

  /*2012-07-06 18:12
  FlatMap(Filter(
    ConstByIdentity(List(ClassFile(0,50,33,ObjectType(className="bugs/SuperA"),Some(ObjectType(className="java/lang/Obje..."))))),
    v3784 => Not(ClassFile_isInterfaceDeclaration14(v3784))),
    v3728 => MapNode(Filter(
      ExpSeq(List(Call1('TraversableLike$toSet, MapNode(Filter(ClassFile_fields6(v3728),v3786 => ClassMember_isPrivate2(v3786)),v4084 => Field_name1(v4084))))),
      v3790 => Not(IsEmpty(
        Filter(
          ExpSeq(List(Diff(v3790,
          TypeCaseExp(FlatMap(ClassFile_methods7(v3728),
            v3765 => FlatMap(Call1('Option_option2Iterable, Method_body12(v3765)),v3764 => Code_instructions2(v3764))),
            ArrayBuffer(
              TypeCase(class de.tud.cs.st.bat.resolved.GETFIELD,v3766 => Eq(Call1('declaringClass, v3766),ClassFile_thisClass3(v3728)),v3767 => Call1('name, v3767)),
              TypeCase(class de.tud.cs.st.bat.resolved.GETSTATIC,v3768 => Eq(Call1('declaringClass, v3768),ClassFile_thisClass3(v3728)),v3769 => Call1('name, v3769))))))),
          v3788 => Not(IsEmpty(v3788)))))),
  v4085 => LiftTuple2(v3728,v4085)))*/
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
    def getPrivateFields(classFile: ClassFile) = (for (field ← classFile.fields if field.isPrivate) yield field.name).toSet //XXX toSet is unneeded, field names are unique.
    def usedPrivateFields(classFile: ClassFile, declaringClass: ObjectType) = for {
        method ← classFile.methods
        body ← method.body.toList
        instruction ← body.instructions
        usedPrivateField ← instruction match {
          case GETFIELD(`declaringClass`, name, _) ⇒ Some(name)
          case GETSTATIC(`declaringClass`, name, _) ⇒ Some(name)
          case _ ⇒ None
        }
      } yield usedPrivateField //for (field ← privateFields if !usedPrivateFields.contains(field)) yield field
    def getPrivateFieldsLos(classFile: Exp[ClassFile]) = {
      import BATLifting._
      (for (field ← classFile.fields if field.isPrivate) yield field.name).toSet  //XXX toSet is unneeded, field names are unique.
    }
    def usedPrivateFieldsLos(classFile: Exp[ClassFile], declaringClass: Exp[ObjectType]) = {
      import BATLifting._
      import InstructionLifting._
      for {
        instructions ← Let(for { //This requires unnesting on a Let... will the optimizer manage? I suspect not, simply because the Let is there.
          //However, it should inline the Let early because the bound variable is used only once.
          method ← classFile.methods
          body ← method.body
          instruction ← body.instructions
        } yield instruction)
        name <- instructions.typeCase(
          when[GETFIELD](asGETFIELD => asGETFIELD.declaringClass ==# declaringClass, _.name),
          when[GETSTATIC](asGETSTATIC => asGETSTATIC.declaringClass ==# declaringClass, _.name))
      } yield name
      /*(for {
        method ← classFile.methods
        body ← method.body.toList
        instruction ← body.instructions
        usedPrivateField ← instruction match {
          case GETFIELD(`declaringClass`, name, _) ⇒ Some(name)
          case GETSTATIC(`declaringClass`, name, _) ⇒ Some(name)
          case _ ⇒ None
        }
      } yield usedPrivateField)*/
    } //for (field ← privateFields if !usedPrivateFields.contains(field)) yield field


    benchQueryComplete("UNUSED_PRIVATE_FIELD")({ // FB: "UUF_UNUSED_FIELD") { // However, we only focus on private fields.
      for {
        classFile ← classFiles if !classFile.isInterfaceDeclaration
        declaringClass = classFile.thisClass
        privateFields = (for (field ← classFile.fields if field.isPrivate) yield field.name).toSet //XXX toSet is unneeded, field names are unique.
      //Note that this could even allow unnesting - should we try to have this unnested?
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
    }, {
      for {
        classFile ← classFiles if !classFile.isInterfaceDeclaration
        declaringClass = classFile.thisClass
        privateFields = getPrivateFields(classFile)
        //Note that this could even allow unnesting - should we try to have this unnested?
        unusedPrivateFields = privateFields -- usedPrivateFields(classFile, declaringClass)
        if unusedPrivateFields.size > 0
      } yield (classFile, privateFields)
    })({
      import BATLifting._
      import InstructionLifting._
      for {
        classFile ← classFiles.asSmart if !classFile.isInterfaceDeclaration
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
      } yield (classFile, privateFields)
    }, Optimization removeIdentityMaps {
      import BATLifting._
      for {
        classFile ← classFiles.asSmart if !classFile.isInterfaceDeclaration
        declaringClass ← Let(classFile.thisClass)
        privateFields ← Let(getPrivateFieldsLos(classFile))
        usedPrivateFields ← Let(usedPrivateFieldsLos(classFile, declaringClass))
        unusedPrivateFields ← Let(privateFields -- usedPrivateFields) //for (field ← privateFields if !usedPrivateFields.contains(field)) yield field
        if unusedPrivateFields.size > 0
      } yield (classFile, privateFields)
    })
  }

  test("ExplicitGC") {
    analyzeExplicitGC()
  }
  def analyzeExplicitGC() {
    val NoArgNoRetMethodDesc = MethodDescriptor(Seq(), VoidType)

    // FINDBUGS: Dm: Explicit garbage collection; extremely dubious except in benchmarking code (DM_GC)
    benchQueryComplete("GC_CALL")({ // FB: "DM_GC") {
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
    }, {
      (for {
        methodAndBody <- methodBodiesModularNative()
        instruction ← methodAndBody.body.instructions
        if (instruction match {
          case INVOKESTATIC(ObjectType("java/lang/System"), "gc", NoArgNoRetMethodDesc) |
               INVOKEVIRTUAL(ObjectType("java/lang/Runtime"), "gc", NoArgNoRetMethodDesc) ⇒ true
          case _ ⇒ false
        })
      } yield (methodAndBody.classFile, methodAndBody.method, instruction)).toSet
    }) ({
      import BATLifting._
      import InstructionLifting._
      (for {
        classFile ← classFiles.asSmart
        method ← classFile.methods
        body ← method.body
        instruction ← body.instructions.typeCase(
          when[INVOKESTATIC](instr =>
            instr.declaringClass ==# ObjectType("java/lang/System") && instr.name ==# "gc" && instr.methodDescriptor ==# NoArgNoRetMethodDesc, identity),
          when[INVOKEVIRTUAL](instr =>
            instr.declaringClass ==# ObjectType("java/lang/Runtime") && instr.name ==# "gc" && instr.methodDescriptor ==# NoArgNoRetMethodDesc, identity))
      } yield (classFile, method, instruction)).toSet
    }, {
      import BATLifting._
      import InstructionLifting._
      import dbschema.squopt._
      (for {
        methodAndBody <- methodBodiesSQuOpt()
        instruction <- methodAndBody.body.instructions.typeCase(
          when[INVOKESTATIC](instr =>
            instr.declaringClass ==# ObjectType("java/lang/System") && instr.name ==# "gc" && instr.methodDescriptor ==# NoArgNoRetMethodDesc, identity),
          when[INVOKEVIRTUAL](instr =>
            instr.declaringClass ==# ObjectType("java/lang/Runtime") && instr.name ==# "gc" && instr.methodDescriptor ==# NoArgNoRetMethodDesc, identity))
      } yield (methodAndBody.classFile, methodAndBody.method, instruction)).toSet
    }, {
      import BATLifting._
      import InstructionLifting._
      import dbschema.squopt._
      (for {
        methodAndBody <- methodBodiesModularSQuOpt()
        instruction <- methodAndBody.body.instructions.typeCase(
          when[INVOKESTATIC](instr =>
            instr.declaringClass ==# ObjectType("java/lang/System") && instr.name ==# "gc" && instr.methodDescriptor ==# NoArgNoRetMethodDesc, identity),
          when[INVOKEVIRTUAL](instr =>
            instr.declaringClass ==# ObjectType("java/lang/Runtime") && instr.name ==# "gc" && instr.methodDescriptor ==# NoArgNoRetMethodDesc, identity))
      } yield (methodAndBody.classFile, methodAndBody.method, instruction)).toSet
    })
  }

/*
  //Unoptimized:
Call1('TraversableLike$toSet,
  FlatMap(ConstByIdentity(List(ClassFile(0,50,33,ObjectType(className="bugs/SuperA"),Some(ObjectType(className="java/lang/Obje..."))))),
    v279 => FlatMap(ClassFile_methods7(v279),
      v280 => FlatMap(Call1('Option_option2Iterable, Method_body12(v280)),
      v281 => MapNode(
        TypeCaseExp(Code_instructions2(v281),WrappedArray(TypeCase(class de.tud.cs.st.bat.resolved.INVOKESTATIC,v282 => And(And(Eq(Call1('declaringClass, v282),Const(ObjectType(className="java/lang/System"))),Eq(Call1('name, v282),Const("gc"))),Eq(Call1('methodDescriptor, v282),Const(MethodDescriptor(List(),VoidType)))),v283 => v283), TypeCase(class de.tud.cs.st.bat.resolved.INVOKEVIRTUAL,v284 => And(And(Eq(Call1('declaringClass, v284),Const(ObjectType(className="java/lang/Runtime"))),Eq(Call1('name, v284),Const("gc"))),Eq(Call1('methodDescriptor, v284),Const(MethodDescriptor(List(),VoidType)))),v285 => v285))),
  v286 => LiftTuple3(v279,v280,v286))))))
  //Modularized:
Call1('TraversableLike$toSet,
  FlatMap(
    FlatMap(ConstByIdentity(List(ClassFile(0,50,33,ObjectType(className="bugs/SuperA"),Some(ObjectType(className="java/lang/Obje..."))))),
      v449 => FlatMap(ClassFile_methods7(v449),v450 => MapNode(Call1('Option_option2Iterable, Method_body12(v450)),
        v451 => MethodRecordExp0(v449,v450,v451)))),
    v452 => MapNode(
      TypeCaseExp(Code_instructions2(MethodRecord_body2(v452)),WrappedArray(TypeCase(class de.tud.cs.st.bat.resolved.INVOKESTATIC,v453 => And(And(Eq(Call1('declaringClass, v453),Const(ObjectType(className="java/lang/System"))),Eq(Call1('name, v453),Const("gc"))),Eq(Call1('methodDescriptor, v453),Const(MethodDescriptor(List(),VoidType)))),v454 => v454), TypeCase(class de.tud.cs.st.bat.resolved.INVOKEVIRTUAL,v455 => And(And(Eq(Call1('declaringClass, v455),Const(ObjectType(className="java/lang/Runtime"))),Eq(Call1('name, v455),Const("gc"))),Eq(Call1('methodDescriptor, v455),Const(MethodDescriptor(List(),VoidType)))),v456 => v456))),
  v457 => LiftTuple3(MethodRecord_classFile0(v452),MethodRecord_method1(v452),v457))))
  //The modularized version which uses methodBodiesModularSQuOpt is even worse.
  //Modularized & optimized
Call1('TraversableLike$toSet, FlatMap(ConstByIdentity(List(ClassFile(0,50,33,ObjectType(className="bugs/SuperA"),Some(ObjectType(className="java/lang/Obje..."))))),v449 => FlatMap(ClassFile_methods7(v449),v448 => FlatMap(Call1('Option_option2Iterable, Method_body12(v448)),v447 => App(v501 => MapNode(TypeCaseExp(Code_instructions2(MethodRecord_body2(v501)),ArrayBuffer(TypeCase(class de.tud.cs.st.bat.resolved.INVOKESTATIC,v437 => And(And(Eq(Call1('declaringClass, v437),Const(ObjectType(className="java/lang/System"))),Eq(Call1('name, v437),Const("gc"))),Eq(Call1('methodDescriptor, v437),Const(MethodDescriptor(List(),VoidType)))),v438 => v438), TypeCase(class de.tud.cs.st.bat.resolved.INVOKEVIRTUAL,v439 => And(And(Eq(Call1('declaringClass, v439),Const(ObjectType(className="java/lang/Runtime"))),Eq(Call1('name, v439),Const("gc"))),Eq(Call1('methodDescriptor, v439),Const(MethodDescriptor(List(),VoidType)))),v440 => v440))),v500 => LiftTuple3(MethodRecord_classFile0(v501),MethodRecord_method1(v501),v500)),MethodRecordExp0(v449,v448,v447))))))
Call1('TraversableLike$toSet, FlatMap(ConstByIdentity(List(ClassFile(0,50,33,ObjectType(className="bugs/SuperA"),Some(ObjectType(className="java/lang/Obje..."))))),v449 => FlatMap(ClassFile_methods7(v449),v448 => FlatMap(Call1('Option_option2Iterable, Method_body12(v448)),v447 => App(v501 => MapNode(TypeCaseExp(Code_instructions2(MethodRecord_body2(v501)),ArrayBuffer(TypeCase(class de.tud.cs.st.bat.resolved.INVOKESTATIC,v437 => And(And(Eq(Call1('declaringClass, v437),Const(ObjectType(className="java/lang/System"))),Eq(Call1('name, v437),Const("gc"))),Eq(Call1('methodDescriptor, v437),Const(MethodDescriptor(List(),VoidType)))),v438 => v438), TypeCase(class de.tud.cs.st.bat.resolved.INVOKEVIRTUAL,v439 => And(And(Eq(Call1('declaringClass, v439),Const(ObjectType(className="java/lang/Runtime"))),Eq(Call1('name, v439),Const("gc"))),Eq(Call1('methodDescriptor, v439),Const(MethodDescriptor(List(),VoidType)))),v440 => v440))),v500 => LiftTuple3(MethodRecord_classFile0(v501),MethodRecord_method1(v501),v500)),MethodRecordExp0(v449,v448,v447))))))
Call1('TraversableLike$toSet, FlatMap(ConstByIdentity(List(ClassFile(0,50,33,ObjectType(className="bugs/SuperA"),Some(ObjectType(className="java/lang/Obje..."))))),v465 => FlatMap(ClassFile_methods7(v465),v464 => FlatMap(Call1('Option_option2Iterable, Method_body12(v464)),v463 => App(v517 => MapNode(TypeCaseExp(Code_instructions2(MethodRecord_body2(v517)),ArrayBuffer(TypeCase(class de.tud.cs.st.bat.resolved.INVOKESTATIC,v453 => And(And(Eq(Call1('declaringClass, v453),Const(ObjectType(className="java/lang/System"))),Eq(Call1('name, v453),Const("gc"))),Eq(Call1('methodDescriptor, v453),Const(MethodDescriptor(List(),VoidType)))),v454 => v454), TypeCase(class de.tud.cs.st.bat.resolved.INVOKEVIRTUAL,v455 => And(And(Eq(Call1('declaringClass, v455),Const(ObjectType(className="java/lang/Runtime"))),Eq(Call1('name, v455),Const("gc"))),Eq(Call1('methodDescriptor, v455),Const(MethodDescriptor(List(),VoidType)))),v456 => v456))),v516 => LiftTuple3(MethodRecord_classFile0(v517),MethodRecord_method1(v517),v516)),MethodRecordExp0(v465,v464,v463))))))
Call1('TraversableLike$toSet,
  FlatMap(ConstByIdentity(List(ClassFile(0,50,33,ObjectType(className="bugs/SuperA"),Some(ObjectType(className="java/lang/Obje..."))))),
    v449 => FlatMap(ClassFile_methods7(v449),
      v448 => FlatMap(Call1('Option_option2Iterable, Method_body12(v448)),
        v447 => App(
          v501 => MapNode(
            TypeCaseExp(Code_instructions2(MethodRecord_body2(v501)),ArrayBuffer(TypeCase(class de.tud.cs.st.bat.resolved.INVOKESTATIC,v437 => And(And(Eq(Call1('declaringClass, v437),Const(ObjectType(className="java/lang/System"))),Eq(Call1('name, v437),Const("gc"))),Eq(Call1('methodDescriptor, v437),Const(MethodDescriptor(List(),VoidType)))),v438 => v438), TypeCase(class de.tud.cs.st.bat.resolved.INVOKEVIRTUAL,v439 => And(And(Eq(Call1('declaringClass, v439),Const(ObjectType(className="java/lang/Runtime"))),Eq(Call1('name, v439),Const("gc"))),Eq(Call1('methodDescriptor, v439),Const(MethodDescriptor(List(),VoidType)))),v440 => v440))),
            v500 => LiftTuple3(MethodRecord_classFile0(v501),MethodRecord_method1(v501),v500)),
          MethodRecordExp0(v449,v448,v447))
  ))))
  //Modular & optimized with fixed optimizer.
Call1('TraversableLike$toSet,
  Union(
    MapNode(
      Filter(
        TypeMappingApp(Const("ivm.collections.TypeMapping@28308c62"),"class de.tud.cs.st.bat.resolved.INVOKESTATIC"),
        v510 => And(And(Eq(Call1('declaringClass, Tuple2Proj2(v510)),Const(ObjectType(className="java/lang/System"))),Eq(Call1('name, Tuple2Proj2(v510)),Const("gc"))),Eq(Call1('methodDescriptor, Tuple2Proj2(v510)),Const(MethodDescriptor(List(),VoidType))))),
      v553 => LiftTuple3(Tuple3Proj1(Tuple2Proj1(v553)),Tuple3Proj2(Tuple2Proj1(v553)),Tuple2Proj2(v553))),
    MapNode(
      Filter(TypeMappingApp(Const("ivm.collections.TypeMapping@28308c62"),"class de.tud.cs.st.bat.resolved.INVOKEVIRTUAL"),
        v520 => And(And(Eq(Call1('declaringClass, Tuple2Proj2(v520)),Const(ObjectType(className="java/lang/Runtime"))),Eq(Call1('name, Tuple2Proj2(v520)),Const("gc"))),Eq(Call1('methodDescriptor, Tuple2Proj2(v520)),Const(MethodDescriptor(List(),VoidType))))),
      v554 => LiftTuple3(Tuple3Proj1(Tuple2Proj1(v554)),Tuple3Proj2(Tuple2Proj1(v554)),Tuple2Proj2(v554)))))
  //Nonmodular & optimized with fixed optimizer (probably also with the older version).
Call1('TraversableLike$toSet,
  Union(
    MapNode(
      Filter(
        TypeMappingApp(Const("ivm.collections.TypeMapping@28308c62"),"class de.tud.cs.st.bat.resolved.INVOKESTATIC"),
        v408 => And(And(Eq(Call1('declaringClass, Tuple2Proj2(v408)),Const(ObjectType(className="java/lang/System"))),Eq(Call1('name, Tuple2Proj2(v408)),Const("gc"))),Eq(Call1('methodDescriptor, Tuple2Proj2(v408)),Const(MethodDescriptor(List(),VoidType))))),
      v451 => LiftTuple3(Tuple3Proj1(Tuple2Proj1(v451)),Tuple3Proj2(Tuple2Proj1(v451)),Tuple2Proj2(v451))),
    MapNode(
      Filter(TypeMappingApp(Const("ivm.collections.TypeMapping@28308c62"),"class de.tud.cs.st.bat.resolved.INVOKEVIRTUAL"),
        v418 => And(And(Eq(Call1('declaringClass, Tuple2Proj2(v418)),Const(ObjectType(className="java/lang/Runtime"))),Eq(Call1('name, Tuple2Proj2(v418)),Const("gc"))),Eq(Call1('methodDescriptor, Tuple2Proj2(v418)),Const(MethodDescriptor(List(),VoidType))))),
      v452 => LiftTuple3(Tuple3Proj1(Tuple2Proj1(v452)),Tuple3Proj2(Tuple2Proj1(v452)),Tuple2Proj2(v452)))))
*/

  //XXX: for the native version, FI_PUBLIC_SHOULD_BE_PROTECTED is faster; for the LoS version, FI_PUBLIC_SHOULD_BE_PROTECTED-2 is faster.
  //However, the first one should get as fast if I enable the unnesting of Exists.
  test("PublicFinalizer") {
    analyzePublicFinalizer()
  }
  def analyzePublicFinalizer() {
    // FINDBUGS: FI: Finalizer should be protected, not public (FI_PUBLIC_SHOULD_BE_PROTECTED)
    benchQueryComplete("FINALIZER_NOT_PROTECTED"){ // FB: "FI_PUBLIC_SHOULD_BE_PROTECTED") {
      for (
        classFile ← classFiles
        if classFile.methods.exists(method ⇒ method.name == "finalize" && method.isPublic && method.descriptor.returnType == VoidType && method.descriptor.parameterTypes.size == 0)
      ) yield classFile
    } {
      import BATLifting._
      for (
        classFile ← classFiles.asSmart
        if classFile.methods.exists(method ⇒ method.name ==# "finalize" && method.isPublic && method.descriptor.returnType ==# VoidType && method.descriptor.parameterTypes.size ==# 0)
      ) yield classFile
    }
  }


  test("PublicFinalizer2") {
    analyzePublicFinalizer2()
  }
  def analyzePublicFinalizer2() {
    // FINDBUGS: FI: Finalizer should be protected, not public (FI_PUBLIC_SHOULD_BE_PROTECTED)
    benchQueryComplete("FINALIZER_NOT_PROTECTED-2") {
      for {
        classFile ← classFiles
        method ← classFile.methods
        if method.name == "finalize" && method.isPublic && method.descriptor.returnType == VoidType && method.descriptor.parameterTypes.size == 0
      } yield classFile
    } {
      import BATLifting._
      for {
        classFile ← classFiles.asSmart
        method ← classFile.methods
        if method.name ==# "finalize" && method.isPublic && method.descriptor.returnType ==# VoidType && method.descriptor.parameterTypes.size ==# 0
      } yield classFile
    }
  }

  test("SerializableNoConstructor") {
    analyzeSerializableNoConstructor()
  }
  def analyzeSerializableNoConstructor() {
    // FINDBUGS: Se: Class is Serializable but its superclass doesn't define a void constructor (SE_NO_SUITABLE_CONSTRUCTOR)
    val serializableClasses = classHierarchy.subclasses(ObjectType("java/io/Serializable")).getOrElse(Set.empty)
    benchQueryComplete("NO_SUITABLE_CONSTRUCTOR") { // FB: SE_NO_SUITABLE_CONSTRUCTOR
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
      for {
        superclass ← classHierarchy.superclasses(serializableClasses).asSmart if getClassFile.asSmart.isDefinedAt(superclass) && // the class file of some supertypes (defined in libraries, which we do not analyze) may not be available
        {
          val superClassFile = getClassFile.asSmart.apply(superclass)
          !superClassFile.isInterfaceDeclaration &&
            !superClassFile.constructors.exists(_.descriptor.parameterTypes.length ==# 0)
        }
      } yield superclass // there can be at most one method
    }
  }

  /*
  //2012-07-06 18:12 - optim results, idempotence problem.
  //optim:
  FlatMap(Filter(ConstByIdentity(List(ClassFile(0,50,33,ObjectType(className="bugs/SuperA"),Some(ObjectType(className="java/lang/Obje..."))))),
    v9334 => ClassFile_isClassDeclaration12(v9334)),
    v9316 => MapNode(Filter(ClassFile_methods7(v9316),

      v9340 => Not(IsEmpty(Filter(Call1('Option_option2Iterable, Method_body12(v9340)),
        v9338 =>

          Not(IsEmpty(Filter(Code_exceptionHandlers3(v9338),
          v9336 => Eq(ExceptionHandler_catchType3(v9336),
            Const(ObjectType(className="java/lang/IllegalMonitorStateException")))))))))),
      v9521 => LiftTuple2(v9316,v9521)))
  //reoptim:
  FlatMap(Filter(ConstByIdentity(List(ClassFile(0,50,33,ObjectType(className="bugs/SuperA"),Some(ObjectType(className="java/lang/Obje..."))))),
    v9539 => ClassFile_isClassDeclaration12(v9539)),
    v9316 => MapNode(Filter(ClassFile_methods7(v9316),

      v9543 => Not(IsEmpty(FlatMap(Call1('Option_option2Iterable, Method_body12(v9543)),
        v9536 => App(v9537 => IfThenElse(v9537,ExpSeq(List(v9537)),ExpSeq(List())),

          Not(IsEmpty(Filter(Code_exceptionHandlers3(v9536),
            v9541 => Eq(ExceptionHandler_catchType3(v9541),
              Const(ObjectType(className="java/lang/IllegalMonitorStateException"))))))))))),
      v9554 => LiftTuple2(v9316,v9554)))
      */

  test("CatchIllegalMonitorStateException") {
    analyzeCatchIllegalMonitorStateException()
  }
  def analyzeCatchIllegalMonitorStateException() {
    // FINDBUGS: (IMSE_DONT_CATCH_IMSE) http://code.google.com/p/findbugs/source/browse/branches/2.0_gui_rework/findbugs/src/java/edu/umd/cs/findbugs/detect/DontCatchIllegalMonitorStateException.java
    benchQueryComplete("DONT_CATCH_IMSE") { // FB: IMSE_DONT_CATCH_IMSE
      for {
        classFile ← classFiles if classFile.isClassDeclaration
        method ← classFile.methods
        body ← method.body.toList
        exceptionHandler ← body.exceptionHandlers
        catchType ← exceptionHandler.catchType.toList
        if catchType == ObjectType("java/lang/IllegalMonitorStateException")
      } yield (classFile, method)
    } {
      import BATLifting._
      for {
        classFile ← classFiles.asSmart if classFile.isClassDeclaration
        method ← classFile.methods
        body ← method.body
        exceptionHandler ← body.exceptionHandlers
        catchType ← exceptionHandler.catchType
        if catchType ==# ObjectType("java/lang/IllegalMonitorStateException")
      } yield (classFile, method)
    }
  }

  test("CovariantCompareToMethods") {
    analyzeCovariantCompareToMethods()
  }
  def analyzeCovariantCompareToMethods() {
    val comparableType = ObjectType("java/lang/Comparable")
    benchQueryComplete("COVARIANT_COMPARETO"){// FB: "CO_SELF_NO_OBJECT/CO_ABSTRACT_SELF") {
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
      for {
        allComparables ← classHierarchy.subtypes(comparableType).toList.asSmart
        comparable ← allComparables
        classFile ← getClassFile.get(comparable) //getClassFile is lifted through Const and makes optimization expensive.
        method ← classFile.methods //if parameterType != ObjectType.Object
        if method.name ==# "compareTo" && method.descriptor.returnType ==# IntegerType
        parameterTypes <- Let(method.descriptor.parameterTypes)
        if parameterTypes.length ==# 1 && parameterTypes(0) !=# ObjectType.Object

      } yield (classFile, method)
    }
  }

  test("AbstractClassesThatDefinesCovariantEquals") {
    analyzeAbstractClassesThatDefinesCovariantEquals()
  }
  def analyzeAbstractClassesThatDefinesCovariantEquals() {
    //XXX this was changed in BAT.
    benchQueryComplete("COVARIANT_EQUALS") { // FB: EQ_ABSTRACT_SELF") {
      for {
        classFile ← classFiles
        method @ Method(_, "equals", MethodDescriptor(CSeq(parameterType), BooleanType), _) ← classFile.methods
        if method.isAbstract && parameterType == classFile.thisClass //!= ObjectType.Object
      } yield (classFile, method)
    } {
      import BATLifting._
      for {
        classFile ← classFiles.asSmart
        method ← classFile.methods
        if method.isAbstract && method.name ==# "equals" && method.descriptor.returnType ==# BooleanType
        parameterTypes <- Let(method.descriptor.parameterTypes)
        if parameterTypes.length ==# 1 && parameterTypes(0) ==# classFile.thisClass //parameterTypes(0) !=# ObjectType.Object
      } yield (classFile, method)
    }
  }

  test("MethodsThatCallRunFinalizersOnExit") {
    analyzeMethodsThatCallRunFinalizersOnExit()
  }
  def analyzeMethodsThatCallRunFinalizersOnExit() {
    benchQueryComplete("RUN_FINALIZERS_ON_EXIT") { // FB: DM_RUN_FINALIZERS_ON_EXIT
      for {
        classFile ← classFiles
        method ← classFile.methods
        body ← method.body.toList
        //the method descriptor is not checked in FindBugs - there's no need
        instruction @ INVOKESTATIC(ObjectType(recvClassName), "runFinalizersOnExit", _/*MethodDescriptor(CSeq(BooleanType), VoidType)*/) ← body.instructions
        if recvClassName == "java/lang/System" || recvClassName == "java/lang/Runtime"
      } yield (classFile, method, instruction)
    } {
      import BATLifting._
      import InstructionLifting._

      for {
        classFile ← classFiles.asSmart
        method ← classFile.methods
        body ← method.body
        instruction ← body.instructions.typeCase(when[INVOKESTATIC](
          instruction => instruction.name ==# "runFinalizersOnExit" && (instruction.declaringClass ==# ObjectType("java/lang/System") || instruction.declaringClass ==# ObjectType("java/lang/Runtime")), identity))
//        if instruction.declaringClass ==# ObjectType("java/lang/System") || instruction.declaringClass ==# ObjectType("java/lang/Runtime")
//        desc <- Let(instruction.methodDescriptor)
//        recv <- instruction.declaringClass.ifInstanceOf[ObjectType]
//        if (recv.className ==# "java/lang/System" || recv.className ==# "java/lang/Runtime") &&
//          desc ==# MethodDescriptor(Seq(BooleanType), VoidType)
        //the method descriptor is not checked in FindBugs - there's no need
        // && instruction.methodDescriptor ==# MethodDescriptor(Seq(BooleanType), VoidType)
        //if desc ==# MethodDescriptor(Seq(BooleanType), VoidType)
      } yield (classFile, method, instruction)
    }
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
