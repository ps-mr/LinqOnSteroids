package performancetests
package opaltests

import ivm._
import expressiontree._
import Lifting._
import optimization.Optimization

import de.tud.cs.st.bat
import bat.resolved._
import analyses._

import reader.Java6Framework
import analyses.ClassHierarchy

import collection.immutable.Seq
import collection.{Seq => CSeq}

/**
 * User: pgiarrusso
 * Date: 4/8/2012
 */

trait FBExplicitGC {
  this: FBAnalysesBase =>

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
        (classFile, method, body, instruction) <- methodBodiesInstructionsModularNative()
        if (instruction match {
          case INVOKESTATIC(ObjectType("java/lang/System"), "gc", NoArgNoRetMethodDesc) |
               INVOKEVIRTUAL(ObjectType("java/lang/Runtime"), "gc", NoArgNoRetMethodDesc) ⇒ true
          case _ ⇒ false
        })
      } yield (classFile, method, instruction)).toSet
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
        methodAndBody <- methodBodiesSQuOpt() //<---
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
        methodAndBody <- methodBodiesModularSQuOpt() //<--- changed
        instruction <- methodAndBody.body.instructions.typeCase(
          when[INVOKESTATIC](instr =>
            instr.declaringClass ==# ObjectType("java/lang/System") && instr.name ==# "gc" && instr.methodDescriptor ==# NoArgNoRetMethodDesc, identity),
          when[INVOKEVIRTUAL](instr =>
            instr.declaringClass ==# ObjectType("java/lang/Runtime") && instr.name ==# "gc" && instr.methodDescriptor ==# NoArgNoRetMethodDesc, identity))
      } yield (methodAndBody.classFile, methodAndBody.method, instruction)).toSet
    }/*, {
      import BATLifting._
      import InstructionLifting._
      import dbschema.squopt._
      (for {
        methodBodyInstr <- methodBodiesInstructionsModularSQuOpt() //<--- changed
        instruction <- methodBodyInstr.body.instructions.typeCase( //How do we write typeCase on a single value? Not possible - not directly.
          when[INVOKESTATIC](instr =>
            instr.declaringClass ==# ObjectType("java/lang/System") && instr.name ==# "gc" && instr.methodDescriptor ==# NoArgNoRetMethodDesc, identity),
          when[INVOKEVIRTUAL](instr =>
            instr.declaringClass ==# ObjectType("java/lang/Runtime") && instr.name ==# "gc" && instr.methodDescriptor ==# NoArgNoRetMethodDesc, identity))
      } yield (methodBodyInstr._1, methodBodyInstr._2, instruction)).toSet
    }*/)
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
}
