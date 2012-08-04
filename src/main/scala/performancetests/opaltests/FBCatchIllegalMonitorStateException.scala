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

trait FBCatchIllegalMonitorStateException {
  this: FBAnalysesBase =>

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
}
