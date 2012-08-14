package performancetests.opaltests.analyses

import de.tud.cs.st.bat.resolved._

/**
 *
 * Author: Ralf Mitschke
 * Date: 06.08.12
 * Time: 15:22
 *
 */
trait ITA_INEFFICIENT_TO_ARRAY{
    this: performancetests.opaltests.FBAnalysesBase =>

    import BaseAnalyses._

    import ivm.expressiontree.Exp

    val objectArrayType = ArrayType(ObjectType("java/lang/Object"))

    val toArrayDescriptor = MethodDescriptor(List(objectArrayType), objectArrayType)

    val collectionInterface = ObjectType("java/util/Collection")

    val listInterface = ObjectType("java/util/List")

    def isCollectionType(t: ReferenceType): Boolean = {
      if (!t.isObjectType) {
        false
      } else {
        classHierarchy.isSubtypeOf(t.asInstanceOf[ObjectType], collectionInterface).getOrElse(false) ||
        t == listInterface // TODO needs more heuristic or more analysis
      }
    }

    def isCollectionType(t: Exp[ReferenceType]): Exp[Boolean] = {
      import ivm._
      import expressiontree._
      import Lifting._
      import BATLifting._
      if (!t.isObjectType.value) { // TODO: added a _.value is this correct?
        false
      } else {
        classHierarchy.isSubtypeOf(t.asInstanceOf_#[ObjectType].value, collectionInterface).getOrElse(false) || // TODO: added a _.value is this correct?
        t ==# listInterface // TODO needs more heuristic or more analysis
      }
    }

    def analyzeBaseWithoutAbstractions() = {
      for (classFile ← classFiles;
           method ← classFile.methods if method.body.isDefined;
           Seq((ICONST_0, _), (ANEWARRAY(_), _), (instr, idx)) ←
              withIndexNative(method.body.get.instructions).sliding(3) if (
                   instr match {
                     case INVOKEINTERFACE(targetType, "toArray", `toArrayDescriptor`)
                       if (isCollectionType(targetType)) => true
                     case INVOKEVIRTUAL(targetType, "toArray", `toArrayDescriptor`)
                       if (isCollectionType(targetType)) => true
                     case _                              => false
                   })
      ) yield
        (classFile,method, idx)
    }

    def analyzeSQuOptWithoutAbstractions() = {
      import ivm._
      import expressiontree._
      import Lifting._
      import BATLifting._
      import performancetests.opaltests.InstructionLifting._
      import ivm.expressiontree.Util.ExtraImplicits._
      for (classFile ← classFiles.asSmart;
           method ← classFile.methods if method.body.isDefined;
           instructionsWithIndex ← withIndexSQuOpt(method.body.get.instructions).sliding(3)
           if // instructionsWithIndex(0)._1.isInstanceOf_#[ICONST_0] && // TODO type ICONST_0 not found (why ???)
              instructionsWithIndex(1)._1.isInstanceOf_#[ANEWARRAY] &&
              (
              (instructionsWithIndex(2)._1.isInstanceOf_#[INVOKEINTERFACE] &&
                {val third = instructionsWithIndex(2)._1.asInstanceOf_#[INVOKEINTERFACE]
                 third.name ==# "toArray" && third.methodDescriptor ==# toArrayDescriptor && isCollectionType(third.declaringClass)
                }) ||
              (instructionsWithIndex(2)._1.isInstanceOf_#[INVOKEVIRTUAL] &&
                              {val third = instructionsWithIndex(2)._1.asInstanceOf_#[INVOKEVIRTUAL]
                               third.name ==# "toArray" && third.methodDescriptor ==# toArrayDescriptor && isCollectionType(third.declaringClass)
                              })
              )
      ) yield
        (classFile,method, instructionsWithIndex.last._2)
    }

    def analyzeBaseWithAbstractions() = {
      for ( schema.BytecodeInstrWindow(
                    Seq(_,_,idx), Seq(ICONST_0, ANEWARRAY(_),instr), classFile, method
            ) ← methodBodiesInstructionsSlidingNative(3)
            if (
                 instr match {
                   case INVOKEINTERFACE(targetType, "toArray", `toArrayDescriptor`)
                     if (isCollectionType(targetType)) => true
                   case INVOKEVIRTUAL(targetType, "toArray", `toArrayDescriptor`)
                     if (isCollectionType(targetType)) => true
                   case _                              => false
                 })
      ) yield
        (classFile,method, idx)
    }


/*
// TODO
//found   : Int(0)
//  required: ivm.expressiontree.OverloadHack.Overloaded2
//            if window.instrs(0).isInstanceOf_#[ICONST_0] &&

    def analyzeSQuOptWithAbstractions() = {
        import de.tud.cs.st.bat.resolved._
        import ivm._
        import expressiontree._
        import Lifting._
        import BATLifting._
        import performancetests.opaltests.InstructionLifting._
        import ivm.expressiontree.Util.ExtraImplicits._
        import schema.squopt._
        for ( window ← methodBodiesInstructionsSlidingSQuOpt(3)
           if window.instrs(0).isInstanceOf_#[ICONST_0] &&
              window.instrs(1).isInstanceOf_#[ANEWARRAY] &&
              (
              (window.instrs(2).isInstanceOf_#[INVOKEINTERFACE] &&
                {val third = window.instrs(2).asInstanceOf_#[INVOKEINTERFACE]
                 third.name ==# "toArray" && third.methodDescriptor ==# toArrayDescriptor && isCollectionType(third.declaringClass)
                }) ||
              (window.instrs(2).isInstanceOf_#[INVOKEVIRTUAL] &&
                              {val third = instructionsWithIndex(2).asInstanceOf_#[INVOKEVIRTUAL]
                               third.name ==# "toArray" && third.methodDescriptor ==# toArrayDescriptor && isCollectionType(third.declaringClass)
                              })
              )
        ) yield
          (window.classFile,window.method, window.instrIdxes.last)
    }
*/

}