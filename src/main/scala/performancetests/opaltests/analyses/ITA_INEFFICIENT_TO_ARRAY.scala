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

    import ivm.expressiontree.Exp

    private val objectArrayType = ArrayType(ObjectType("java/lang/Object"))

    private val toArrayDescriptor = MethodDescriptor(List(objectArrayType), objectArrayType)

    private val collectionInterface = ObjectType("java/util/Collection")

    private val listInterface = ObjectType("java/util/List")

    private def isCollectionType(t: ReferenceType): Boolean = {
      if (!t.isObjectType) {
        false
      } else {
        classHierarchy.isSubtypeOf(t.asInstanceOf[ObjectType], collectionInterface).getOrElse(false) ||
        t == listInterface // TODO needs more heuristic or more analysis
      }
    }

    private def isCollectionType(t: Exp[ReferenceType]): Exp[Boolean] = {
      import ivm._
      import expressiontree._
      import Lifting._
      import BATLifting._
      if_# (!t.isObjectType) {
        false
      } else_# {
        classHierarchySQuOpt.isSubtypeOf(t.asInstanceOf_#[ObjectType], collectionInterface).getOrElse(false) ||
        t ==# listInterface // TODO needs more heuristic or more analysis
      }
    }

    private def analyzeBaseWithoutAbstractions() = {
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

    private def analyzeSQuOptWithoutAbstractions() = {
      import ivm._
      import expressiontree._
      import Lifting._
      import BATLifting._
      import performancetests.opaltests.InstructionLifting._
      import ivm.expressiontree.Util.ExtraImplicits._
      for (classFile ← classFiles.asSquopt;
           method ← classFile.methods if method.body.isDefined;
           instructionsWithIndex ← withIndexSQuOpt(method.body.get.instructions).sliding(3)
           if instructionsWithIndex(0)._1 ==# ICONST_0 &&
              instructionsWithIndex(1)._1.isInstanceOf_#[ANEWARRAY] &&
              (
              instructionsWithIndex(2)._1.ifInstanceOf[INVOKEINTERFACE].fold(false){
                third =>
                 third.name ==# "toArray" && third.methodDescriptor ==# toArrayDescriptor && isCollectionType(third.declaringClass)
                } ||
              instructionsWithIndex(2)._1.ifInstanceOf[INVOKEVIRTUAL].fold(false) {
                              third =>
                               third.name ==# "toArray" && third.methodDescriptor ==# toArrayDescriptor && isCollectionType(third.declaringClass)
                              }
              )
      ) yield
        (classFile,method, instructionsWithIndex.last._2)
    }

    private def analyzeBaseWithAbstractions() = {
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


   private def analyzeSQuOptWithAbstractions() = {
        import de.tud.cs.st.bat.resolved._
        import ivm._
        import expressiontree._
        import Lifting._
        import BATLifting._
        import performancetests.opaltests.InstructionLifting._
        import ivm.expressiontree.Util.ExtraImplicits._
        import schema.squopt._
        for ( window ← methodBodiesInstructionsSlidingSQuOpt(3)
           if window.instrs.apply(0) ==# ICONST_0 &&  // TODO had to add .apply() otherwise required: ivm.expressiontree.OverloadHack.Overloaded2 (why???)
              window.instrs.apply(1).isInstanceOf_#[ANEWARRAY] && // TODO had to add .apply() otherwise required: ivm.expressiontree.OverloadHack.Overloaded2 (why???)
              (
              window.instrs.apply(2).ifInstanceOf[INVOKEINTERFACE].fold(false) { // TODO had to add .apply() otherwise required: ivm.expressiontree.OverloadHack.Overloaded2 (why???)
                third =>
                 third.name ==# "toArray" && third.methodDescriptor ==# toArrayDescriptor && isCollectionType(third.declaringClass)
                } ||
              window.instrs.apply(2).ifInstanceOf[INVOKEVIRTUAL].fold(false) { // TODO had to add .apply() otherwise required: ivm.expressiontree.OverloadHack.Overloaded2 (why???)
                              third =>
                               third.name ==# "toArray" && third.methodDescriptor ==# toArrayDescriptor && isCollectionType(third.declaringClass)
                              }
              )
        ) yield
          (window.classFile,window.method, window.instrIdxes.last)
    }

    def analyzeITA_INEFFICIENT_TO_ARRAY() {
      benchQueryComplete("ITA_INEFFICIENT_TO_ARRAY")(
                                                       analyzeBaseWithoutAbstractions(),
                                                       analyzeBaseWithAbstractions()
                                                      )(
                                                       analyzeSQuOptWithoutAbstractions(),
                                                       analyzeSQuOptWithAbstractions()
                                           )
    }

}
