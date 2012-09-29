package performancetests
package opaltests

/**
 * User: pgiarrusso
 * Date: 4/8/2012
 */
// CO_ABSTRACT_SELF && CO_SELF_NO_OBJECT
trait FBCovariantCompareToMethods {
  this: FBAnalysesBase =>

  private val comparableType = de.tud.cs.st.bat.resolved.ObjectType("java/lang/Comparable")

  private def analyzeBaseWithoutAbstractions() = {
    import de.tud.cs.st.bat.resolved._
    import collection.{Seq => CSeq}
    for {
            allComparables ← classHierarchy.subtypes(comparableType).toList
            comparable ← allComparables
            classFile ← getClassFile.get(comparable).toList
            method @ Method(_, "compareTo", MethodDescriptor(CSeq(parameterType), IntegerType), _) ← classFile.methods if parameterType != ObjectType.Object
          } yield (classFile.thisClass, method.name, method.descriptor)
  }


  private def analyzeSQuOptWithoutAbstractions() = {
    import de.tud.cs.st.bat.resolved._
    import ivm._
    import expressiontree._
    import Lifting._
    import BATLifting._
    import InstructionLifting._
    import ivm.expressiontree.Util.ExtraImplicits._
      for {
        allComparables ← classHierarchy.subtypes(comparableType).toList.asSquopt
        comparable ← allComparables
        classFile ← getClassFile.get(comparable) //getClassFile is lifted through Const and makes optimization expensive.
        method ← classFile.methods //if parameterType != ObjectType.Object
        if method.name ==# "compareTo" && method.descriptor.returnType ==# IntegerType
        parameterTypes <- Let(method.descriptor.parameterTypes)
        if parameterTypes.length ==# 1 && parameterTypes(0) !=# ObjectType.Object

      } yield (classFile.thisClass, method.name, method.descriptor)
  }


  private def analyzeBaseWithAbstractions() = {
    import de.tud.cs.st.bat.resolved._
    import collection.{Seq => CSeq}
    import schema._
    for {
            allComparables ← classHierarchy.subtypes(comparableType).toList
            comparable ← allComparables
            classFile ← getClassFile.get(comparable).toList
            method @ Method(_, "compareTo", MethodDescriptor(CSeq(parameterType), IntegerType), _) ← classFile.methods if parameterType != ObjectType.Object
          } yield (classFile.thisClass, method.name, method.descriptor)
  }


  private def analyzeSQuOptWithAbstractions() = {
      import de.tud.cs.st.bat.resolved._
      import ivm._
      import expressiontree._
      import Lifting._
      import BATLifting._
      import InstructionLifting._
      import ivm.expressiontree.Util.ExtraImplicits._
      import schema.squopt._
      for {
        allComparables ← classHierarchy.subtypes(comparableType).toList.asSquopt
        comparable ← allComparables
        classFile ← getClassFile.get(comparable) //getClassFile is lifted through Const and makes optimization expensive.
        method ← classFile.methods //if parameterType != ObjectType.Object
        if method.name ==# "compareTo" && method.descriptor.returnType ==# IntegerType
        parameterTypes <- Let(method.descriptor.parameterTypes)
        if parameterTypes.length ==# 1 && parameterTypes(0) !=# ObjectType.Object

      } yield (classFile.thisClass, method.name, method.descriptor)
    }



  def analyzeCovariantCompareToMethods() {

    benchQueryComplete("COVARIANT_COMPARETO")(// FB: "CO_SELF_NO_OBJECT/CO_ABSTRACT_SELF") {
      // Weakness: In a project, where we extend a predefined class (of the JDK) that
      // inherits from Comparable and in which we define covariant comparesTo method,
      // we will not be able to identify this issue unless we have identified the whole
      // class hierarchy.
       analyzeBaseWithoutAbstractions(),
       analyzeBaseWithAbstractions()
      )(
       analyzeSQuOptWithoutAbstractions(),
       analyzeSQuOptWithAbstractions()
      )
  }
}
