package performancetests
package opaltests

import de.tud.cs.st.bat.resolved._

/**
 * User: pgiarrusso
 * Date: 4/8/2012
 */
// FB: SE_NO_SUITABLE_CONSTRUCTOR
// this analysis does not optimized away any of the abstractions
trait FBSerializableNoConstructor {
  this: FBAnalysesBase =>

  private def analyzeBaseWithoutAbstractions() = {
    val serializableClasses = classHierarchy.subclasses(ObjectType("java/io/Serializable")).getOrElse(Set.empty)
    import collection.{Seq => CSeq}
      for {
        superclass ← classHierarchy.superclasses(serializableClasses) if getClassFile.isDefinedAt(superclass) && // the class file of some supertypes (defined in libraries, which we do not analyze) may not be available
        {
          val superClassFile = getClassFile(superclass)
          !superClassFile.isInterfaceDeclaration &&
            !superClassFile.constructors.exists(_.descriptor.parameterTypes.length == 0)
        }
      } yield superclass // only the class is returned; there can be at most one method
  }


  private def analyzeSQuOptWithoutAbstractions() = {
    import de.tud.cs.st.bat.resolved._
    import ivm._
    import expressiontree._
    import Lifting._
    import BATLifting._
    import InstructionLifting._
    import ivm.expressiontree.Util.ExtraImplicits._
    val serializableClasses = classHierarchy.subclasses(ObjectType("java/io/Serializable")).getOrElse(Set.empty)
      for {
        superclass ← classHierarchy.superclasses(serializableClasses).asSmart if getClassFile.asSmart.isDefinedAt(superclass) && // the class file of some supertypes (defined in libraries, which we do not analyze) may not be available
        {
          val superClassFile = getClassFile.asSmart.apply(superclass)
          !superClassFile.isInterfaceDeclaration &&
            !superClassFile.constructors.exists(_.descriptor.parameterTypes.length ==# 0)
        }
      } yield superclass // only the class is returned; there can be at most one method
  }


  private def analyzeBaseWithAbstractions() = {
    import de.tud.cs.st.bat.resolved._
    import collection.{Seq => CSeq}
    import schema._
    import collection.{Seq => CSeq}
    val serializableClasses = classHierarchy.subclasses(ObjectType("java/io/Serializable")).getOrElse(Set.empty)
      for {
        superclass ← classHierarchy.superclasses(serializableClasses) if getClassFile.isDefinedAt(superclass) && // the class file of some supertypes (defined in libraries, which we do not analyze) may not be available
        {
          val superClassFile = getClassFile(superclass)
          !superClassFile.isInterfaceDeclaration &&
            !superClassFile.constructors.exists(_.descriptor.parameterTypes.length == 0)
        }
      } yield superclass // only the class is returned; there can be at most one method
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
      val serializableClasses = classHierarchy.subclasses(ObjectType("java/io/Serializable")).getOrElse(Set.empty)
            for {
              superclass ← classHierarchy.superclasses(serializableClasses).asSmart if getClassFile.asSmart.isDefinedAt(superclass) && // the class file of some supertypes (defined in libraries, which we do not analyze) may not be available
              {
                val superClassFile = getClassFile.asSmart.apply(superclass)
                !superClassFile.isInterfaceDeclaration &&
                  !superClassFile.constructors.exists(_.descriptor.parameterTypes.length ==# 0)
              }
            } yield superclass // only the class is returned; there can be at most one method
    }



  def analyzeSerializableNoConstructor() {
    // FINDBUGS: Se: Class is Serializable but its superclass doesn't define a void constructor (SE_NO_SUITABLE_CONSTRUCTOR)
    benchQueryComplete("NO_SUITABLE_CONSTRUCTOR")( // FB: SE_NO_SUITABLE_CONSTRUCTOR
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
