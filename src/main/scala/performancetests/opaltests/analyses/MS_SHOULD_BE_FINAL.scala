package performancetests.opaltests.analyses

import de.tud.cs.st.bat.resolved._
import de.tud.cs.st.bat.resolved.Field

/**
 *
 * Author: Ralf Mitschke
 * Date: 06.08.12
 * Time: 16:01
 *
 */
trait MS_SHOULD_BE_FINAL{
  this: performancetests.opaltests.FBAnalysesBase =>

  private val hashTableType = ObjectType("java/util/Hashtable")

  private def isHashTable(t: FieldType) = t == hashTableType

  private def isArray(t: FieldType) = t.isArrayType

  private def isHashTable(t: ivm.expressiontree.Exp[FieldType]) = {
      import ivm._
      import expressiontree._
      import Lifting._
      import BATLifting._
      t ==# hashTableType
  }

  private def isArray(t:  ivm.expressiontree.Exp[FieldType]) = {
      import ivm._
      import expressiontree._
      import Lifting._
      import BATLifting._
      t.isArrayType
  }



  private def analyzeBaseWithoutAbstractions() = {
    import de.tud.cs.st.bat.resolved._
        for (classFile ← classFiles if (!classFile.isInterfaceDeclaration);
             field ← classFile.fields
             if (!field.isFinal &&
                 field.isStatic &&
                 !field.isSynthetic &&
                 !field.isVolatile &&
                 (field.isPublic || field.isProtected) &&
                 !isArray(field.fieldType) && !isHashTable(field.fieldType)
                )
        ) yield
          (classFile, field)
  }

    private def analyzeSQuOptWithoutAbstractions() = {
      import de.tud.cs.st.bat.resolved._
      import ivm._
      import expressiontree._
      import Lifting._
      import BATLifting._
      import performancetests.opaltests.InstructionLifting._
      import ivm.expressiontree.Util.ExtraImplicits._

        for (classFile ← classFiles.asSmart if (!classFile.isInterfaceDeclaration);
             field ← classFile.fields
             if (!field.isFinal &&
                 field.isStatic &&
                 !field.isSynthetic &&
                 !field.isVolatile &&
                 (field.isPublic || field.isProtected) &&
                 !isArray(field.fieldType) && !isHashTable(field.fieldType)
                )
        ) yield
          (classFile, field)
    }


    private def analyzeBaseWithAbstractions() = {
      import de.tud.cs.st.bat.resolved._
      import schema._
      for (
           FieldRecord(classFile, field) ← fieldsNative
             if (!field.isFinal &&
                 field.isStatic &&
                 !field.isSynthetic &&
                 !field.isVolatile &&
                 (field.isPublic || field.isProtected) &&
                 !isArray(field.fieldType) && !isHashTable(field.fieldType)
                )
      ) yield
        (classFile, field)
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

        for (
             fieldRecord ← fieldsSQuOpt
               if (!fieldRecord.field.isFinal &&
                   fieldRecord.field.isStatic &&
                   !fieldRecord.field.isSynthetic &&
                   !fieldRecord.field.isVolatile &&
                   (fieldRecord.field.isPublic || fieldRecord.field.isProtected) &&
                   !isArray(fieldRecord.field.fieldType) && !isHashTable(fieldRecord.field.fieldType)
                  )
        ) yield
          (fieldRecord.classFile, fieldRecord.field)
      }


    def analyzeMS_SHOULD_BE_FINAL() {
      benchQueryComplete("MS_SHOULD_BE_FINAL")(
        analyzeBaseWithoutAbstractions(),
        analyzeBaseWithAbstractions()
        )(
        analyzeSQuOptWithoutAbstractions(),
        analyzeSQuOptWithAbstractions()
      )
    }

}
