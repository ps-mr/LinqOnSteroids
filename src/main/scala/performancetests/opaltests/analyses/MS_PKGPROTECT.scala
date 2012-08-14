package performancetests.opaltests.analyses

import de.tud.cs.st.bat.resolved._

/**
 *
 * Author: Ralf Mitschke
 * Date: 06.08.12
 * Time: 16:01
 *
 */

trait MS_PKGPROTECT{
    this: performancetests.opaltests.FBAnalysesBase =>

    import ivm.expressiontree.Exp

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
        val readFieldsFromPackage = readFieldsNative.map(entry => (entry._1._1.thisClass.packageName, entry._2))
        for (classFile ← classFiles if (!classFile.isInterfaceDeclaration);
             field ← classFile.fields
             if (field.isFinal &&
                 field.isStatic &&
                 !field.isSynthetic &&
                 !field.isVolatile &&
                 (field.isPublic || field.isProtected) &&
                 (isArray(field.fieldType) || isHashTable(field.fieldType)) &&
                 !readFieldsFromPackage.exists(entry => entry._2 ==(classFile.thisClass, field.name, field.fieldType) && entry._1 != classFile.thisClass.packageName)
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

      //[error] D:\workspace\LinqOnSteroids\src\main\scala\performancetests\opaltests\analyses\MS_PKGPROTECT.scala:71: value thisClass is not a member of ivm.expressiontree.Exp[ivm.expressiontree.Exp[de.tud.cs.st.bat.resolved.ClassFile]]
      //[error]                               .map(entry => (entry._1._1.thisClass.packageName, entry._2))
      //[error]                                                          ^
        val readFieldsFromPackage = readFieldsSQuOpt
                              .map(entry => (entry._1._1.thisClass.packageName, entry._2))
        for (classFile ← classFiles.asSmart if (!classFile.isInterfaceDeclaration);
             field ← classFile.fields
             if (field.isFinal &&
                 field.isStatic &&
                 //!field.isSynthetic && // TODO generate a BATLifting with isSynthetic
                 !field.isVolatile &&
                 (field.isPublic || field.isProtected) &&
                 (isArray(field.fieldType) || isHashTable(field.fieldType)) &&
                 !readFieldsFromPackage.exists(entry => entry._2 ==#(classFile.thisClass, field.name, field.fieldType) && entry._1 !=# classFile.thisClass.packageName)
                )
        ) yield
          (classFile, field)
    }


    private def analyzeBaseWithAbstractions() = {
      import de.tud.cs.st.bat.resolved._
      import schema._
      val readFieldsFromPackage = readFieldsNative.map(entry => (entry._1._1.thisClass.packageName, entry._2))
      for (
           FieldRecord(classFile, field) ← fieldsNative
             if (field.isFinal &&
                 field.isStatic &&
                 !field.isSynthetic &&
                 !field.isVolatile &&
                 (field.isPublic || field.isProtected) &&
                 (isArray(field.fieldType) || isHashTable(field.fieldType)) &&
                 !readFieldsFromPackage.exists(entry => entry._2 ==(classFile.thisClass, field.name, field.fieldType) && entry._1 != classFile.thisClass.packageName)
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
        val readFieldsFromPackage = readFieldsSQuOpt
                              .map(entry => (entry._1._1.thisClass.packageName, entry._2))
        for (
             fieldRecord ← fieldsSQuOpt
               if (fieldRecord.field.isFinal &&
                   fieldRecord.field.isStatic &&
                   //!fieldRecord.field.isSynthetic && // TODO generate a BATLifting with isSynthetic
                   !fieldRecord.field.isVolatile &&
                   (fieldRecord.field.isPublic || fieldRecord.field.isProtected) &&
                   (isArray(fieldRecord.field.fieldType) || isHashTable(fieldRecord.field.fieldType)) &&
                   !readFieldsFromPackage.exists(entry => entry._2 ==#(fieldRecord.classFile.thisClass, fieldRecord.field.name, fieldRecord.field.fieldType) && entry._1 !=# fieldRecord.classFile.thisClass.packageName)
                  )
        ) yield
          (fieldRecord.classFile, fieldRecord.field)
      }



    def analyzeMS_PKGPROTECT() {
      benchQueryComplete("MS_PKGPROTECT")(
        analyzeBaseWithoutAbstractions(),
        analyzeBaseWithAbstractions())(
        analyzeSQuOptWithoutAbstractions(),
        analyzeSQuOptWithAbstractions()
      )
    }

}