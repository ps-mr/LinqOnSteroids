package performancetests.opaltests.analyses

import java.util.regex.Pattern
import de.tud.cs.st.bat.resolved._

/**
 *
 * Author: Ralf Mitschke
 * Date: 06.08.12
 * Time: 14:47
 *
 */
trait SIC_INNER_SHOULD_BE_STATIC_ANON{
    this: performancetests.opaltests.FBAnalysesBase =>

    import ivm.expressiontree.Exp

    val withinAnonymousClass = Pattern.compile("[$][0-9].*[$]")

    /**
     * A heuristic for determining whether an inner class is inside an anonymous inner class based on the class name
     */
    def isWithinAnonymousInnerClass(classFile: ClassFile): Boolean = {
      withinAnonymousClass.matcher(classFile.thisClass.className).find()
    }

    def isWithinAnonymousInnerClass(classFile: Exp[ClassFile]): Exp[Boolean] = {
          import ivm._
          import expressiontree._
          import Lifting._
          import BATLifting._
          import performancetests.opaltests.InstructionLifting._
      withinAnonymousClass.matcher(classFile.thisClass.className.value).find() // TODO: added a _.value is this correct?
    }

    def lastIndexOfInnerClassEncoding(classFile: ClassFile): Int = {
      val name = classFile.thisClass.className
      math.max(name.lastIndexOf('$'), name.lastIndexOf('+'))
    }

    def lastIndexOfInnerClassEncoding(classFile: Exp[ClassFile]): Exp[Int] = {
          import ivm._
          import expressiontree._
          import Lifting._
          import BATLifting._
          import performancetests.opaltests.InstructionLifting._
      val name = classFile.thisClass.className
      math.max(name.value.lastIndexOf('$'), name.value.lastIndexOf('+')) // TODO: added a _.value is this correct?
    }

    /**
     * A heuristic for determining inner classes by the encoding in the name
     */
    def isInnerClass(classFile: ClassFile): Boolean = {
      lastIndexOfInnerClassEncoding(classFile) >= 0
    }

    def isInnerClass(classFile: Exp[ClassFile]): Exp[Boolean] = {
          import ivm._
          import expressiontree._
          import Lifting._
          import BATLifting._
          import performancetests.opaltests.InstructionLifting._
      lastIndexOfInnerClassEncoding(classFile) >= 0
    }

    /**
     * A heuristic for determining anonymous inner classes by the encoding in the name
     */
    def isAnonymousInnerClass(classFile: ClassFile): Boolean = {
      val lastSpecialChar = lastIndexOfInnerClassEncoding(classFile)
      isInnerClass(classFile) &&
      Character.isDigit(classFile.thisClass.className.charAt(lastSpecialChar + 1))
    }


    def isAnonymousInnerClass(classFile: Exp[ClassFile]): Exp[Boolean] = {
          import ivm._
          import expressiontree._
          import Lifting._
          import BATLifting._
          import performancetests.opaltests.InstructionLifting._
      val lastSpecialChar = lastIndexOfInnerClassEncoding(classFile)
      isInnerClass(classFile) &&
      Character.isDigit(classFile.thisClass.className.value.charAt(lastSpecialChar.value + 1)) // TODO: added a _.value is this correct?
    }


    /**
     * A heuristic for determining whether an inner class can be made static
     */
    def canConvertToStaticInnerClass(classFile: ClassFile): Boolean = {
      !isWithinAnonymousInnerClass(classFile)
    }

    def canConvertToStaticInnerClass(classFile: Exp[ClassFile]): Exp[Boolean] = {
          import ivm._
          import expressiontree._
          import Lifting._
          import BATLifting._
          import performancetests.opaltests.InstructionLifting._
      !isWithinAnonymousInnerClass(classFile)
    }

    /**
     * A heuristic for determining whether the field points to the enclosing instance
     */
    def isOuterThisField(field: Field): Boolean = {
      field.name.startsWith("this$") || field.name.startsWith("this+")
    }

    def isOuterThisField(field: Exp[Field]): Exp[Boolean] = {
          import ivm._
          import expressiontree._
          import Lifting._
          import BATLifting._
          import performancetests.opaltests.InstructionLifting._
      field.name.startsWith("this$") || field.name.startsWith("this+")
    }

    /**
     * A heuristic that determines whether the outer this field is read, by counting aload_1 instructions
     * The count must be greater than 1, because the variable will be read once for storing it
     * into the field reference for the outer this instance.
     */
    def constructorReadsOuterThisField(classFile: ClassFile): Boolean = {
      (for (method ← classFile.constructors if (method.name == "<init>") && method.body.isDefined;
            instr ← method.body.get.instructions if (instr.isInstanceOf[ALOAD_1.type])
      ) yield 1).sum > 1
    }

    def constructorReadsOuterThisField(classFile: Exp[ClassFile]): Exp[Boolean] = {
          import ivm._
          import expressiontree._
          import Lifting._
          import BATLifting._
          import performancetests.opaltests.InstructionLifting._
      (for (method ← classFile.constructors if (method.name ==# "<init>") && method.body.isDefined;
            instr ← method.body.get.instructions if (instr.isInstanceOf_#[ALOAD_1.type])
      ) yield 1).sum > 1
    }

    def analyzeBaseWithoutAbstractions() = {
      val readFields = readFieldsNative.map(_._2)
      for (classFile ← classFiles
           if (isAnonymousInnerClass(classFile) &&
               canConvertToStaticInnerClass(classFile)
              );
           field ← classFile.fields
           if (isOuterThisField(field) &&
               !readFields.contains((classFile.thisClass, field.name, field.fieldType)) &&
               !constructorReadsOuterThisField(classFile)
              )
      ) yield
        (classFile)
    }

    def analyzeSQuOptWithoutAbstractions() = {
      import de.tud.cs.st.bat.resolved._
      import ivm._
      import expressiontree._
      import Lifting._
      import BATLifting._
      import performancetests.opaltests.InstructionLifting._
      import ivm.expressiontree.Util.ExtraImplicits._
      val readFields = readFieldsSQuOpt.map(_._2)
      for (classFile ← classFiles.asSmart
           if (isAnonymousInnerClass(classFile) &&
               canConvertToStaticInnerClass(classFile)
              );
           field ← classFile.fields
           if (isOuterThisField(field) &&
               !readFields.contains((classFile.thisClass, field.name, field.fieldType)) &&
               !constructorReadsOuterThisField(classFile)
              )
      ) yield
        (classFile)
    }


    def analyzeBaseWithAbstractions() = {
      val readFields = readFieldsNative.map(_._2)
      for (schema.FieldRecord(classFile, field) ← fieldsNative
           if (isAnonymousInnerClass(classFile) &&
               canConvertToStaticInnerClass(classFile) &&
               isOuterThisField(field) &&
               !readFields.contains((classFile.thisClass, field.name, field.fieldType)) &&
               !constructorReadsOuterThisField(classFile)
              )
      ) yield
        (classFile)
    }

    def analyzeSQuOptWithAbstractions() = {
        import de.tud.cs.st.bat.resolved._
        import ivm._
        import expressiontree._
        import Lifting._
        import BATLifting._
        import performancetests.opaltests.InstructionLifting._
        import ivm.expressiontree.Util.ExtraImplicits._
        import schema.squopt._
        val readFields = readFieldsSQuOpt.map(_._2)
        for (fieldRecord ← fieldsSQuOpt
             if (isAnonymousInnerClass(fieldRecord.classFile) &&
                 canConvertToStaticInnerClass(fieldRecord.classFile) &&
                 isOuterThisField(fieldRecord.field) &&
                 !readFields.contains((fieldRecord.classFile.thisClass, fieldRecord.field.name, fieldRecord.field.fieldType)) &&
                 !constructorReadsOuterThisField(fieldRecord.classFile)
                )
        ) yield
          (fieldRecord.classFile)
      }


    def analyzeSIC_INNER_SHOULD_BE_STATIC_ANON() {
      benchQueryComplete("SIC_INNER_SHOULD_BE_STATIC_ANON")(
        analyzeBaseWithoutAbstractions(),
        analyzeBaseWithAbstractions())(
        analyzeSQuOptWithoutAbstractions(),
        analyzeSQuOptWithAbstractions()
      )
    }
}