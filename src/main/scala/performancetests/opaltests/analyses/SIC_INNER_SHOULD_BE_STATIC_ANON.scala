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

    private val withinAnonymousClass = Pattern.compile("[$][0-9].*[$]")

    /**
     * A heuristic for determining whether an inner class is inside an anonymous inner class based on the class name
     */
    private def isWithinAnonymousInnerClass(classFile: ClassFile): Boolean = {
      withinAnonymousClass.matcher(classFile.thisClass.className).find()
    }

    private def isWithinAnonymousInnerClass(classFile: Exp[ClassFile]): Exp[Boolean] = {
          import ivm._
          import expressiontree._
          import Lifting._
          import BATLifting._
          import performancetests.opaltests.InstructionLifting._
      withinAnonymousClass.matcher(classFile.thisClass.className).find()
    }

    private def lastIndexOfInnerClassEncoding(classFile: ClassFile): Int = {
      val name = classFile.thisClass.className
      math.max(name.lastIndexOf('$'), name.lastIndexOf('+'))
    }

    private def lastIndexOfInnerClassEncoding(classFile: Exp[ClassFile]): Exp[Int] = {
          import ivm._
          import expressiontree._
          import Lifting._
          import BATLifting._
          import performancetests.opaltests.InstructionLifting._
      val name = classFile.thisClass.className
      expMath.max(name.lastIndexOf('$'), name.lastIndexOf('+'))
    }

    /**
     * A heuristic for determining inner classes by the encoding in the name
     */
    private def isInnerClass(classFile: ClassFile): Boolean = {
      lastIndexOfInnerClassEncoding(classFile) >= 0
    }

    private def isInnerClass(classFile: Exp[ClassFile]): Exp[Boolean] = {
          import ivm._
          import expressiontree._
          import Lifting._
          import BATLifting._
          import performancetests.opaltests.InstructionLifting._
      lastIndexOfInnerClassEncoding(classFile) >= 0
    }

    /**
     * A heuristic for determining anonymous inner classes by the encoding in the name.
     * PG: I changed this heuristic to avoid failure on Scala class names, which
     * often end with $.
     */
    private def isAnonymousInnerClass(classFile: ClassFile): Boolean = {
      val lastSpecialChar = lastIndexOfInnerClassEncoding(classFile)
      val className = classFile.thisClass.className

      isInnerClass(classFile) &&
      className.length > lastSpecialChar + 1 &&
      Character.isDigit(className.charAt(lastSpecialChar + 1))
    }


    private def isAnonymousInnerClass(classFile: Exp[ClassFile]): Exp[Boolean] = {
          import ivm._
          import expressiontree._
          import Lifting._
          import BATLifting._
          import performancetests.opaltests.InstructionLifting._

      val lastSpecialChar = lastIndexOfInnerClassEncoding(classFile)
      val className = classFile.thisClass.className

      isInnerClass(classFile) &&
      className.length > lastSpecialChar + 1 &&
      expCharacter.isDigit(className.charAt(lastSpecialChar + 1))
    }


    /**
     * A heuristic for determining whether an inner class can be made static
     */
    private def canConvertToStaticInnerClass(classFile: ClassFile): Boolean = {
      !isWithinAnonymousInnerClass(classFile)
    }

    private def canConvertToStaticInnerClass(classFile: Exp[ClassFile]): Exp[Boolean] = {
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
    private def isOuterThisField(field: Field): Boolean = {
      field.name.startsWith("this$") || field.name.startsWith("this+")
    }

    private def isOuterThisField(field: Exp[Field]): Exp[Boolean] = {
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
    private def constructorReadsOuterThisField(classFile: ClassFile): Boolean = {
      (for (method ← classFile.constructors if (method.name == "<init>") && method.body.isDefined;
            instr ← method.body.get.instructions if instr == ALOAD_1
      ) yield instr).size > 1
    }

    private def constructorReadsOuterThisField(classFile: Exp[ClassFile]): Exp[Boolean] = {
          import ivm._
          import expressiontree._
          import Lifting._
          import BATLifting._
          import performancetests.opaltests.InstructionLifting._
          //XXX To get the size, the code had 'yield 1' followed by .sum on the resulting collection.
          //I optimized this manually, but we could consider automating that.
          //I also transformed instr.isInstanceOf_#[ALOAD_1.type] to instr ==#
          //ALOAD_1; this could also maybe be automated, with enough help from reflection.
      (for (method ← classFile.constructors if (method.name ==# "<init>") && method.body.isDefined;
            instr ← method.body.get.instructions if instr ==# ALOAD_1
      ) yield instr).size > 1
    }

    private def analyzeBaseWithoutAbstractions() = {
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
        (classFile.thisClass)
    }

    private def analyzeSQuOptWithoutAbstractions() = {
      import de.tud.cs.st.bat.resolved._
      import ivm._
      import expressiontree._
      import Lifting._
      import BATLifting._
      import performancetests.opaltests.InstructionLifting._
      import ivm.expressiontree.Util.ExtraImplicits._
      for {readFields ← Let(readFieldsSQuOpt.map(_._2))
           classFile ← classFiles.asSquopt
           if (isAnonymousInnerClass(classFile) &&
               canConvertToStaticInnerClass(classFile)
              );
           field ← classFile.fields
           if (isOuterThisField(field) &&
               !readFields.contains((classFile.thisClass, field.name, field.fieldType)) &&
               !constructorReadsOuterThisField(classFile)
              )
      } yield
        (classFile.thisClass)
    }


    private def analyzeBaseWithAbstractions() = {
      val readFields = readFieldsNative.map(_._2)
      for (schema.FieldRecord(classFile, field) ← fieldsNative
           if (isAnonymousInnerClass(classFile) &&
               canConvertToStaticInnerClass(classFile) &&
               isOuterThisField(field) &&
               !readFields.contains((classFile.thisClass, field.name, field.fieldType)) &&
               !constructorReadsOuterThisField(classFile)
              )
      ) yield
        (classFile.thisClass)
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
        for {readFields ← Let(readFieldsSQuOpt.map(_._2))
             fieldRecord ← fieldsSQuOpt
             if (isAnonymousInnerClass(fieldRecord.classFile) &&
                 canConvertToStaticInnerClass(fieldRecord.classFile) &&
                 isOuterThisField(fieldRecord.field) &&
                 !readFields.contains((fieldRecord.classFile.thisClass, fieldRecord.field.name, fieldRecord.field.fieldType)) &&
                 !constructorReadsOuterThisField(fieldRecord.classFile)
                )
        } yield
          (fieldRecord.classFile.thisClass)
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
