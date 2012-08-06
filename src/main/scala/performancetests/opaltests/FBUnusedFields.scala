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

trait FBUnusedFields {
  this: FBAnalysesBase =>

  /*
  //actual:
  FlatMap(
    Filter(
      ConstByIdentity(List(ClassFile(0,50,33,ObjectType(className="bugs/SuperA"),Some(ObjectType(className="java/lang/Obje..."))))),
      v110 => Not(ClassFile_isInterfaceDeclaration14(v110))),
    v111 => App(
      v544 => MapNode(
        Filter(
          ExpSeq(List(Diff(v544,
            App(
              v543 =>
                TypeCaseExp(v543,
                  ArrayBuffer(
                    TypeCase(class de.tud.cs.st.bat.resolved.GETFIELD,
                      v462 => Eq(Call1('declaringClass, v462),ClassFile_thisClass3(v111)),v463 => Call1('name, v463)),
                    TypeCase(class de.tud.cs.st.bat.resolved.GETSTATIC,
                      v464 => Eq(Call1('declaringClass, v464),ClassFile_thisClass3(v111)),v465 => Call1('name, v465)))),
              FlatMap(ClassFile_methods7(v111),v461 => FlatMap(Call1('Option_option2Iterable, Method_body12(v461)),v460 => Code_instructions2(v460))))
            ))),
          v467 => Not(IsEmpty(v467))),
        v542 => LiftTuple2(v111,v544)),
      Call1('TraversableLike$toSet, MapNode(Filter(ClassFile_fields6(v111),v447 => ClassMember_isPrivate2(v447)),v541 => Field_name1(v541)))))
  //expected:
  FlatMap(
    Filter(
      ConstByIdentity(List(ClassFile(0,50,33,ObjectType(className="bugs/SuperA"),Some(ObjectType(className="java/lang/Obje..."))))),
      v128 => Not(ClassFile_isInterfaceDeclaration14(v128))),
    v129 => App(
      v344 => MapNode(
        Filter(
          ExpSeq(List(Diff(v344,
            TypeCaseExp(
              FlatMap(ClassFile_methods7(v129),v266 => FlatMap(Call1('Option_option2Iterable, Method_body12(v266)),v265 => Code_instructions2(v265))),
              ArrayBuffer(
                TypeCase(class de.tud.cs.st.bat.resolved.GETFIELD,
                  v267 => Eq(Call1('declaringClass, v267),ClassFile_thisClass3(v129)),v268 => Call1('name, v268)),
                TypeCase(class de.tud.cs.st.bat.resolved.GETSTATIC,
                  v269 => Eq(Call1('declaringClass, v269),ClassFile_thisClass3(v129)),v270 => Call1('name, v270))))
            ))),
        v271 => Not(IsEmpty(v271))),
      v343 => LiftTuple2(v129,v344)),
    Call1('TraversableLike$toSet, MapNode(Filter(ClassFile_fields6(v129),v263 => ClassMember_isPrivate2(v263)),v342 => Field_name1(v342)))))
  */

  /*2012-07-06 18:12
  FlatMap(Filter(
    ConstByIdentity(List(ClassFile(0,50,33,ObjectType(className="bugs/SuperA"),Some(ObjectType(className="java/lang/Obje..."))))),
    v3784 => Not(ClassFile_isInterfaceDeclaration14(v3784))),
    v3728 => MapNode(Filter(
      ExpSeq(List(Call1('TraversableLike$toSet, MapNode(Filter(ClassFile_fields6(v3728),v3786 => ClassMember_isPrivate2(v3786)),v4084 => Field_name1(v4084))))),
      v3790 => Not(IsEmpty(
        Filter(
          ExpSeq(List(Diff(v3790,
          TypeCaseExp(FlatMap(ClassFile_methods7(v3728),
            v3765 => FlatMap(Call1('Option_option2Iterable, Method_body12(v3765)),v3764 => Code_instructions2(v3764))),
            ArrayBuffer(
              TypeCase(class de.tud.cs.st.bat.resolved.GETFIELD,v3766 => Eq(Call1('declaringClass, v3766),ClassFile_thisClass3(v3728)),v3767 => Call1('name, v3767)),
              TypeCase(class de.tud.cs.st.bat.resolved.GETSTATIC,v3768 => Eq(Call1('declaringClass, v3768),ClassFile_thisClass3(v3728)),v3769 => Call1('name, v3769))))))),
          v3788 => Not(IsEmpty(v3788)))))),
  v4085 => LiftTuple2(v3728,v4085)))*/

  def analyzeUnusedFields() {
    // FINDBUGS: UuF: Unused field (UUF_UNUSED_FIELD)
    //XXX This analysis was totally rewritten for BAT, reconsider.
    /*val unusedFields: Seq[(ClassFile, Set[String])] = benchMark("UUF_UNUSED_FIELD") {
      for {
        classFile ← classFiles if !classFile.isInterfaceDeclaration
        instructions = for {
          method ← classFile.methods
          body ← method.body.toList
          instruction ← body.instructions
        } yield instruction
        declaringClass = classFile.thisClass
        privateFields = (for (field ← classFile.fields if field.isPrivate) yield field.name).toSet
        usedPrivateFields = instructions withFilter {
          case GETFIELD(`declaringClass`, _, _) ⇒ true
          case GETSTATIC(`declaringClass`, _, _) ⇒ true
          case _ ⇒ false
        } map {
          case GETFIELD(`declaringClass`, name, _) ⇒ name
          case GETSTATIC(`declaringClass`, name, _) ⇒ name
        }
        unusedPrivateFields = privateFields -- usedPrivateFields //for (field ← privateFields if !usedPrivateFields.contains(field)) yield field
        if unusedPrivateFields.size > 0
      } yield (classFile, privateFields)
    }*/
    def getPrivateFields(classFile: ClassFile) = (for (field ← classFile.fields if field.isPrivate) yield field.name).toSet //XXX toSet is unneeded, field names are unique.
    def usedPrivateFields(classFile: ClassFile, declaringClass: ObjectType) = for {
        method ← classFile.methods
        body ← method.body.toList
        instruction ← body.instructions
        usedPrivateField ← instruction match {
          case GETFIELD(`declaringClass`, name, _) ⇒ Some(name)
          case GETSTATIC(`declaringClass`, name, _) ⇒ Some(name)
          case _ ⇒ None
        }
      } yield usedPrivateField //for (field ← privateFields if !usedPrivateFields.contains(field)) yield field
    def getPrivateFieldsLos(classFile: Exp[ClassFile]) = {
      import BATLifting._
      (for (field ← classFile.fields if field.isPrivate) yield field.name).toSet  //XXX toSet is unneeded, field names are unique.
    }
    def usedPrivateFieldsLos(classFile: Exp[ClassFile], declaringClass: Exp[ObjectType]) = {
      import BATLifting._
      import InstructionLifting._
      for {
        instructions ← Let(for {
          method ← classFile.methods
          body ← method.body
          instruction ← body.instructions
        } yield instruction) //XXX reuse code from methodBodiesInstructionsModularSQuOpt().
        name <- instructions.typeCase(
          when[GETFIELD](asGETFIELD => asGETFIELD.declaringClass ==# declaringClass, _.name),
          when[GETSTATIC](asGETSTATIC => asGETSTATIC.declaringClass ==# declaringClass, _.name))
      } yield name
      /*(for {
        method ← classFile.methods
        body ← method.body.toList
        instruction ← body.instructions
        usedPrivateField ← instruction match {
          case GETFIELD(`declaringClass`, name, _) ⇒ Some(name)
          case GETSTATIC(`declaringClass`, name, _) ⇒ Some(name)
          case _ ⇒ None
        }
      } yield usedPrivateField)*/
    } //for (field ← privateFields if !usedPrivateFields.contains(field)) yield field


    benchQueryComplete("UNUSED_PRIVATE_FIELD")({ // FB: "UUF_UNUSED_FIELD") { // However, we only focus on private fields.
      for {
        classFile ← classFiles if !classFile.isInterfaceDeclaration
        declaringClass = classFile.thisClass
        privateFields = (for (field ← classFile.fields if field.isPrivate) yield field.name).toSet //XXX toSet is unneeded, field names are unique.
        //Note that this could even allow unnesting - should we try to have this unnested?
        unusedPrivateFields = privateFields -- (for {
          method ← classFile.methods
          body ← method.body.toList
          instruction ← body.instructions
          usedPrivateField ← instruction match {
            case GETFIELD(`declaringClass`, name, _) ⇒ Some(name)
            case GETSTATIC(`declaringClass`, name, _) ⇒ Some(name)
            case _ ⇒ None
          }
        } yield usedPrivateField) //for (field ← privateFields if !usedPrivateFields.contains(field)) yield field
        if unusedPrivateFields.size > 0
      } yield (classFile, unusedPrivateFields)
    }, {
      for {
        classFile ← classFiles if !classFile.isInterfaceDeclaration
        declaringClass = classFile.thisClass
        privateFields = getPrivateFields(classFile)
        //Note that this could even allow unnesting - should we try to have this unnested?
        unusedPrivateFields = privateFields -- usedPrivateFields(classFile, declaringClass)
        if unusedPrivateFields.size > 0
      } yield (classFile, unusedPrivateFields)
    })({
      import BATLifting._
      import InstructionLifting._
      for {
        classFile ← classFiles.asSmart if !classFile.isInterfaceDeclaration
        instructions ← Let(for {
          method ← classFile.methods
          body ← method.body
          instruction ← body.instructions
        } yield instruction)
        declaringClass ← Let(classFile.thisClass)
        privateFields ← Let((for (field ← classFile.fields if field.isPrivate) yield field.name).toSet)
        usedPrivateFields ← Let(instructions.typeCase(
          when[GETFIELD](asGETFIELD => asGETFIELD.declaringClass ==# declaringClass, _.name),
          when[GETSTATIC](asGETSTATIC => asGETSTATIC.declaringClass ==# declaringClass, _.name)))
        unusedPrivateFields ← Let(privateFields -- usedPrivateFields) //for (field ← privateFields if !usedPrivateFields.contains(field)) yield field
        if unusedPrivateFields.size > 0
      } yield (classFile, unusedPrivateFields)
    }, Optimization removeIdentityMaps {
      import BATLifting._
      for {
        classFile ← classFiles.asSmart if !classFile.isInterfaceDeclaration
        declaringClass ← Let(classFile.thisClass)
        privateFields ← Let(getPrivateFieldsLos(classFile))
        usedPrivateFields ← Let(usedPrivateFieldsLos(classFile, declaringClass))
        unusedPrivateFields ← Let(privateFields -- usedPrivateFields) //for (field ← privateFields if !usedPrivateFields.contains(field)) yield field
        if unusedPrivateFields.size > 0
      } yield (classFile, unusedPrivateFields)
    })
  }

}
