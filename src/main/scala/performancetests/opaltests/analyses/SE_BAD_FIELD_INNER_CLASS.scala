/* License (BSD Style License):
*  Copyright (c) 2009, 2011
*  Software Technology Group
*  Department of Computer Science
*  Technische Universität Darmstadt
*  All rights reserved.
*
*  Redistribution and use in source and binary forms, with or without
*  modification, are permitted provided that the following conditions are met:
*
*  - Redistributions of source code must retain the above copyright notice,
*    this list of conditions and the following disclaimer.
*  - Redistributions in binary form must reproduce the above copyright notice,
*    this list of conditions and the following disclaimer in the documentation
*    and/or other materials provided with the distribution.
*  - Neither the name of the Software Technology Group or Technische
*    Universität Darmstadt nor the names of its contributors may be used to
*    endorse or promote products derived from this software without specific
*    prior written permission.
*
*  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
*  AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
*  IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
*  ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
*  LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
*  CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
*  SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
*  INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
*  CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
*  ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
*  POSSIBILITY OF SUCH DAMAGE.
*/
package performancetests.opaltests.analyses

import de.tud.cs.st.bat.resolved._
import ivm._
import expressiontree._

/**
 * An analysis that identifies (non-static) inner classes that are serializable, but where the outer class
 * is not.
 *
 * @author Ralf Mitschke, original by Michael Eichberg
 */

trait SE_BAD_FIELD_INNER_CLASS {
   this: performancetests.opaltests.FBAnalysesBase =>

  import ivm.expressiontree.Exp

  private val serializable = ObjectType("java/io/Serializable")

  private def hasStaticModifier(modifiers : Int) : Boolean = de.tud.cs.st.bat.ACC_STATIC.element_of(modifiers)

  private def hasStaticModifier(modifiers : Exp[Int]) : Exp[Boolean] = {
    //TODO how do I define bitwise and operation? "&"
    //modifiers & de.tud.cs.st.bat.ACC_STATIC.mask) != 0
    import Lifting._
    import BATLifting._
    de.tud.cs.st.bat.ACC_STATIC.element_of(modifiers)
  }

  private def analyzeBaseWithoutAbstractions() = {
    val serializableClasses = classHierarchy.subclasses(serializable).getOrElse(Set.empty)
    for(  objectType ← serializableClasses;
          classFile = getClassFile(objectType);
          (outerType, thisInnerClassesAccessFlags) ← classFile.outerType
          if !hasStaticModifier(thisInnerClassesAccessFlags) &&
             !classHierarchy.isSubtypeOf(outerType, serializable).getOrElse(true /* if we don't know anything about the class, then we don't want to generate a warning */)
    ) yield {
      (objectType, outerType)
    }
  }



  private def analyzeSQuOptWithoutAbstractions() = {
    import Lifting._
    import BATLifting._
    import performancetests.opaltests.InstructionLifting._
    import ivm.expressiontree.Util.ExtraImplicits._
    val serializableClasses = classHierarchySQuOpt.subclasses(serializable).getOrElse(Set.empty)
    for(  objectType ← serializableClasses;
          classFile ←Let( getClassFileSQuOpt(objectType));
           outerClassEntry ← classFile.outerType
           if !hasStaticModifier(outerClassEntry._2) &&
              !classHierarchySQuOpt.isSubtypeOf(outerClassEntry._1, serializable).getOrElse(true /* if we don't know anything about the class, then we don't want to generate a warning */)
    ) yield {
      (objectType, outerClassEntry._1)
    }
  }

  private def analyzeBaseWithAbstractions() = {
    val serializableClasses = classHierarchy.subclasses(serializable).getOrElse(Set.empty)
    for(  objectType ← serializableClasses;
          classFile = getClassFile(objectType);
          (outerType, thisInnerClassesAccessFlags) ← classFile.outerType
          if !hasStaticModifier(thisInnerClassesAccessFlags) &&
             !classHierarchy.isSubtypeOf(outerType, serializable).getOrElse(true /* if we don't know anything about the class, then we don't want to generate a warning */)
    ) yield {
      (objectType, outerType)
    }
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
    val serializableClasses = classHierarchySQuOpt.subclasses(serializable).getOrElse(Set.empty)
    for(  objectType ← serializableClasses;
          classFile ←Let( getClassFileSQuOpt(objectType));
           outerClassEntry ← classFile.outerType
           if !hasStaticModifier(outerClassEntry._2) &&
              !classHierarchySQuOpt.isSubtypeOf(outerClassEntry._1, serializable).getOrElse(true /* if we don't know anything about the class, then we don't want to generate a warning */)
    ) yield {
      (objectType, outerClassEntry._1)
    }
  }


  def analyzeSE_BAD_FIELD_INNER_CLASS() {
    benchQueryComplete("SE_BAD_FIELD_INNER_CLASS")(
      analyzeBaseWithoutAbstractions(),
      analyzeBaseWithAbstractions()
      )(
      analyzeSQuOptWithoutAbstractions(),
      analyzeSQuOptWithAbstractions()
    )
  }
}
