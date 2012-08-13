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

/**
 * An analysis that identifies (non-static) inner classes that are serializable, but where the outer class
 * is not.
 *
 * @author Ralf Mitschke, original by Michael Eichberg
 */

trait SE_BAD_FIELD_INNER_CLASS {
   this: performancetests.opaltests.FBAnalysesBase =>

  import BaseAnalyses._

  import de.tud.cs.st.bat.ACC_STATIC

  val serializable = ObjectType("java/io/Serializable")


  def analyzeBaseWithoutAbstractions() = {
    for {
      objectTypes ← classHierarchy.subclasses(serializable).toSeq
      objectType ← objectTypes
      classFile = getClassFile(objectType)
      (outerType, thisInnerClassesAccessFlags) ← classFile.outerType if !ACC_STATIC.element_of(thisInnerClassesAccessFlags)
      if !classHierarchy.isSubtypeOf(outerType, serializable).getOrElse(true /* if we don't know anything about the class, then we don't want to generate a warning */)
    } yield {
      (objectType, outerType)
    }
  }

/*
  def analyzeSQuOptWithoutAbstractions() = {
    import ivm._
    import expressiontree._
    import Lifting._
    import BATLifting._
    import InstructionLifting._  //TODO import not found (???)
    import ivm.expressiontree.Util.ExtraImplicits._
    for {
      objectTypes ← classHierarchy.asSmart.subclasses(serializable).toSeq //TODO toSeq not found
      objectType ← objectTypes
      classFile = getClassFile(objectType)
      (outerType, thisInnerClassesAccessFlags) ← classFile.outerType if !ACC_STATIC.element_of(thisInnerClassesAccessFlags)
      if !classHierarchy.asSmart.isSubtypeOf(outerType, serializable).getOrElse(true /* if we don't know anything about the class, then we don't want to generate a warning */)
    } yield {
      (objectType, outerType)
    }
  }
*/
}
