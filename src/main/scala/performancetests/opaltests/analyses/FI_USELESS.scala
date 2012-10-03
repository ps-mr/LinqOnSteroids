/* License (BSD Style License):
*  Copyright (c) 2009, 2012
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
 * Finalize just calls super.finalize.
 *
 * @author Ralf Mitschke, original by Michael Eichberg
 */
trait FI_USELESS {
  this: performancetests.opaltests.FBAnalysesBase =>

  private val finalizeMethodDescriptor = MethodDescriptor(Seq(), VoidType)

  private def analyzeBaseWithoutAbstractions() = {
      for { classFile ← classFiles
            if !classFile.isInterfaceDeclaration // performance optimization
            method@Method(_, "finalize",`finalizeMethodDescriptor`, _) ← classFile.methods
            if method.body.isDefined &&
               method.body.get.instructions.length == 5 &&
               method.body.get.instructions.exists(
            {
              case INVOKESPECIAL(_, "finalize", `finalizeMethodDescriptor`) ⇒ true;
              case _                                                ⇒ false
            }                            )
      } yield (classFile, method)
  }

   private def analyzeSQuOptWithoutAbstractions() = {
      import ivm._
      import expressiontree._
      import Lifting._
      import BATLifting._
      import performancetests.opaltests.InstructionLifting._
      import ivm.expressiontree.Util.ExtraImplicits._
      for { classFile ← classFiles.asSquopt
            if !classFile.isInterfaceDeclaration // performance optimization
            method ← classFile.methods
            if method.name ==# "finalize" && method.descriptor ==# finalizeMethodDescriptor
            body <- method.body
            if body.instructions.length ==# 5 &&
               body.instructions.exists(instruction => (instruction.ifInstanceOf[INVOKESPECIAL] map { invoke =>
                 invoke.name ==# "finalize" && invoke.methodDescriptor ==# finalizeMethodDescriptor
               }).foldLeft(false){ pair => pair._1 || pair._2 })
      } yield (classFile, method)
  }

    private def analyzeBaseWithAbstractions() = {
      for { schema.ConcreteMethodRecord(classFile, method@Method(_, "finalize",`finalizeMethodDescriptor`, _), body) ← methodBodiesModularNative
            if !classFile.isInterfaceDeclaration && // performance optimization
               body.instructions.length == 5 &&
               body.instructions.exists(
                  {
                    case INVOKESPECIAL(_, "finalize", `finalizeMethodDescriptor`) ⇒ true;
                    case _                                                ⇒ false
                  }                            )
      } yield (classFile, method)
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
      for {
            concreteMethodRecord ← methodBodiesModularSQuOpt
            if !concreteMethodRecord.classFile.isInterfaceDeclaration && // performance optimization
               concreteMethodRecord.method.name ==# "finalize" && concreteMethodRecord.method.descriptor ==# finalizeMethodDescriptor &&
               concreteMethodRecord.body.instructions.length ==# 5 &&
               concreteMethodRecord.body.instructions.exists(instruction => (instruction.ifInstanceOf[INVOKESPECIAL] map { invoke =>
                 invoke.name ==# "finalize" && invoke.methodDescriptor ==# finalizeMethodDescriptor
               }).foldLeft(false){ pair => pair._1 || pair._2 })
      } yield (concreteMethodRecord.classFile, concreteMethodRecord.method)
    }

    def analyzeFI_USELESS() {
      benchQueryComplete("FI_USELESS")(
                                       analyzeBaseWithoutAbstractions(),
                                       analyzeBaseWithAbstractions()
                                      )(
                                       analyzeSQuOptWithoutAbstractions(),
                                       analyzeSQuOptWithAbstractions()
                                      )
    }
}
