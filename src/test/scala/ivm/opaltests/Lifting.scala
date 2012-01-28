/*
--------------------------------------------------------------------------

		THIS FILE IS AUTO GENERATED - DO NOT CHANGE MANUALLY!
		Generated:  2011-12-20T01:59:31.498+01:00
		Source File: GenerateInstructionClasses.xsl

--------------------------------------------------------------------------

 License (BSD Style License):
 Copyright (c) 2009, 2011
 Software Technology Group
 Department of Computer Science
 Technische Universität Darmstadt
 All rights reserved.

 Redistribution and use in source and binary forms, with or without
 modification, are permitted provided that the following conditions are met:

 - Redistributions of source code must retain the above copyright notice,
   this list of conditions and the following disclaimer.
 - Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.
 - Neither the name of the Software Technology Group or Technische
   Universität Darmstadt nor the names of its contributors may be used to
   endorse or promote products derived from this software without specific
   prior written permission.

 THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
 LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
 CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 POSSIBILITY OF SUCH DAMAGE.
*/

package ivm.opaltests

import ivm.expressiontree.Exp
import ivm.expressiontree.Lifting.onExp
import de.tud.cs.st.bat.resolved._

object InstructionLifting {
	implicit def expToAALOAD_Ops(t: Exp[AALOAD.type]) = new AALOAD_Ops(t)
	class AALOAD_Ops(t: Exp[AALOAD.type]) {
		def opcode = onExp(t)('opcode, _.opcode)
		def mnemonic = onExp(t)('mnemonic, _.mnemonic)
		def exceptions = onExp(t)('exceptions, _.exceptions)
	}

	implicit def expToAASTORE_Ops(t: Exp[AASTORE.type]) = new AASTORE_Ops(t)
	class AASTORE_Ops(t: Exp[AASTORE.type]) {
		def opcode = onExp(t)('opcode, _.opcode)
		def mnemonic = onExp(t)('mnemonic, _.mnemonic)
		def exceptions = onExp(t)('exceptions, _.exceptions)
	}

	implicit def expToACONST_NULL_Ops(t: Exp[ACONST_NULL.type]) = new ACONST_NULL_Ops(t)
	class ACONST_NULL_Ops(t: Exp[ACONST_NULL.type]) {
		def opcode = onExp(t)('opcode, _.opcode)
		def mnemonic = onExp(t)('mnemonic, _.mnemonic)
		def exceptions = onExp(t)('exceptions, _.exceptions)
	}

	implicit def expToALOAD_Ops(t: Exp[ALOAD]) = new ALOAD_Ops(t)
	class ALOAD_Ops(t: Exp[ALOAD]) {
		def opcode = onExp(t)('opcode, _.opcode)
		def mnemonic = onExp(t)('mnemonic, _.mnemonic)
		def exceptions = onExp(t)('exceptions, _.exceptions)
		def lvIndex = onExp(t)('lvIndex, _.lvIndex)
	}

	implicit def expToALOAD_0_Ops(t: Exp[ALOAD_0.type]) = new ALOAD_0_Ops(t)
	class ALOAD_0_Ops(t: Exp[ALOAD_0.type]) {
		def opcode = onExp(t)('opcode, _.opcode)
		def mnemonic = onExp(t)('mnemonic, _.mnemonic)
		def exceptions = onExp(t)('exceptions, _.exceptions)
	}

	implicit def expToALOAD_1_Ops(t: Exp[ALOAD_1.type]) = new ALOAD_1_Ops(t)
	class ALOAD_1_Ops(t: Exp[ALOAD_1.type]) {
		def opcode = onExp(t)('opcode, _.opcode)
		def mnemonic = onExp(t)('mnemonic, _.mnemonic)
		def exceptions = onExp(t)('exceptions, _.exceptions)
	}

	implicit def expToALOAD_2_Ops(t: Exp[ALOAD_2.type]) = new ALOAD_2_Ops(t)
	class ALOAD_2_Ops(t: Exp[ALOAD_2.type]) {
		def opcode = onExp(t)('opcode, _.opcode)
		def mnemonic = onExp(t)('mnemonic, _.mnemonic)
		def exceptions = onExp(t)('exceptions, _.exceptions)
	}

	implicit def expToALOAD_3_Ops(t: Exp[ALOAD_3.type]) = new ALOAD_3_Ops(t)
	class ALOAD_3_Ops(t: Exp[ALOAD_3.type]) {
		def opcode = onExp(t)('opcode, _.opcode)
		def mnemonic = onExp(t)('mnemonic, _.mnemonic)
		def exceptions = onExp(t)('exceptions, _.exceptions)
	}

	implicit def expToANEWARRAY_Ops(t: Exp[ANEWARRAY]) = new ANEWARRAY_Ops(t)
	class ANEWARRAY_Ops(t: Exp[ANEWARRAY]) {
		def opcode = onExp(t)('opcode, _.opcode)
		def mnemonic = onExp(t)('mnemonic, _.mnemonic)
		def exceptions = onExp(t)('exceptions, _.exceptions)
		def componentType = onExp(t)('componentType, _.componentType)
	}

	implicit def expToARETURN_Ops(t: Exp[ARETURN.type]) = new ARETURN_Ops(t)
	class ARETURN_Ops(t: Exp[ARETURN.type]) {
		def opcode = onExp(t)('opcode, _.opcode)
		def mnemonic = onExp(t)('mnemonic, _.mnemonic)
		def exceptions = onExp(t)('exceptions, _.exceptions)
	}

	implicit def expToARRAYLENGTH_Ops(t: Exp[ARRAYLENGTH.type]) = new ARRAYLENGTH_Ops(t)
	class ARRAYLENGTH_Ops(t: Exp[ARRAYLENGTH.type]) {
		def opcode = onExp(t)('opcode, _.opcode)
		def mnemonic = onExp(t)('mnemonic, _.mnemonic)
		def exceptions = onExp(t)('exceptions, _.exceptions)
	}

	implicit def expToASTORE_Ops(t: Exp[ASTORE]) = new ASTORE_Ops(t)
	class ASTORE_Ops(t: Exp[ASTORE]) {
		def opcode = onExp(t)('opcode, _.opcode)
		def mnemonic = onExp(t)('mnemonic, _.mnemonic)
		def exceptions = onExp(t)('exceptions, _.exceptions)
		def lvIndex = onExp(t)('lvIndex, _.lvIndex)
	}

	implicit def expToASTORE_0_Ops(t: Exp[ASTORE_0.type]) = new ASTORE_0_Ops(t)
	class ASTORE_0_Ops(t: Exp[ASTORE_0.type]) {
		def opcode = onExp(t)('opcode, _.opcode)
		def mnemonic = onExp(t)('mnemonic, _.mnemonic)
		def exceptions = onExp(t)('exceptions, _.exceptions)
	}

	implicit def expToASTORE_1_Ops(t: Exp[ASTORE_1.type]) = new ASTORE_1_Ops(t)
	class ASTORE_1_Ops(t: Exp[ASTORE_1.type]) {
		def opcode = onExp(t)('opcode, _.opcode)
		def mnemonic = onExp(t)('mnemonic, _.mnemonic)
		def exceptions = onExp(t)('exceptions, _.exceptions)
	}

	implicit def expToASTORE_2_Ops(t: Exp[ASTORE_2.type]) = new ASTORE_2_Ops(t)
	class ASTORE_2_Ops(t: Exp[ASTORE_2.type]) {
		def opcode = onExp(t)('opcode, _.opcode)
		def mnemonic = onExp(t)('mnemonic, _.mnemonic)
		def exceptions = onExp(t)('exceptions, _.exceptions)
	}

	implicit def expToASTORE_3_Ops(t: Exp[ASTORE_3.type]) = new ASTORE_3_Ops(t)
	class ASTORE_3_Ops(t: Exp[ASTORE_3.type]) {
		def opcode = onExp(t)('opcode, _.opcode)
		def mnemonic = onExp(t)('mnemonic, _.mnemonic)
		def exceptions = onExp(t)('exceptions, _.exceptions)
	}

	implicit def expToATHROW_Ops(t: Exp[ATHROW.type]) = new ATHROW_Ops(t)
	class ATHROW_Ops(t: Exp[ATHROW.type]) {
		def opcode = onExp(t)('opcode, _.opcode)
		def mnemonic = onExp(t)('mnemonic, _.mnemonic)
		def exceptions = onExp(t)('exceptions, _.exceptions)
	}

	implicit def expToBALOAD_Ops(t: Exp[BALOAD.type]) = new BALOAD_Ops(t)
	class BALOAD_Ops(t: Exp[BALOAD.type]) {
		def opcode = onExp(t)('opcode, _.opcode)
		def mnemonic = onExp(t)('mnemonic, _.mnemonic)
		def exceptions = onExp(t)('exceptions, _.exceptions)
	}

	implicit def expToBASTORE_Ops(t: Exp[BASTORE.type]) = new BASTORE_Ops(t)
	class BASTORE_Ops(t: Exp[BASTORE.type]) {
		def opcode = onExp(t)('opcode, _.opcode)
		def mnemonic = onExp(t)('mnemonic, _.mnemonic)
		def exceptions = onExp(t)('exceptions, _.exceptions)
	}

	implicit def expToBIPUSH_Ops(t: Exp[BIPUSH]) = new BIPUSH_Ops(t)
	class BIPUSH_Ops(t: Exp[BIPUSH]) {
		def opcode = onExp(t)('opcode, _.opcode)
		def mnemonic = onExp(t)('mnemonic, _.mnemonic)
		def exceptions = onExp(t)('exceptions, _.exceptions)
		def value = onExp(t)('value, _.value)
	}

	implicit def expToCALOAD_Ops(t: Exp[CALOAD.type]) = new CALOAD_Ops(t)
	class CALOAD_Ops(t: Exp[CALOAD.type]) {
		def opcode = onExp(t)('opcode, _.opcode)
		def mnemonic = onExp(t)('mnemonic, _.mnemonic)
		def exceptions = onExp(t)('exceptions, _.exceptions)
	}

	implicit def expToCASTORE_Ops(t: Exp[CASTORE.type]) = new CASTORE_Ops(t)
	class CASTORE_Ops(t: Exp[CASTORE.type]) {
		def opcode = onExp(t)('opcode, _.opcode)
		def mnemonic = onExp(t)('mnemonic, _.mnemonic)
		def exceptions = onExp(t)('exceptions, _.exceptions)
	}

	implicit def expToCHECKCAST_Ops(t: Exp[CHECKCAST]) = new CHECKCAST_Ops(t)
	class CHECKCAST_Ops(t: Exp[CHECKCAST]) {
		def opcode = onExp(t)('opcode, _.opcode)
		def mnemonic = onExp(t)('mnemonic, _.mnemonic)
		def exceptions = onExp(t)('exceptions, _.exceptions)
		def referenceType = onExp(t)('referenceType, _.referenceType)
	}

	implicit def expToD2F_Ops(t: Exp[D2F.type]) = new D2F_Ops(t)
	class D2F_Ops(t: Exp[D2F.type]) {
		def opcode = onExp(t)('opcode, _.opcode)
		def mnemonic = onExp(t)('mnemonic, _.mnemonic)
		def exceptions = onExp(t)('exceptions, _.exceptions)
	}

	implicit def expToD2I_Ops(t: Exp[D2I.type]) = new D2I_Ops(t)
	class D2I_Ops(t: Exp[D2I.type]) {
		def opcode = onExp(t)('opcode, _.opcode)
		def mnemonic = onExp(t)('mnemonic, _.mnemonic)
		def exceptions = onExp(t)('exceptions, _.exceptions)
	}

	implicit def expToD2L_Ops(t: Exp[D2L.type]) = new D2L_Ops(t)
	class D2L_Ops(t: Exp[D2L.type]) {
		def opcode = onExp(t)('opcode, _.opcode)
		def mnemonic = onExp(t)('mnemonic, _.mnemonic)
		def exceptions = onExp(t)('exceptions, _.exceptions)
	}

	implicit def expToDADD_Ops(t: Exp[DADD.type]) = new DADD_Ops(t)
	class DADD_Ops(t: Exp[DADD.type]) {
		def opcode = onExp(t)('opcode, _.opcode)
		def mnemonic = onExp(t)('mnemonic, _.mnemonic)
		def exceptions = onExp(t)('exceptions, _.exceptions)
	}

	implicit def expToDALOAD_Ops(t: Exp[DALOAD.type]) = new DALOAD_Ops(t)
	class DALOAD_Ops(t: Exp[DALOAD.type]) {
		def opcode = onExp(t)('opcode, _.opcode)
		def mnemonic = onExp(t)('mnemonic, _.mnemonic)
		def exceptions = onExp(t)('exceptions, _.exceptions)
	}

	implicit def expToDASTORE_Ops(t: Exp[DASTORE.type]) = new DASTORE_Ops(t)
	class DASTORE_Ops(t: Exp[DASTORE.type]) {
		def opcode = onExp(t)('opcode, _.opcode)
		def mnemonic = onExp(t)('mnemonic, _.mnemonic)
		def exceptions = onExp(t)('exceptions, _.exceptions)
	}

	implicit def expToDCMPG_Ops(t: Exp[DCMPG.type]) = new DCMPG_Ops(t)
	class DCMPG_Ops(t: Exp[DCMPG.type]) {
		def opcode = onExp(t)('opcode, _.opcode)
		def mnemonic = onExp(t)('mnemonic, _.mnemonic)
		def exceptions = onExp(t)('exceptions, _.exceptions)
	}

	implicit def expToDCMPL_Ops(t: Exp[DCMPL.type]) = new DCMPL_Ops(t)
	class DCMPL_Ops(t: Exp[DCMPL.type]) {
		def opcode = onExp(t)('opcode, _.opcode)
		def mnemonic = onExp(t)('mnemonic, _.mnemonic)
		def exceptions = onExp(t)('exceptions, _.exceptions)
	}

	implicit def expToDCONST_0_Ops(t: Exp[DCONST_0.type]) = new DCONST_0_Ops(t)
	class DCONST_0_Ops(t: Exp[DCONST_0.type]) {
		def opcode = onExp(t)('opcode, _.opcode)
		def mnemonic = onExp(t)('mnemonic, _.mnemonic)
		def exceptions = onExp(t)('exceptions, _.exceptions)
	}

	implicit def expToDCONST_1_Ops(t: Exp[DCONST_1.type]) = new DCONST_1_Ops(t)
	class DCONST_1_Ops(t: Exp[DCONST_1.type]) {
		def opcode = onExp(t)('opcode, _.opcode)
		def mnemonic = onExp(t)('mnemonic, _.mnemonic)
		def exceptions = onExp(t)('exceptions, _.exceptions)
	}

	implicit def expToDDIV_Ops(t: Exp[DDIV.type]) = new DDIV_Ops(t)
	class DDIV_Ops(t: Exp[DDIV.type]) {
		def opcode = onExp(t)('opcode, _.opcode)
		def mnemonic = onExp(t)('mnemonic, _.mnemonic)
		def exceptions = onExp(t)('exceptions, _.exceptions)
	}

	implicit def expToDLOAD_Ops(t: Exp[DLOAD]) = new DLOAD_Ops(t)
	class DLOAD_Ops(t: Exp[DLOAD]) {
		def opcode = onExp(t)('opcode, _.opcode)
		def mnemonic = onExp(t)('mnemonic, _.mnemonic)
		def exceptions = onExp(t)('exceptions, _.exceptions)
		def lvIndex = onExp(t)('lvIndex, _.lvIndex)
	}

	implicit def expToDLOAD_0_Ops(t: Exp[DLOAD_0.type]) = new DLOAD_0_Ops(t)
	class DLOAD_0_Ops(t: Exp[DLOAD_0.type]) {
		def opcode = onExp(t)('opcode, _.opcode)
		def mnemonic = onExp(t)('mnemonic, _.mnemonic)
		def exceptions = onExp(t)('exceptions, _.exceptions)
	}

	implicit def expToDLOAD_1_Ops(t: Exp[DLOAD_1.type]) = new DLOAD_1_Ops(t)
	class DLOAD_1_Ops(t: Exp[DLOAD_1.type]) {
		def opcode = onExp(t)('opcode, _.opcode)
		def mnemonic = onExp(t)('mnemonic, _.mnemonic)
		def exceptions = onExp(t)('exceptions, _.exceptions)
	}

	implicit def expToDLOAD_2_Ops(t: Exp[DLOAD_2.type]) = new DLOAD_2_Ops(t)
	class DLOAD_2_Ops(t: Exp[DLOAD_2.type]) {
		def opcode = onExp(t)('opcode, _.opcode)
		def mnemonic = onExp(t)('mnemonic, _.mnemonic)
		def exceptions = onExp(t)('exceptions, _.exceptions)
	}

	implicit def expToDLOAD_3_Ops(t: Exp[DLOAD_3.type]) = new DLOAD_3_Ops(t)
	class DLOAD_3_Ops(t: Exp[DLOAD_3.type]) {
		def opcode = onExp(t)('opcode, _.opcode)
		def mnemonic = onExp(t)('mnemonic, _.mnemonic)
		def exceptions = onExp(t)('exceptions, _.exceptions)
	}

	implicit def expToDMUL_Ops(t: Exp[DMUL.type]) = new DMUL_Ops(t)
	class DMUL_Ops(t: Exp[DMUL.type]) {
		def opcode = onExp(t)('opcode, _.opcode)
		def mnemonic = onExp(t)('mnemonic, _.mnemonic)
		def exceptions = onExp(t)('exceptions, _.exceptions)
	}

	implicit def expToDNEG_Ops(t: Exp[DNEG.type]) = new DNEG_Ops(t)
	class DNEG_Ops(t: Exp[DNEG.type]) {
		def opcode = onExp(t)('opcode, _.opcode)
		def mnemonic = onExp(t)('mnemonic, _.mnemonic)
		def exceptions = onExp(t)('exceptions, _.exceptions)
	}

	implicit def expToDREM_Ops(t: Exp[DREM.type]) = new DREM_Ops(t)
	class DREM_Ops(t: Exp[DREM.type]) {
		def opcode = onExp(t)('opcode, _.opcode)
		def mnemonic = onExp(t)('mnemonic, _.mnemonic)
		def exceptions = onExp(t)('exceptions, _.exceptions)
	}

	implicit def expToDRETURN_Ops(t: Exp[DRETURN.type]) = new DRETURN_Ops(t)
	class DRETURN_Ops(t: Exp[DRETURN.type]) {
		def opcode = onExp(t)('opcode, _.opcode)
		def mnemonic = onExp(t)('mnemonic, _.mnemonic)
		def exceptions = onExp(t)('exceptions, _.exceptions)
	}

	implicit def expToDSTORE_Ops(t: Exp[DSTORE]) = new DSTORE_Ops(t)
	class DSTORE_Ops(t: Exp[DSTORE]) {
		def opcode = onExp(t)('opcode, _.opcode)
		def mnemonic = onExp(t)('mnemonic, _.mnemonic)
		def exceptions = onExp(t)('exceptions, _.exceptions)
		def lvIndex = onExp(t)('lvIndex, _.lvIndex)
	}

	implicit def expToDSTORE_0_Ops(t: Exp[DSTORE_0.type]) = new DSTORE_0_Ops(t)
	class DSTORE_0_Ops(t: Exp[DSTORE_0.type]) {
		def opcode = onExp(t)('opcode, _.opcode)
		def mnemonic = onExp(t)('mnemonic, _.mnemonic)
		def exceptions = onExp(t)('exceptions, _.exceptions)
	}

	implicit def expToDSTORE_1_Ops(t: Exp[DSTORE_1.type]) = new DSTORE_1_Ops(t)
	class DSTORE_1_Ops(t: Exp[DSTORE_1.type]) {
		def opcode = onExp(t)('opcode, _.opcode)
		def mnemonic = onExp(t)('mnemonic, _.mnemonic)
		def exceptions = onExp(t)('exceptions, _.exceptions)
	}

	implicit def expToDSTORE_2_Ops(t: Exp[DSTORE_2.type]) = new DSTORE_2_Ops(t)
	class DSTORE_2_Ops(t: Exp[DSTORE_2.type]) {
		def opcode = onExp(t)('opcode, _.opcode)
		def mnemonic = onExp(t)('mnemonic, _.mnemonic)
		def exceptions = onExp(t)('exceptions, _.exceptions)
	}

	implicit def expToDSTORE_3_Ops(t: Exp[DSTORE_3.type]) = new DSTORE_3_Ops(t)
	class DSTORE_3_Ops(t: Exp[DSTORE_3.type]) {
		def opcode = onExp(t)('opcode, _.opcode)
		def mnemonic = onExp(t)('mnemonic, _.mnemonic)
		def exceptions = onExp(t)('exceptions, _.exceptions)
	}

	implicit def expToDSUB_Ops(t: Exp[DSUB.type]) = new DSUB_Ops(t)
	class DSUB_Ops(t: Exp[DSUB.type]) {
		def opcode = onExp(t)('opcode, _.opcode)
		def mnemonic = onExp(t)('mnemonic, _.mnemonic)
		def exceptions = onExp(t)('exceptions, _.exceptions)
	}

	implicit def expToDUP_Ops(t: Exp[DUP.type]) = new DUP_Ops(t)
	class DUP_Ops(t: Exp[DUP.type]) {
		def opcode = onExp(t)('opcode, _.opcode)
		def mnemonic = onExp(t)('mnemonic, _.mnemonic)
		def exceptions = onExp(t)('exceptions, _.exceptions)
	}

	implicit def expToDUP_X1_Ops(t: Exp[DUP_X1.type]) = new DUP_X1_Ops(t)
	class DUP_X1_Ops(t: Exp[DUP_X1.type]) {
		def opcode = onExp(t)('opcode, _.opcode)
		def mnemonic = onExp(t)('mnemonic, _.mnemonic)
		def exceptions = onExp(t)('exceptions, _.exceptions)
	}

	implicit def expToDUP_X2_Ops(t: Exp[DUP_X2.type]) = new DUP_X2_Ops(t)
	class DUP_X2_Ops(t: Exp[DUP_X2.type]) {
		def opcode = onExp(t)('opcode, _.opcode)
		def mnemonic = onExp(t)('mnemonic, _.mnemonic)
		def exceptions = onExp(t)('exceptions, _.exceptions)
	}

	implicit def expToDUP2_Ops(t: Exp[DUP2.type]) = new DUP2_Ops(t)
	class DUP2_Ops(t: Exp[DUP2.type]) {
		def opcode = onExp(t)('opcode, _.opcode)
		def mnemonic = onExp(t)('mnemonic, _.mnemonic)
		def exceptions = onExp(t)('exceptions, _.exceptions)
	}

	implicit def expToDUP2_X1_Ops(t: Exp[DUP2_X1.type]) = new DUP2_X1_Ops(t)
	class DUP2_X1_Ops(t: Exp[DUP2_X1.type]) {
		def opcode = onExp(t)('opcode, _.opcode)
		def mnemonic = onExp(t)('mnemonic, _.mnemonic)
		def exceptions = onExp(t)('exceptions, _.exceptions)
	}

	implicit def expToDUP2_X2_Ops(t: Exp[DUP2_X2.type]) = new DUP2_X2_Ops(t)
	class DUP2_X2_Ops(t: Exp[DUP2_X2.type]) {
		def opcode = onExp(t)('opcode, _.opcode)
		def mnemonic = onExp(t)('mnemonic, _.mnemonic)
		def exceptions = onExp(t)('exceptions, _.exceptions)
	}

	implicit def expToF2D_Ops(t: Exp[F2D.type]) = new F2D_Ops(t)
	class F2D_Ops(t: Exp[F2D.type]) {
		def opcode = onExp(t)('opcode, _.opcode)
		def mnemonic = onExp(t)('mnemonic, _.mnemonic)
		def exceptions = onExp(t)('exceptions, _.exceptions)
	}

	implicit def expToF2I_Ops(t: Exp[F2I.type]) = new F2I_Ops(t)
	class F2I_Ops(t: Exp[F2I.type]) {
		def opcode = onExp(t)('opcode, _.opcode)
		def mnemonic = onExp(t)('mnemonic, _.mnemonic)
		def exceptions = onExp(t)('exceptions, _.exceptions)
	}

	implicit def expToF2L_Ops(t: Exp[F2L.type]) = new F2L_Ops(t)
	class F2L_Ops(t: Exp[F2L.type]) {
		def opcode = onExp(t)('opcode, _.opcode)
		def mnemonic = onExp(t)('mnemonic, _.mnemonic)
		def exceptions = onExp(t)('exceptions, _.exceptions)
	}

	implicit def expToFADD_Ops(t: Exp[FADD.type]) = new FADD_Ops(t)
	class FADD_Ops(t: Exp[FADD.type]) {
		def opcode = onExp(t)('opcode, _.opcode)
		def mnemonic = onExp(t)('mnemonic, _.mnemonic)
		def exceptions = onExp(t)('exceptions, _.exceptions)
	}

	implicit def expToFALOAD_Ops(t: Exp[FALOAD.type]) = new FALOAD_Ops(t)
	class FALOAD_Ops(t: Exp[FALOAD.type]) {
		def opcode = onExp(t)('opcode, _.opcode)
		def mnemonic = onExp(t)('mnemonic, _.mnemonic)
		def exceptions = onExp(t)('exceptions, _.exceptions)
	}

	implicit def expToFASTORE_Ops(t: Exp[FASTORE.type]) = new FASTORE_Ops(t)
	class FASTORE_Ops(t: Exp[FASTORE.type]) {
		def opcode = onExp(t)('opcode, _.opcode)
		def mnemonic = onExp(t)('mnemonic, _.mnemonic)
		def exceptions = onExp(t)('exceptions, _.exceptions)
	}

	implicit def expToFCMPG_Ops(t: Exp[FCMPG.type]) = new FCMPG_Ops(t)
	class FCMPG_Ops(t: Exp[FCMPG.type]) {
		def opcode = onExp(t)('opcode, _.opcode)
		def mnemonic = onExp(t)('mnemonic, _.mnemonic)
		def exceptions = onExp(t)('exceptions, _.exceptions)
	}

	implicit def expToFCMPL_Ops(t: Exp[FCMPL.type]) = new FCMPL_Ops(t)
	class FCMPL_Ops(t: Exp[FCMPL.type]) {
		def opcode = onExp(t)('opcode, _.opcode)
		def mnemonic = onExp(t)('mnemonic, _.mnemonic)
		def exceptions = onExp(t)('exceptions, _.exceptions)
	}

	implicit def expToFCONST_0_Ops(t: Exp[FCONST_0.type]) = new FCONST_0_Ops(t)
	class FCONST_0_Ops(t: Exp[FCONST_0.type]) {
		def opcode = onExp(t)('opcode, _.opcode)
		def mnemonic = onExp(t)('mnemonic, _.mnemonic)
		def exceptions = onExp(t)('exceptions, _.exceptions)
	}

	implicit def expToFCONST_1_Ops(t: Exp[FCONST_1.type]) = new FCONST_1_Ops(t)
	class FCONST_1_Ops(t: Exp[FCONST_1.type]) {
		def opcode = onExp(t)('opcode, _.opcode)
		def mnemonic = onExp(t)('mnemonic, _.mnemonic)
		def exceptions = onExp(t)('exceptions, _.exceptions)
	}

	implicit def expToFCONST_2_Ops(t: Exp[FCONST_2.type]) = new FCONST_2_Ops(t)
	class FCONST_2_Ops(t: Exp[FCONST_2.type]) {
		def opcode = onExp(t)('opcode, _.opcode)
		def mnemonic = onExp(t)('mnemonic, _.mnemonic)
		def exceptions = onExp(t)('exceptions, _.exceptions)
	}

	implicit def expToFDIV_Ops(t: Exp[FDIV.type]) = new FDIV_Ops(t)
	class FDIV_Ops(t: Exp[FDIV.type]) {
		def opcode = onExp(t)('opcode, _.opcode)
		def mnemonic = onExp(t)('mnemonic, _.mnemonic)
		def exceptions = onExp(t)('exceptions, _.exceptions)
	}

	implicit def expToFLOAD_Ops(t: Exp[FLOAD]) = new FLOAD_Ops(t)
	class FLOAD_Ops(t: Exp[FLOAD]) {
		def opcode = onExp(t)('opcode, _.opcode)
		def mnemonic = onExp(t)('mnemonic, _.mnemonic)
		def exceptions = onExp(t)('exceptions, _.exceptions)
		def lvIndex = onExp(t)('lvIndex, _.lvIndex)
	}

	implicit def expToFLOAD_0_Ops(t: Exp[FLOAD_0.type]) = new FLOAD_0_Ops(t)
	class FLOAD_0_Ops(t: Exp[FLOAD_0.type]) {
		def opcode = onExp(t)('opcode, _.opcode)
		def mnemonic = onExp(t)('mnemonic, _.mnemonic)
		def exceptions = onExp(t)('exceptions, _.exceptions)
	}

	implicit def expToFLOAD_1_Ops(t: Exp[FLOAD_1.type]) = new FLOAD_1_Ops(t)
	class FLOAD_1_Ops(t: Exp[FLOAD_1.type]) {
		def opcode = onExp(t)('opcode, _.opcode)
		def mnemonic = onExp(t)('mnemonic, _.mnemonic)
		def exceptions = onExp(t)('exceptions, _.exceptions)
	}

	implicit def expToFLOAD_2_Ops(t: Exp[FLOAD_2.type]) = new FLOAD_2_Ops(t)
	class FLOAD_2_Ops(t: Exp[FLOAD_2.type]) {
		def opcode = onExp(t)('opcode, _.opcode)
		def mnemonic = onExp(t)('mnemonic, _.mnemonic)
		def exceptions = onExp(t)('exceptions, _.exceptions)
	}

	implicit def expToFLOAD_3_Ops(t: Exp[FLOAD_3.type]) = new FLOAD_3_Ops(t)
	class FLOAD_3_Ops(t: Exp[FLOAD_3.type]) {
		def opcode = onExp(t)('opcode, _.opcode)
		def mnemonic = onExp(t)('mnemonic, _.mnemonic)
		def exceptions = onExp(t)('exceptions, _.exceptions)
	}

	implicit def expToFMUL_Ops(t: Exp[FMUL.type]) = new FMUL_Ops(t)
	class FMUL_Ops(t: Exp[FMUL.type]) {
		def opcode = onExp(t)('opcode, _.opcode)
		def mnemonic = onExp(t)('mnemonic, _.mnemonic)
		def exceptions = onExp(t)('exceptions, _.exceptions)
	}

	implicit def expToFNEG_Ops(t: Exp[FNEG.type]) = new FNEG_Ops(t)
	class FNEG_Ops(t: Exp[FNEG.type]) {
		def opcode = onExp(t)('opcode, _.opcode)
		def mnemonic = onExp(t)('mnemonic, _.mnemonic)
		def exceptions = onExp(t)('exceptions, _.exceptions)
	}

	implicit def expToFREM_Ops(t: Exp[FREM.type]) = new FREM_Ops(t)
	class FREM_Ops(t: Exp[FREM.type]) {
		def opcode = onExp(t)('opcode, _.opcode)
		def mnemonic = onExp(t)('mnemonic, _.mnemonic)
		def exceptions = onExp(t)('exceptions, _.exceptions)
	}

	implicit def expToFRETURN_Ops(t: Exp[FRETURN.type]) = new FRETURN_Ops(t)
	class FRETURN_Ops(t: Exp[FRETURN.type]) {
		def opcode = onExp(t)('opcode, _.opcode)
		def mnemonic = onExp(t)('mnemonic, _.mnemonic)
		def exceptions = onExp(t)('exceptions, _.exceptions)
	}

	implicit def expToFSTORE_Ops(t: Exp[FSTORE]) = new FSTORE_Ops(t)
	class FSTORE_Ops(t: Exp[FSTORE]) {
		def opcode = onExp(t)('opcode, _.opcode)
		def mnemonic = onExp(t)('mnemonic, _.mnemonic)
		def exceptions = onExp(t)('exceptions, _.exceptions)
		def lvIndex = onExp(t)('lvIndex, _.lvIndex)
	}

	implicit def expToFSTORE_0_Ops(t: Exp[FSTORE_0.type]) = new FSTORE_0_Ops(t)
	class FSTORE_0_Ops(t: Exp[FSTORE_0.type]) {
		def opcode = onExp(t)('opcode, _.opcode)
		def mnemonic = onExp(t)('mnemonic, _.mnemonic)
		def exceptions = onExp(t)('exceptions, _.exceptions)
	}

	implicit def expToFSTORE_1_Ops(t: Exp[FSTORE_1.type]) = new FSTORE_1_Ops(t)
	class FSTORE_1_Ops(t: Exp[FSTORE_1.type]) {
		def opcode = onExp(t)('opcode, _.opcode)
		def mnemonic = onExp(t)('mnemonic, _.mnemonic)
		def exceptions = onExp(t)('exceptions, _.exceptions)
	}

	implicit def expToFSTORE_2_Ops(t: Exp[FSTORE_2.type]) = new FSTORE_2_Ops(t)
	class FSTORE_2_Ops(t: Exp[FSTORE_2.type]) {
		def opcode = onExp(t)('opcode, _.opcode)
		def mnemonic = onExp(t)('mnemonic, _.mnemonic)
		def exceptions = onExp(t)('exceptions, _.exceptions)
	}

	implicit def expToFSTORE_3_Ops(t: Exp[FSTORE_3.type]) = new FSTORE_3_Ops(t)
	class FSTORE_3_Ops(t: Exp[FSTORE_3.type]) {
		def opcode = onExp(t)('opcode, _.opcode)
		def mnemonic = onExp(t)('mnemonic, _.mnemonic)
		def exceptions = onExp(t)('exceptions, _.exceptions)
	}

	implicit def expToFSUB_Ops(t: Exp[FSUB.type]) = new FSUB_Ops(t)
	class FSUB_Ops(t: Exp[FSUB.type]) {
		def opcode = onExp(t)('opcode, _.opcode)
		def mnemonic = onExp(t)('mnemonic, _.mnemonic)
		def exceptions = onExp(t)('exceptions, _.exceptions)
	}

	implicit def expToGETFIELD_Ops(t: Exp[GETFIELD]) = new GETFIELD_Ops(t)
	class GETFIELD_Ops(t: Exp[GETFIELD]) {
		def opcode = onExp(t)('opcode, _.opcode)
		def mnemonic = onExp(t)('mnemonic, _.mnemonic)
		def exceptions = onExp(t)('exceptions, _.exceptions)
		def declaringClass = onExp(t)('declaringClass, _.declaringClass)
		def name = onExp(t)('name, _.name)
		def fieldType = onExp(t)('fieldType, _.fieldType)
	}

	implicit def expToGETSTATIC_Ops(t: Exp[GETSTATIC]) = new GETSTATIC_Ops(t)
	class GETSTATIC_Ops(t: Exp[GETSTATIC]) {
		def opcode = onExp(t)('opcode, _.opcode)
		def mnemonic = onExp(t)('mnemonic, _.mnemonic)
		def exceptions = onExp(t)('exceptions, _.exceptions)
		def declaringClass = onExp(t)('declaringClass, _.declaringClass)
		def name = onExp(t)('name, _.name)
		def fieldType = onExp(t)('fieldType, _.fieldType)
	}

	implicit def expToGOTO_Ops(t: Exp[GOTO]) = new GOTO_Ops(t)
	class GOTO_Ops(t: Exp[GOTO]) {
		def opcode = onExp(t)('opcode, _.opcode)
		def mnemonic = onExp(t)('mnemonic, _.mnemonic)
		def exceptions = onExp(t)('exceptions, _.exceptions)
		def branchoffset = onExp(t)('branchoffset, _.branchoffset)
	}

	implicit def expToGOTO_W_Ops(t: Exp[GOTO_W]) = new GOTO_W_Ops(t)
	class GOTO_W_Ops(t: Exp[GOTO_W]) {
		def opcode = onExp(t)('opcode, _.opcode)
		def mnemonic = onExp(t)('mnemonic, _.mnemonic)
		def exceptions = onExp(t)('exceptions, _.exceptions)
		def branchoffset = onExp(t)('branchoffset, _.branchoffset)
	}

	implicit def expToI2B_Ops(t: Exp[I2B.type]) = new I2B_Ops(t)
	class I2B_Ops(t: Exp[I2B.type]) {
		def opcode = onExp(t)('opcode, _.opcode)
		def mnemonic = onExp(t)('mnemonic, _.mnemonic)
		def exceptions = onExp(t)('exceptions, _.exceptions)
	}

	implicit def expToI2C_Ops(t: Exp[I2C.type]) = new I2C_Ops(t)
	class I2C_Ops(t: Exp[I2C.type]) {
		def opcode = onExp(t)('opcode, _.opcode)
		def mnemonic = onExp(t)('mnemonic, _.mnemonic)
		def exceptions = onExp(t)('exceptions, _.exceptions)
	}

	implicit def expToI2D_Ops(t: Exp[I2D.type]) = new I2D_Ops(t)
	class I2D_Ops(t: Exp[I2D.type]) {
		def opcode = onExp(t)('opcode, _.opcode)
		def mnemonic = onExp(t)('mnemonic, _.mnemonic)
		def exceptions = onExp(t)('exceptions, _.exceptions)
	}

	implicit def expToI2F_Ops(t: Exp[I2F.type]) = new I2F_Ops(t)
	class I2F_Ops(t: Exp[I2F.type]) {
		def opcode = onExp(t)('opcode, _.opcode)
		def mnemonic = onExp(t)('mnemonic, _.mnemonic)
		def exceptions = onExp(t)('exceptions, _.exceptions)
	}

	implicit def expToI2L_Ops(t: Exp[I2L.type]) = new I2L_Ops(t)
	class I2L_Ops(t: Exp[I2L.type]) {
		def opcode = onExp(t)('opcode, _.opcode)
		def mnemonic = onExp(t)('mnemonic, _.mnemonic)
		def exceptions = onExp(t)('exceptions, _.exceptions)
	}

	implicit def expToI2S_Ops(t: Exp[I2S.type]) = new I2S_Ops(t)
	class I2S_Ops(t: Exp[I2S.type]) {
		def opcode = onExp(t)('opcode, _.opcode)
		def mnemonic = onExp(t)('mnemonic, _.mnemonic)
		def exceptions = onExp(t)('exceptions, _.exceptions)
	}

	implicit def expToIADD_Ops(t: Exp[IADD.type]) = new IADD_Ops(t)
	class IADD_Ops(t: Exp[IADD.type]) {
		def opcode = onExp(t)('opcode, _.opcode)
		def mnemonic = onExp(t)('mnemonic, _.mnemonic)
		def exceptions = onExp(t)('exceptions, _.exceptions)
	}

	implicit def expToIALOAD_Ops(t: Exp[IALOAD.type]) = new IALOAD_Ops(t)
	class IALOAD_Ops(t: Exp[IALOAD.type]) {
		def opcode = onExp(t)('opcode, _.opcode)
		def mnemonic = onExp(t)('mnemonic, _.mnemonic)
		def exceptions = onExp(t)('exceptions, _.exceptions)
	}

	implicit def expToIAND_Ops(t: Exp[IAND.type]) = new IAND_Ops(t)
	class IAND_Ops(t: Exp[IAND.type]) {
		def opcode = onExp(t)('opcode, _.opcode)
		def mnemonic = onExp(t)('mnemonic, _.mnemonic)
		def exceptions = onExp(t)('exceptions, _.exceptions)
	}

	implicit def expToIASTORE_Ops(t: Exp[IASTORE.type]) = new IASTORE_Ops(t)
	class IASTORE_Ops(t: Exp[IASTORE.type]) {
		def opcode = onExp(t)('opcode, _.opcode)
		def mnemonic = onExp(t)('mnemonic, _.mnemonic)
		def exceptions = onExp(t)('exceptions, _.exceptions)
	}

	implicit def expToICONST_M1_Ops(t: Exp[ICONST_M1.type]) = new ICONST_M1_Ops(t)
	class ICONST_M1_Ops(t: Exp[ICONST_M1.type]) {
		def opcode = onExp(t)('opcode, _.opcode)
		def mnemonic = onExp(t)('mnemonic, _.mnemonic)
		def exceptions = onExp(t)('exceptions, _.exceptions)
	}

	implicit def expToICONST_0_Ops(t: Exp[ICONST_0.type]) = new ICONST_0_Ops(t)
	class ICONST_0_Ops(t: Exp[ICONST_0.type]) {
		def opcode = onExp(t)('opcode, _.opcode)
		def mnemonic = onExp(t)('mnemonic, _.mnemonic)
		def exceptions = onExp(t)('exceptions, _.exceptions)
	}

	implicit def expToICONST_1_Ops(t: Exp[ICONST_1.type]) = new ICONST_1_Ops(t)
	class ICONST_1_Ops(t: Exp[ICONST_1.type]) {
		def opcode = onExp(t)('opcode, _.opcode)
		def mnemonic = onExp(t)('mnemonic, _.mnemonic)
		def exceptions = onExp(t)('exceptions, _.exceptions)
	}

	implicit def expToICONST_2_Ops(t: Exp[ICONST_2.type]) = new ICONST_2_Ops(t)
	class ICONST_2_Ops(t: Exp[ICONST_2.type]) {
		def opcode = onExp(t)('opcode, _.opcode)
		def mnemonic = onExp(t)('mnemonic, _.mnemonic)
		def exceptions = onExp(t)('exceptions, _.exceptions)
	}

	implicit def expToICONST_3_Ops(t: Exp[ICONST_3.type]) = new ICONST_3_Ops(t)
	class ICONST_3_Ops(t: Exp[ICONST_3.type]) {
		def opcode = onExp(t)('opcode, _.opcode)
		def mnemonic = onExp(t)('mnemonic, _.mnemonic)
		def exceptions = onExp(t)('exceptions, _.exceptions)
	}

	implicit def expToICONST_4_Ops(t: Exp[ICONST_4.type]) = new ICONST_4_Ops(t)
	class ICONST_4_Ops(t: Exp[ICONST_4.type]) {
		def opcode = onExp(t)('opcode, _.opcode)
		def mnemonic = onExp(t)('mnemonic, _.mnemonic)
		def exceptions = onExp(t)('exceptions, _.exceptions)
	}

	implicit def expToICONST_5_Ops(t: Exp[ICONST_5.type]) = new ICONST_5_Ops(t)
	class ICONST_5_Ops(t: Exp[ICONST_5.type]) {
		def opcode = onExp(t)('opcode, _.opcode)
		def mnemonic = onExp(t)('mnemonic, _.mnemonic)
		def exceptions = onExp(t)('exceptions, _.exceptions)
	}

	implicit def expToIDIV_Ops(t: Exp[IDIV.type]) = new IDIV_Ops(t)
	class IDIV_Ops(t: Exp[IDIV.type]) {
		def opcode = onExp(t)('opcode, _.opcode)
		def mnemonic = onExp(t)('mnemonic, _.mnemonic)
		def exceptions = onExp(t)('exceptions, _.exceptions)
	}

	implicit def expToIF_ACMPEQ_Ops(t: Exp[IF_ACMPEQ]) = new IF_ACMPEQ_Ops(t)
	class IF_ACMPEQ_Ops(t: Exp[IF_ACMPEQ]) {
		def opcode = onExp(t)('opcode, _.opcode)
		def mnemonic = onExp(t)('mnemonic, _.mnemonic)
		def exceptions = onExp(t)('exceptions, _.exceptions)
		def branchoffset = onExp(t)('branchoffset, _.branchoffset)
	}

	implicit def expToIF_ACMPNE_Ops(t: Exp[IF_ACMPNE]) = new IF_ACMPNE_Ops(t)
	class IF_ACMPNE_Ops(t: Exp[IF_ACMPNE]) {
		def opcode = onExp(t)('opcode, _.opcode)
		def mnemonic = onExp(t)('mnemonic, _.mnemonic)
		def exceptions = onExp(t)('exceptions, _.exceptions)
		def branchoffset = onExp(t)('branchoffset, _.branchoffset)
	}

	implicit def expToIF_ICMPEQ_Ops(t: Exp[IF_ICMPEQ]) = new IF_ICMPEQ_Ops(t)
	class IF_ICMPEQ_Ops(t: Exp[IF_ICMPEQ]) {
		def opcode = onExp(t)('opcode, _.opcode)
		def mnemonic = onExp(t)('mnemonic, _.mnemonic)
		def exceptions = onExp(t)('exceptions, _.exceptions)
		def branchoffset = onExp(t)('branchoffset, _.branchoffset)
	}

	implicit def expToIF_ICMPNE_Ops(t: Exp[IF_ICMPNE]) = new IF_ICMPNE_Ops(t)
	class IF_ICMPNE_Ops(t: Exp[IF_ICMPNE]) {
		def opcode = onExp(t)('opcode, _.opcode)
		def mnemonic = onExp(t)('mnemonic, _.mnemonic)
		def exceptions = onExp(t)('exceptions, _.exceptions)
		def branchoffset = onExp(t)('branchoffset, _.branchoffset)
	}

	implicit def expToIF_ICMPLT_Ops(t: Exp[IF_ICMPLT]) = new IF_ICMPLT_Ops(t)
	class IF_ICMPLT_Ops(t: Exp[IF_ICMPLT]) {
		def opcode = onExp(t)('opcode, _.opcode)
		def mnemonic = onExp(t)('mnemonic, _.mnemonic)
		def exceptions = onExp(t)('exceptions, _.exceptions)
		def branchoffset = onExp(t)('branchoffset, _.branchoffset)
	}

	implicit def expToIF_ICMPGE_Ops(t: Exp[IF_ICMPGE]) = new IF_ICMPGE_Ops(t)
	class IF_ICMPGE_Ops(t: Exp[IF_ICMPGE]) {
		def opcode = onExp(t)('opcode, _.opcode)
		def mnemonic = onExp(t)('mnemonic, _.mnemonic)
		def exceptions = onExp(t)('exceptions, _.exceptions)
		def branchoffset = onExp(t)('branchoffset, _.branchoffset)
	}

	implicit def expToIF_ICMPGT_Ops(t: Exp[IF_ICMPGT]) = new IF_ICMPGT_Ops(t)
	class IF_ICMPGT_Ops(t: Exp[IF_ICMPGT]) {
		def opcode = onExp(t)('opcode, _.opcode)
		def mnemonic = onExp(t)('mnemonic, _.mnemonic)
		def exceptions = onExp(t)('exceptions, _.exceptions)
		def branchoffset = onExp(t)('branchoffset, _.branchoffset)
	}

	implicit def expToIF_ICMPLE_Ops(t: Exp[IF_ICMPLE]) = new IF_ICMPLE_Ops(t)
	class IF_ICMPLE_Ops(t: Exp[IF_ICMPLE]) {
		def opcode = onExp(t)('opcode, _.opcode)
		def mnemonic = onExp(t)('mnemonic, _.mnemonic)
		def exceptions = onExp(t)('exceptions, _.exceptions)
		def branchoffset = onExp(t)('branchoffset, _.branchoffset)
	}

	implicit def expToIFEQ_Ops(t: Exp[IFEQ]) = new IFEQ_Ops(t)
	class IFEQ_Ops(t: Exp[IFEQ]) {
		def opcode = onExp(t)('opcode, _.opcode)
		def mnemonic = onExp(t)('mnemonic, _.mnemonic)
		def exceptions = onExp(t)('exceptions, _.exceptions)
		def branchoffset = onExp(t)('branchoffset, _.branchoffset)
	}

	implicit def expToIFNE_Ops(t: Exp[IFNE]) = new IFNE_Ops(t)
	class IFNE_Ops(t: Exp[IFNE]) {
		def opcode = onExp(t)('opcode, _.opcode)
		def mnemonic = onExp(t)('mnemonic, _.mnemonic)
		def exceptions = onExp(t)('exceptions, _.exceptions)
		def branchoffset = onExp(t)('branchoffset, _.branchoffset)
	}

	implicit def expToIFLT_Ops(t: Exp[IFLT]) = new IFLT_Ops(t)
	class IFLT_Ops(t: Exp[IFLT]) {
		def opcode = onExp(t)('opcode, _.opcode)
		def mnemonic = onExp(t)('mnemonic, _.mnemonic)
		def exceptions = onExp(t)('exceptions, _.exceptions)
		def branchoffset = onExp(t)('branchoffset, _.branchoffset)
	}

	implicit def expToIFGE_Ops(t: Exp[IFGE]) = new IFGE_Ops(t)
	class IFGE_Ops(t: Exp[IFGE]) {
		def opcode = onExp(t)('opcode, _.opcode)
		def mnemonic = onExp(t)('mnemonic, _.mnemonic)
		def exceptions = onExp(t)('exceptions, _.exceptions)
		def branchoffset = onExp(t)('branchoffset, _.branchoffset)
	}

	implicit def expToIFGT_Ops(t: Exp[IFGT]) = new IFGT_Ops(t)
	class IFGT_Ops(t: Exp[IFGT]) {
		def opcode = onExp(t)('opcode, _.opcode)
		def mnemonic = onExp(t)('mnemonic, _.mnemonic)
		def exceptions = onExp(t)('exceptions, _.exceptions)
		def branchoffset = onExp(t)('branchoffset, _.branchoffset)
	}

	implicit def expToIFLE_Ops(t: Exp[IFLE]) = new IFLE_Ops(t)
	class IFLE_Ops(t: Exp[IFLE]) {
		def opcode = onExp(t)('opcode, _.opcode)
		def mnemonic = onExp(t)('mnemonic, _.mnemonic)
		def exceptions = onExp(t)('exceptions, _.exceptions)
		def branchoffset = onExp(t)('branchoffset, _.branchoffset)
	}

	implicit def expToIFNONNULL_Ops(t: Exp[IFNONNULL]) = new IFNONNULL_Ops(t)
	class IFNONNULL_Ops(t: Exp[IFNONNULL]) {
		def opcode = onExp(t)('opcode, _.opcode)
		def mnemonic = onExp(t)('mnemonic, _.mnemonic)
		def exceptions = onExp(t)('exceptions, _.exceptions)
		def branchoffset = onExp(t)('branchoffset, _.branchoffset)
	}

	implicit def expToIFNULL_Ops(t: Exp[IFNULL]) = new IFNULL_Ops(t)
	class IFNULL_Ops(t: Exp[IFNULL]) {
		def opcode = onExp(t)('opcode, _.opcode)
		def mnemonic = onExp(t)('mnemonic, _.mnemonic)
		def exceptions = onExp(t)('exceptions, _.exceptions)
		def branchoffset = onExp(t)('branchoffset, _.branchoffset)
	}

	implicit def expToIINC_Ops(t: Exp[IINC]) = new IINC_Ops(t)
	class IINC_Ops(t: Exp[IINC]) {
		def opcode = onExp(t)('opcode, _.opcode)
		def mnemonic = onExp(t)('mnemonic, _.mnemonic)
		def exceptions = onExp(t)('exceptions, _.exceptions)
		def lvIndex = onExp(t)('lvIndex, _.lvIndex)
		def constValue = onExp(t)('constValue, _.constValue)
	}

	implicit def expToILOAD_Ops(t: Exp[ILOAD]) = new ILOAD_Ops(t)
	class ILOAD_Ops(t: Exp[ILOAD]) {
		def opcode = onExp(t)('opcode, _.opcode)
		def mnemonic = onExp(t)('mnemonic, _.mnemonic)
		def exceptions = onExp(t)('exceptions, _.exceptions)
		def lvIndex = onExp(t)('lvIndex, _.lvIndex)
	}

	implicit def expToILOAD_0_Ops(t: Exp[ILOAD_0.type]) = new ILOAD_0_Ops(t)
	class ILOAD_0_Ops(t: Exp[ILOAD_0.type]) {
		def opcode = onExp(t)('opcode, _.opcode)
		def mnemonic = onExp(t)('mnemonic, _.mnemonic)
		def exceptions = onExp(t)('exceptions, _.exceptions)
	}

	implicit def expToILOAD_1_Ops(t: Exp[ILOAD_1.type]) = new ILOAD_1_Ops(t)
	class ILOAD_1_Ops(t: Exp[ILOAD_1.type]) {
		def opcode = onExp(t)('opcode, _.opcode)
		def mnemonic = onExp(t)('mnemonic, _.mnemonic)
		def exceptions = onExp(t)('exceptions, _.exceptions)
	}

	implicit def expToILOAD_2_Ops(t: Exp[ILOAD_2.type]) = new ILOAD_2_Ops(t)
	class ILOAD_2_Ops(t: Exp[ILOAD_2.type]) {
		def opcode = onExp(t)('opcode, _.opcode)
		def mnemonic = onExp(t)('mnemonic, _.mnemonic)
		def exceptions = onExp(t)('exceptions, _.exceptions)
	}

	implicit def expToILOAD_3_Ops(t: Exp[ILOAD_3.type]) = new ILOAD_3_Ops(t)
	class ILOAD_3_Ops(t: Exp[ILOAD_3.type]) {
		def opcode = onExp(t)('opcode, _.opcode)
		def mnemonic = onExp(t)('mnemonic, _.mnemonic)
		def exceptions = onExp(t)('exceptions, _.exceptions)
	}

	implicit def expToIMUL_Ops(t: Exp[IMUL.type]) = new IMUL_Ops(t)
	class IMUL_Ops(t: Exp[IMUL.type]) {
		def opcode = onExp(t)('opcode, _.opcode)
		def mnemonic = onExp(t)('mnemonic, _.mnemonic)
		def exceptions = onExp(t)('exceptions, _.exceptions)
	}

	implicit def expToINEG_Ops(t: Exp[INEG.type]) = new INEG_Ops(t)
	class INEG_Ops(t: Exp[INEG.type]) {
		def opcode = onExp(t)('opcode, _.opcode)
		def mnemonic = onExp(t)('mnemonic, _.mnemonic)
		def exceptions = onExp(t)('exceptions, _.exceptions)
	}

	implicit def expToINSTANCEOF_Ops(t: Exp[INSTANCEOF]) = new INSTANCEOF_Ops(t)
	class INSTANCEOF_Ops(t: Exp[INSTANCEOF]) {
		def opcode = onExp(t)('opcode, _.opcode)
		def mnemonic = onExp(t)('mnemonic, _.mnemonic)
		def exceptions = onExp(t)('exceptions, _.exceptions)
		def referenceType = onExp(t)('referenceType, _.referenceType)
	}

	implicit def expToINVOKEDYNAMIC_Ops(t: Exp[INVOKEDYNAMIC]) = new INVOKEDYNAMIC_Ops(t)
	class INVOKEDYNAMIC_Ops(t: Exp[INVOKEDYNAMIC]) {
		def opcode = onExp(t)('opcode, _.opcode)
		def mnemonic = onExp(t)('mnemonic, _.mnemonic)
		def exceptions = onExp(t)('exceptions, _.exceptions)
		def name = onExp(t)('name, _.name)
		def methodDescriptor = onExp(t)('methodDescriptor, _.methodDescriptor)
	}

	implicit def expToINVOKEINTERFACE_Ops(t: Exp[INVOKEINTERFACE]) = new INVOKEINTERFACE_Ops(t)
	class INVOKEINTERFACE_Ops(t: Exp[INVOKEINTERFACE]) {
		def opcode = onExp(t)('opcode, _.opcode)
		def mnemonic = onExp(t)('mnemonic, _.mnemonic)
		def exceptions = onExp(t)('exceptions, _.exceptions)
		def declaringClass = onExp(t)('declaringClass, _.declaringClass)
		def name = onExp(t)('name, _.name)
		def methodDescriptor = onExp(t)('methodDescriptor, _.methodDescriptor)
	}

	implicit def expToINVOKESPECIAL_Ops(t: Exp[INVOKESPECIAL]) = new INVOKESPECIAL_Ops(t)
	class INVOKESPECIAL_Ops(t: Exp[INVOKESPECIAL]) {
		def opcode = onExp(t)('opcode, _.opcode)
		def mnemonic = onExp(t)('mnemonic, _.mnemonic)
		def exceptions = onExp(t)('exceptions, _.exceptions)
		def declaringClass = onExp(t)('declaringClass, _.declaringClass)
		def name = onExp(t)('name, _.name)
		def methodDescriptor = onExp(t)('methodDescriptor, _.methodDescriptor)
	}

	implicit def expToINVOKESTATIC_Ops(t: Exp[INVOKESTATIC]) = new INVOKESTATIC_Ops(t)
	class INVOKESTATIC_Ops(t: Exp[INVOKESTATIC]) {
		def opcode = onExp(t)('opcode, _.opcode)
		def mnemonic = onExp(t)('mnemonic, _.mnemonic)
		def exceptions = onExp(t)('exceptions, _.exceptions)
		def declaringClass = onExp(t)('declaringClass, _.declaringClass)
		def name = onExp(t)('name, _.name)
		def methodDescriptor = onExp(t)('methodDescriptor, _.methodDescriptor)
	}

	implicit def expToINVOKEVIRTUAL_Ops(t: Exp[INVOKEVIRTUAL]) = new INVOKEVIRTUAL_Ops(t)
	class INVOKEVIRTUAL_Ops(t: Exp[INVOKEVIRTUAL]) {
		def opcode = onExp(t)('opcode, _.opcode)
		def mnemonic = onExp(t)('mnemonic, _.mnemonic)
		def exceptions = onExp(t)('exceptions, _.exceptions)
		def declaringClass = onExp(t)('declaringClass, _.declaringClass)
		def name = onExp(t)('name, _.name)
		def methodDescriptor = onExp(t)('methodDescriptor, _.methodDescriptor)
	}

	implicit def expToIOR_Ops(t: Exp[IOR.type]) = new IOR_Ops(t)
	class IOR_Ops(t: Exp[IOR.type]) {
		def opcode = onExp(t)('opcode, _.opcode)
		def mnemonic = onExp(t)('mnemonic, _.mnemonic)
		def exceptions = onExp(t)('exceptions, _.exceptions)
	}

	implicit def expToIREM_Ops(t: Exp[IREM.type]) = new IREM_Ops(t)
	class IREM_Ops(t: Exp[IREM.type]) {
		def opcode = onExp(t)('opcode, _.opcode)
		def mnemonic = onExp(t)('mnemonic, _.mnemonic)
		def exceptions = onExp(t)('exceptions, _.exceptions)
	}

	implicit def expToIRETURN_Ops(t: Exp[IRETURN.type]) = new IRETURN_Ops(t)
	class IRETURN_Ops(t: Exp[IRETURN.type]) {
		def opcode = onExp(t)('opcode, _.opcode)
		def mnemonic = onExp(t)('mnemonic, _.mnemonic)
		def exceptions = onExp(t)('exceptions, _.exceptions)
	}

	implicit def expToISHL_Ops(t: Exp[ISHL.type]) = new ISHL_Ops(t)
	class ISHL_Ops(t: Exp[ISHL.type]) {
		def opcode = onExp(t)('opcode, _.opcode)
		def mnemonic = onExp(t)('mnemonic, _.mnemonic)
		def exceptions = onExp(t)('exceptions, _.exceptions)
	}

	implicit def expToISHR_Ops(t: Exp[ISHR.type]) = new ISHR_Ops(t)
	class ISHR_Ops(t: Exp[ISHR.type]) {
		def opcode = onExp(t)('opcode, _.opcode)
		def mnemonic = onExp(t)('mnemonic, _.mnemonic)
		def exceptions = onExp(t)('exceptions, _.exceptions)
	}

	implicit def expToISTORE_Ops(t: Exp[ISTORE]) = new ISTORE_Ops(t)
	class ISTORE_Ops(t: Exp[ISTORE]) {
		def opcode = onExp(t)('opcode, _.opcode)
		def mnemonic = onExp(t)('mnemonic, _.mnemonic)
		def exceptions = onExp(t)('exceptions, _.exceptions)
		def lvIndex = onExp(t)('lvIndex, _.lvIndex)
	}

	implicit def expToISTORE_0_Ops(t: Exp[ISTORE_0.type]) = new ISTORE_0_Ops(t)
	class ISTORE_0_Ops(t: Exp[ISTORE_0.type]) {
		def opcode = onExp(t)('opcode, _.opcode)
		def mnemonic = onExp(t)('mnemonic, _.mnemonic)
		def exceptions = onExp(t)('exceptions, _.exceptions)
	}

	implicit def expToISTORE_1_Ops(t: Exp[ISTORE_1.type]) = new ISTORE_1_Ops(t)
	class ISTORE_1_Ops(t: Exp[ISTORE_1.type]) {
		def opcode = onExp(t)('opcode, _.opcode)
		def mnemonic = onExp(t)('mnemonic, _.mnemonic)
		def exceptions = onExp(t)('exceptions, _.exceptions)
	}

	implicit def expToISTORE_2_Ops(t: Exp[ISTORE_2.type]) = new ISTORE_2_Ops(t)
	class ISTORE_2_Ops(t: Exp[ISTORE_2.type]) {
		def opcode = onExp(t)('opcode, _.opcode)
		def mnemonic = onExp(t)('mnemonic, _.mnemonic)
		def exceptions = onExp(t)('exceptions, _.exceptions)
	}

	implicit def expToISTORE_3_Ops(t: Exp[ISTORE_3.type]) = new ISTORE_3_Ops(t)
	class ISTORE_3_Ops(t: Exp[ISTORE_3.type]) {
		def opcode = onExp(t)('opcode, _.opcode)
		def mnemonic = onExp(t)('mnemonic, _.mnemonic)
		def exceptions = onExp(t)('exceptions, _.exceptions)
	}

	implicit def expToISUB_Ops(t: Exp[ISUB.type]) = new ISUB_Ops(t)
	class ISUB_Ops(t: Exp[ISUB.type]) {
		def opcode = onExp(t)('opcode, _.opcode)
		def mnemonic = onExp(t)('mnemonic, _.mnemonic)
		def exceptions = onExp(t)('exceptions, _.exceptions)
	}

	implicit def expToIUSHR_Ops(t: Exp[IUSHR.type]) = new IUSHR_Ops(t)
	class IUSHR_Ops(t: Exp[IUSHR.type]) {
		def opcode = onExp(t)('opcode, _.opcode)
		def mnemonic = onExp(t)('mnemonic, _.mnemonic)
		def exceptions = onExp(t)('exceptions, _.exceptions)
	}

	implicit def expToIXOR_Ops(t: Exp[IXOR.type]) = new IXOR_Ops(t)
	class IXOR_Ops(t: Exp[IXOR.type]) {
		def opcode = onExp(t)('opcode, _.opcode)
		def mnemonic = onExp(t)('mnemonic, _.mnemonic)
		def exceptions = onExp(t)('exceptions, _.exceptions)
	}

	implicit def expToJSR_Ops(t: Exp[JSR]) = new JSR_Ops(t)
	class JSR_Ops(t: Exp[JSR]) {
		def opcode = onExp(t)('opcode, _.opcode)
		def mnemonic = onExp(t)('mnemonic, _.mnemonic)
		def exceptions = onExp(t)('exceptions, _.exceptions)
		def branchoffset = onExp(t)('branchoffset, _.branchoffset)
	}

	implicit def expToJSR_W_Ops(t: Exp[JSR_W]) = new JSR_W_Ops(t)
	class JSR_W_Ops(t: Exp[JSR_W]) {
		def opcode = onExp(t)('opcode, _.opcode)
		def mnemonic = onExp(t)('mnemonic, _.mnemonic)
		def exceptions = onExp(t)('exceptions, _.exceptions)
		def branchoffset = onExp(t)('branchoffset, _.branchoffset)
	}

	implicit def expToL2D_Ops(t: Exp[L2D.type]) = new L2D_Ops(t)
	class L2D_Ops(t: Exp[L2D.type]) {
		def opcode = onExp(t)('opcode, _.opcode)
		def mnemonic = onExp(t)('mnemonic, _.mnemonic)
		def exceptions = onExp(t)('exceptions, _.exceptions)
	}

	implicit def expToL2F_Ops(t: Exp[L2F.type]) = new L2F_Ops(t)
	class L2F_Ops(t: Exp[L2F.type]) {
		def opcode = onExp(t)('opcode, _.opcode)
		def mnemonic = onExp(t)('mnemonic, _.mnemonic)
		def exceptions = onExp(t)('exceptions, _.exceptions)
	}

	implicit def expToL2I_Ops(t: Exp[L2I.type]) = new L2I_Ops(t)
	class L2I_Ops(t: Exp[L2I.type]) {
		def opcode = onExp(t)('opcode, _.opcode)
		def mnemonic = onExp(t)('mnemonic, _.mnemonic)
		def exceptions = onExp(t)('exceptions, _.exceptions)
	}

	implicit def expToLADD_Ops(t: Exp[LADD.type]) = new LADD_Ops(t)
	class LADD_Ops(t: Exp[LADD.type]) {
		def opcode = onExp(t)('opcode, _.opcode)
		def mnemonic = onExp(t)('mnemonic, _.mnemonic)
		def exceptions = onExp(t)('exceptions, _.exceptions)
	}

	implicit def expToLALOAD_Ops(t: Exp[LALOAD.type]) = new LALOAD_Ops(t)
	class LALOAD_Ops(t: Exp[LALOAD.type]) {
		def opcode = onExp(t)('opcode, _.opcode)
		def mnemonic = onExp(t)('mnemonic, _.mnemonic)
		def exceptions = onExp(t)('exceptions, _.exceptions)
	}

	implicit def expToLAND_Ops(t: Exp[LAND.type]) = new LAND_Ops(t)
	class LAND_Ops(t: Exp[LAND.type]) {
		def opcode = onExp(t)('opcode, _.opcode)
		def mnemonic = onExp(t)('mnemonic, _.mnemonic)
		def exceptions = onExp(t)('exceptions, _.exceptions)
	}

	implicit def expToLASTORE_Ops(t: Exp[LASTORE.type]) = new LASTORE_Ops(t)
	class LASTORE_Ops(t: Exp[LASTORE.type]) {
		def opcode = onExp(t)('opcode, _.opcode)
		def mnemonic = onExp(t)('mnemonic, _.mnemonic)
		def exceptions = onExp(t)('exceptions, _.exceptions)
	}

	implicit def expToLCMP_Ops(t: Exp[LCMP.type]) = new LCMP_Ops(t)
	class LCMP_Ops(t: Exp[LCMP.type]) {
		def opcode = onExp(t)('opcode, _.opcode)
		def mnemonic = onExp(t)('mnemonic, _.mnemonic)
		def exceptions = onExp(t)('exceptions, _.exceptions)
	}

	implicit def expToLCONST_0_Ops(t: Exp[LCONST_0.type]) = new LCONST_0_Ops(t)
	class LCONST_0_Ops(t: Exp[LCONST_0.type]) {
		def opcode = onExp(t)('opcode, _.opcode)
		def mnemonic = onExp(t)('mnemonic, _.mnemonic)
		def exceptions = onExp(t)('exceptions, _.exceptions)
	}

	implicit def expToLCONST_1_Ops(t: Exp[LCONST_1.type]) = new LCONST_1_Ops(t)
	class LCONST_1_Ops(t: Exp[LCONST_1.type]) {
		def opcode = onExp(t)('opcode, _.opcode)
		def mnemonic = onExp(t)('mnemonic, _.mnemonic)
		def exceptions = onExp(t)('exceptions, _.exceptions)
	}

	implicit def expToLDC_Ops(t: Exp[LDC]) = new LDC_Ops(t)
	class LDC_Ops(t: Exp[LDC]) {
		def opcode = onExp(t)('opcode, _.opcode)
		def mnemonic = onExp(t)('mnemonic, _.mnemonic)
		def exceptions = onExp(t)('exceptions, _.exceptions)
		def constantValue = onExp(t)('constantValue, _.constantValue)
	}

	implicit def expToLDC_W_Ops(t: Exp[LDC_W]) = new LDC_W_Ops(t)
	class LDC_W_Ops(t: Exp[LDC_W]) {
		def opcode = onExp(t)('opcode, _.opcode)
		def mnemonic = onExp(t)('mnemonic, _.mnemonic)
		def exceptions = onExp(t)('exceptions, _.exceptions)
		def constantValue = onExp(t)('constantValue, _.constantValue)
	}

	implicit def expToLDC2_W_Ops(t: Exp[LDC2_W]) = new LDC2_W_Ops(t)
	class LDC2_W_Ops(t: Exp[LDC2_W]) {
		def opcode = onExp(t)('opcode, _.opcode)
		def mnemonic = onExp(t)('mnemonic, _.mnemonic)
		def exceptions = onExp(t)('exceptions, _.exceptions)
		def constantValue = onExp(t)('constantValue, _.constantValue)
	}

	implicit def expToLDIV_Ops(t: Exp[LDIV.type]) = new LDIV_Ops(t)
	class LDIV_Ops(t: Exp[LDIV.type]) {
		def opcode = onExp(t)('opcode, _.opcode)
		def mnemonic = onExp(t)('mnemonic, _.mnemonic)
		def exceptions = onExp(t)('exceptions, _.exceptions)
	}

	implicit def expToLLOAD_Ops(t: Exp[LLOAD]) = new LLOAD_Ops(t)
	class LLOAD_Ops(t: Exp[LLOAD]) {
		def opcode = onExp(t)('opcode, _.opcode)
		def mnemonic = onExp(t)('mnemonic, _.mnemonic)
		def exceptions = onExp(t)('exceptions, _.exceptions)
		def lvIndex = onExp(t)('lvIndex, _.lvIndex)
	}

	implicit def expToLLOAD_0_Ops(t: Exp[LLOAD_0.type]) = new LLOAD_0_Ops(t)
	class LLOAD_0_Ops(t: Exp[LLOAD_0.type]) {
		def opcode = onExp(t)('opcode, _.opcode)
		def mnemonic = onExp(t)('mnemonic, _.mnemonic)
		def exceptions = onExp(t)('exceptions, _.exceptions)
	}

	implicit def expToLLOAD_1_Ops(t: Exp[LLOAD_1.type]) = new LLOAD_1_Ops(t)
	class LLOAD_1_Ops(t: Exp[LLOAD_1.type]) {
		def opcode = onExp(t)('opcode, _.opcode)
		def mnemonic = onExp(t)('mnemonic, _.mnemonic)
		def exceptions = onExp(t)('exceptions, _.exceptions)
	}

	implicit def expToLLOAD_2_Ops(t: Exp[LLOAD_2.type]) = new LLOAD_2_Ops(t)
	class LLOAD_2_Ops(t: Exp[LLOAD_2.type]) {
		def opcode = onExp(t)('opcode, _.opcode)
		def mnemonic = onExp(t)('mnemonic, _.mnemonic)
		def exceptions = onExp(t)('exceptions, _.exceptions)
	}

	implicit def expToLLOAD_3_Ops(t: Exp[LLOAD_3.type]) = new LLOAD_3_Ops(t)
	class LLOAD_3_Ops(t: Exp[LLOAD_3.type]) {
		def opcode = onExp(t)('opcode, _.opcode)
		def mnemonic = onExp(t)('mnemonic, _.mnemonic)
		def exceptions = onExp(t)('exceptions, _.exceptions)
	}

	implicit def expToLMUL_Ops(t: Exp[LMUL.type]) = new LMUL_Ops(t)
	class LMUL_Ops(t: Exp[LMUL.type]) {
		def opcode = onExp(t)('opcode, _.opcode)
		def mnemonic = onExp(t)('mnemonic, _.mnemonic)
		def exceptions = onExp(t)('exceptions, _.exceptions)
	}

	implicit def expToLNEG_Ops(t: Exp[LNEG.type]) = new LNEG_Ops(t)
	class LNEG_Ops(t: Exp[LNEG.type]) {
		def opcode = onExp(t)('opcode, _.opcode)
		def mnemonic = onExp(t)('mnemonic, _.mnemonic)
		def exceptions = onExp(t)('exceptions, _.exceptions)
	}

	implicit def expToLOOKUPSWITCH_Ops(t: Exp[LOOKUPSWITCH]) = new LOOKUPSWITCH_Ops(t)
	class LOOKUPSWITCH_Ops(t: Exp[LOOKUPSWITCH]) {
		def opcode = onExp(t)('opcode, _.opcode)
		def mnemonic = onExp(t)('mnemonic, _.mnemonic)
		def exceptions = onExp(t)('exceptions, _.exceptions)
		def defaultOffset = onExp(t)('defaultOffset, _.defaultOffset)
		def npairsCount = onExp(t)('npairsCount, _.npairsCount)
		def npairs = onExp(t)('npairs, _.npairs)
	}

	implicit def expToLOR_Ops(t: Exp[LOR.type]) = new LOR_Ops(t)
	class LOR_Ops(t: Exp[LOR.type]) {
		def opcode = onExp(t)('opcode, _.opcode)
		def mnemonic = onExp(t)('mnemonic, _.mnemonic)
		def exceptions = onExp(t)('exceptions, _.exceptions)
	}

	implicit def expToLREM_Ops(t: Exp[LREM.type]) = new LREM_Ops(t)
	class LREM_Ops(t: Exp[LREM.type]) {
		def opcode = onExp(t)('opcode, _.opcode)
		def mnemonic = onExp(t)('mnemonic, _.mnemonic)
		def exceptions = onExp(t)('exceptions, _.exceptions)
	}

	implicit def expToLRETURN_Ops(t: Exp[LRETURN.type]) = new LRETURN_Ops(t)
	class LRETURN_Ops(t: Exp[LRETURN.type]) {
		def opcode = onExp(t)('opcode, _.opcode)
		def mnemonic = onExp(t)('mnemonic, _.mnemonic)
		def exceptions = onExp(t)('exceptions, _.exceptions)
	}

	implicit def expToLSHL_Ops(t: Exp[LSHL.type]) = new LSHL_Ops(t)
	class LSHL_Ops(t: Exp[LSHL.type]) {
		def opcode = onExp(t)('opcode, _.opcode)
		def mnemonic = onExp(t)('mnemonic, _.mnemonic)
		def exceptions = onExp(t)('exceptions, _.exceptions)
	}

	implicit def expToLSHR_Ops(t: Exp[LSHR.type]) = new LSHR_Ops(t)
	class LSHR_Ops(t: Exp[LSHR.type]) {
		def opcode = onExp(t)('opcode, _.opcode)
		def mnemonic = onExp(t)('mnemonic, _.mnemonic)
		def exceptions = onExp(t)('exceptions, _.exceptions)
	}

	implicit def expToLSTORE_Ops(t: Exp[LSTORE]) = new LSTORE_Ops(t)
	class LSTORE_Ops(t: Exp[LSTORE]) {
		def opcode = onExp(t)('opcode, _.opcode)
		def mnemonic = onExp(t)('mnemonic, _.mnemonic)
		def exceptions = onExp(t)('exceptions, _.exceptions)
		def lvIndex = onExp(t)('lvIndex, _.lvIndex)
	}

	implicit def expToLSTORE_0_Ops(t: Exp[LSTORE_0.type]) = new LSTORE_0_Ops(t)
	class LSTORE_0_Ops(t: Exp[LSTORE_0.type]) {
		def opcode = onExp(t)('opcode, _.opcode)
		def mnemonic = onExp(t)('mnemonic, _.mnemonic)
		def exceptions = onExp(t)('exceptions, _.exceptions)
	}

	implicit def expToLSTORE_1_Ops(t: Exp[LSTORE_1.type]) = new LSTORE_1_Ops(t)
	class LSTORE_1_Ops(t: Exp[LSTORE_1.type]) {
		def opcode = onExp(t)('opcode, _.opcode)
		def mnemonic = onExp(t)('mnemonic, _.mnemonic)
		def exceptions = onExp(t)('exceptions, _.exceptions)
	}

	implicit def expToLSTORE_2_Ops(t: Exp[LSTORE_2.type]) = new LSTORE_2_Ops(t)
	class LSTORE_2_Ops(t: Exp[LSTORE_2.type]) {
		def opcode = onExp(t)('opcode, _.opcode)
		def mnemonic = onExp(t)('mnemonic, _.mnemonic)
		def exceptions = onExp(t)('exceptions, _.exceptions)
	}

	implicit def expToLSTORE_3_Ops(t: Exp[LSTORE_3.type]) = new LSTORE_3_Ops(t)
	class LSTORE_3_Ops(t: Exp[LSTORE_3.type]) {
		def opcode = onExp(t)('opcode, _.opcode)
		def mnemonic = onExp(t)('mnemonic, _.mnemonic)
		def exceptions = onExp(t)('exceptions, _.exceptions)
	}

	implicit def expToLSUB_Ops(t: Exp[LSUB.type]) = new LSUB_Ops(t)
	class LSUB_Ops(t: Exp[LSUB.type]) {
		def opcode = onExp(t)('opcode, _.opcode)
		def mnemonic = onExp(t)('mnemonic, _.mnemonic)
		def exceptions = onExp(t)('exceptions, _.exceptions)
	}

	implicit def expToLUSHR_Ops(t: Exp[LUSHR.type]) = new LUSHR_Ops(t)
	class LUSHR_Ops(t: Exp[LUSHR.type]) {
		def opcode = onExp(t)('opcode, _.opcode)
		def mnemonic = onExp(t)('mnemonic, _.mnemonic)
		def exceptions = onExp(t)('exceptions, _.exceptions)
	}

	implicit def expToLXOR_Ops(t: Exp[LXOR.type]) = new LXOR_Ops(t)
	class LXOR_Ops(t: Exp[LXOR.type]) {
		def opcode = onExp(t)('opcode, _.opcode)
		def mnemonic = onExp(t)('mnemonic, _.mnemonic)
		def exceptions = onExp(t)('exceptions, _.exceptions)
	}

	implicit def expToMONITORENTER_Ops(t: Exp[MONITORENTER.type]) = new MONITORENTER_Ops(t)
	class MONITORENTER_Ops(t: Exp[MONITORENTER.type]) {
		def opcode = onExp(t)('opcode, _.opcode)
		def mnemonic = onExp(t)('mnemonic, _.mnemonic)
		def exceptions = onExp(t)('exceptions, _.exceptions)
	}

	implicit def expToMONITOREXIT_Ops(t: Exp[MONITOREXIT.type]) = new MONITOREXIT_Ops(t)
	class MONITOREXIT_Ops(t: Exp[MONITOREXIT.type]) {
		def opcode = onExp(t)('opcode, _.opcode)
		def mnemonic = onExp(t)('mnemonic, _.mnemonic)
		def exceptions = onExp(t)('exceptions, _.exceptions)
	}

	implicit def expToMULTIANEWARRAY_Ops(t: Exp[MULTIANEWARRAY]) = new MULTIANEWARRAY_Ops(t)
	class MULTIANEWARRAY_Ops(t: Exp[MULTIANEWARRAY]) {
		def opcode = onExp(t)('opcode, _.opcode)
		def mnemonic = onExp(t)('mnemonic, _.mnemonic)
		def exceptions = onExp(t)('exceptions, _.exceptions)
		def componentType = onExp(t)('componentType, _.componentType)
		def dimensions = onExp(t)('dimensions, _.dimensions)
	}

	implicit def expToNEW_Ops(t: Exp[NEW]) = new NEW_Ops(t)
	class NEW_Ops(t: Exp[NEW]) {
		def opcode = onExp(t)('opcode, _.opcode)
		def mnemonic = onExp(t)('mnemonic, _.mnemonic)
		def exceptions = onExp(t)('exceptions, _.exceptions)
		def objectType = onExp(t)('objectType, _.objectType)
	}

	implicit def expToNEWARRAY_Ops(t: Exp[NEWARRAY]) = new NEWARRAY_Ops(t)
	class NEWARRAY_Ops(t: Exp[NEWARRAY]) {
		def opcode = onExp(t)('opcode, _.opcode)
		def mnemonic = onExp(t)('mnemonic, _.mnemonic)
		def exceptions = onExp(t)('exceptions, _.exceptions)
		def atype = onExp(t)('atype, _.atype)
	}

	implicit def expToNOP_Ops(t: Exp[NOP.type]) = new NOP_Ops(t)
	class NOP_Ops(t: Exp[NOP.type]) {
		def opcode = onExp(t)('opcode, _.opcode)
		def mnemonic = onExp(t)('mnemonic, _.mnemonic)
		def exceptions = onExp(t)('exceptions, _.exceptions)
	}

	implicit def expToPOP_Ops(t: Exp[POP.type]) = new POP_Ops(t)
	class POP_Ops(t: Exp[POP.type]) {
		def opcode = onExp(t)('opcode, _.opcode)
		def mnemonic = onExp(t)('mnemonic, _.mnemonic)
		def exceptions = onExp(t)('exceptions, _.exceptions)
	}

	implicit def expToPOP2_Ops(t: Exp[POP2.type]) = new POP2_Ops(t)
	class POP2_Ops(t: Exp[POP2.type]) {
		def opcode = onExp(t)('opcode, _.opcode)
		def mnemonic = onExp(t)('mnemonic, _.mnemonic)
		def exceptions = onExp(t)('exceptions, _.exceptions)
	}

	implicit def expToPUTFIELD_Ops(t: Exp[PUTFIELD]) = new PUTFIELD_Ops(t)
	class PUTFIELD_Ops(t: Exp[PUTFIELD]) {
		def opcode = onExp(t)('opcode, _.opcode)
		def mnemonic = onExp(t)('mnemonic, _.mnemonic)
		def exceptions = onExp(t)('exceptions, _.exceptions)
		def declaringClass = onExp(t)('declaringClass, _.declaringClass)
		def name = onExp(t)('name, _.name)
		def fieldType = onExp(t)('fieldType, _.fieldType)
	}

	implicit def expToPUTSTATIC_Ops(t: Exp[PUTSTATIC]) = new PUTSTATIC_Ops(t)
	class PUTSTATIC_Ops(t: Exp[PUTSTATIC]) {
		def opcode = onExp(t)('opcode, _.opcode)
		def mnemonic = onExp(t)('mnemonic, _.mnemonic)
		def exceptions = onExp(t)('exceptions, _.exceptions)
		def declaringClass = onExp(t)('declaringClass, _.declaringClass)
		def name = onExp(t)('name, _.name)
		def fieldType = onExp(t)('fieldType, _.fieldType)
	}

	implicit def expToRET_Ops(t: Exp[RET]) = new RET_Ops(t)
	class RET_Ops(t: Exp[RET]) {
		def opcode = onExp(t)('opcode, _.opcode)
		def mnemonic = onExp(t)('mnemonic, _.mnemonic)
		def exceptions = onExp(t)('exceptions, _.exceptions)
		def lvIndex = onExp(t)('lvIndex, _.lvIndex)
	}

	implicit def expToRETURN_Ops(t: Exp[RETURN.type]) = new RETURN_Ops(t)
	class RETURN_Ops(t: Exp[RETURN.type]) {
		def opcode = onExp(t)('opcode, _.opcode)
		def mnemonic = onExp(t)('mnemonic, _.mnemonic)
		def exceptions = onExp(t)('exceptions, _.exceptions)
	}

	implicit def expToSALOAD_Ops(t: Exp[SALOAD.type]) = new SALOAD_Ops(t)
	class SALOAD_Ops(t: Exp[SALOAD.type]) {
		def opcode = onExp(t)('opcode, _.opcode)
		def mnemonic = onExp(t)('mnemonic, _.mnemonic)
		def exceptions = onExp(t)('exceptions, _.exceptions)
	}

	implicit def expToSASTORE_Ops(t: Exp[SASTORE.type]) = new SASTORE_Ops(t)
	class SASTORE_Ops(t: Exp[SASTORE.type]) {
		def opcode = onExp(t)('opcode, _.opcode)
		def mnemonic = onExp(t)('mnemonic, _.mnemonic)
		def exceptions = onExp(t)('exceptions, _.exceptions)
	}

	implicit def expToSIPUSH_Ops(t: Exp[SIPUSH]) = new SIPUSH_Ops(t)
	class SIPUSH_Ops(t: Exp[SIPUSH]) {
		def opcode = onExp(t)('opcode, _.opcode)
		def mnemonic = onExp(t)('mnemonic, _.mnemonic)
		def exceptions = onExp(t)('exceptions, _.exceptions)
		def value = onExp(t)('value, _.value)
	}

	implicit def expToSWAP_Ops(t: Exp[SWAP.type]) = new SWAP_Ops(t)
	class SWAP_Ops(t: Exp[SWAP.type]) {
		def opcode = onExp(t)('opcode, _.opcode)
		def mnemonic = onExp(t)('mnemonic, _.mnemonic)
		def exceptions = onExp(t)('exceptions, _.exceptions)
	}

	implicit def expToTABLESWITCH_Ops(t: Exp[TABLESWITCH]) = new TABLESWITCH_Ops(t)
	class TABLESWITCH_Ops(t: Exp[TABLESWITCH]) {
		def opcode = onExp(t)('opcode, _.opcode)
		def mnemonic = onExp(t)('mnemonic, _.mnemonic)
		def exceptions = onExp(t)('exceptions, _.exceptions)
		def defaultOffset = onExp(t)('defaultOffset, _.defaultOffset)
		def low = onExp(t)('low, _.low)
		def high = onExp(t)('high, _.high)
		def jumpOffsets = onExp(t)('jumpOffsets, _.jumpOffsets)
	}

	implicit def expToWIDE_Ops(t: Exp[WIDE.type]) = new WIDE_Ops(t)
	class WIDE_Ops(t: Exp[WIDE.type]) {
		def opcode = onExp(t)('opcode, _.opcode)
		def mnemonic = onExp(t)('mnemonic, _.mnemonic)
		def exceptions = onExp(t)('exceptions, _.exceptions)
	}
}
	