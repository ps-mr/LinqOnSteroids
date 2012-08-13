package schema
case class Author(firstName: String, lastName: String)
case class Book(title: String, publisher: String, authors: Seq[Author])
case class Result(title: String, authorName: String, coauthors: Int)

import de.tud.cs.st.bat.resolved._

/*
        classFile ← classFiles.asSmart
        method ← classFile.methods
        body ← method.body
          body ← method.body
          instruction ← body.instructions

 */
case class FieldRecord(classFile: ClassFile, field: Field)
case class MethodRecord(classFile: ClassFile, method: Method)
case class ConcreteMethodRecord(classFile: ClassFile, method: Method, body: Code)
case class BytecodeInstr(classFile: ClassFile, method: Method, body: Code, instr: Instruction)
case class BytecodeInstrIndexed(classFile: ClassFile, method: Method,instruction: Instruction, index : Int)
case class BytecodeInstrWindow(instrIdxes: Seq[Int], instrs: Seq[Instruction], classFile: ClassFile, method: Method)


// vim: set ts=4 sw=4 et:
