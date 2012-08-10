package performancetests
package opaltests

import ivm._
import expressiontree._
import Lifting._

import de.tud.cs.st.bat
import bat.resolved._
import analyses._

import reader.Java6Framework
import bat.resolved.analyses.ClassHierarchy

import collection.immutable.Seq
import collection.{Seq => CSeq}

import org.scalatest.matchers.ShouldMatchers

/**
 * User: pgiarrusso
 * Date: 4/8/2012
 */

abstract class FBAnalysesBase extends QueryBenchmarking with ShouldMatchers {
  def zipFiles: Seq[String]

  val classFiles: Seq[ClassFile] = benchMark("Reading all class files", execLoops = 1, minSampleLoops = 1, maxCoV = None) {
    for (zipFile ← zipFiles.toVector; classFile ← Java6Framework.ClassFiles(zipFile)) yield classFile
  }
  val getClassFile: Map[ObjectType, ClassFile] = classFiles.map(cf ⇒ (cf.thisClass, cf)).toMap

  // Now the classHierarchy is built functionally - hence, the result could be incrementally maintained (at least for the
  // addition of classes).
  val classHierarchy = (new ClassHierarchy /: classFiles)(_ + _)

  println("Number of class files: " + classFiles.length)
  println("Numer of methods: " + methodsSQuOpt().interpret().size)


  def fieldsNative() =
    for {
      classFile ← classFiles
      field ← classFile.fields
    } yield (classFile, field)

  def fieldsSQuOpt() = {
    import BATLifting._
    for {
      classFile ← classFiles.asSmart
      field ← classFile.fields
    } yield (classFile, field)
  }

  def methodsNative() = {
    for {
      classFile ← classFiles
      method ← classFile.methods
    } yield (classFile, method)
  }

  def methodsSQuOpt() = {
    import BATLifting._
    for {
      classFile ← classFiles.asSmart
      method ← classFile.methods
    } yield (classFile, method)
  }

  def methodBodiesSQuOpt() = {
    import BATLifting._
    //import schema._ //{squopt => _, _}
    import schema.squopt._
    for {
      classFile ← classFiles.asSmart
      method ← classFile.methods
      body ← method.body
    } yield MethodRecord(classFile, method, body)
  }

  def methodBodiesModularNative() = {
    import schema._
    for {
      (classFile, method) <- methodsNative()
      body <- method.body
    } yield MethodRecord(classFile, method, body) //MethodRecord(cfM._1, cfM._2, body)
  }

  def methodBodiesInstructionsModularNative() = {
    import schema._
    for {
      MethodRecord(classFile, method, body) ← methodBodiesModularNative()
      instruction ← body.instructions
    } yield (classFile, method, body, instruction) //MethodRecord(cfM._1, cfM._2, body)
  }

  def methodBodiesModularSQuOpt() = {
    import BATLifting._
    import schema.squopt._
    for {
      cfM <- methodsSQuOpt()
      classFile <- Let(cfM._1)
      method ← Let(cfM._2)
      body <- method.body
    } yield MethodRecord(classFile, method, body) //MethodRecord(cfM._1, cfM._2, body)
  }

  def methodBodiesInstructionsModularSQuOpt() = {
    import BATLifting._
    import schema.squopt._
    for {
      cfMB /*(classFile, method, body)*/ ← methodBodiesModularSQuOpt()
      instruction ← cfMB.body.instructions
    } yield (cfMB.classFile, cfMB.method, cfMB.body, instruction)
  }

  def methodBodiesInstructionsSlidingNative(len: Int): Seq[schema.BytecodeInstrWindow] = {
    import schema._
    for {
      MethodRecord(classFile, method, body) ← methodBodiesModularNative()
      (instrs, instrIdxes) ← body.instructions.zipWithIndex.filter(_._1 != null).toSeq.sliding(len).toStream.map(_.unzip)
    } yield BytecodeInstrWindow(instrIdxes, instrs, classFile, method)
  }

  def  methodBodiesInstructionsSlidingSQuOpt(len: Int): Exp[Seq[schema.BytecodeInstrWindow]] = {
    import BATLifting._
    import schema.squopt._
    for {
      methodRecord ← methodBodiesModularSQuOpt()
      window ← methodRecord.body.instructions.zipWithIndex.filter(_._1 != null).toSeq.sliding(len).map(_.unzip)
    } yield BytecodeInstrWindow(window._2, window._1, methodRecord.classFile, methodRecord.method)
  }
}
