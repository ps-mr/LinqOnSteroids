package ivm
package opaltests


import org.scalatest.junit.{ShouldMatchersForJUnit, JUnitSuite}
import org.junit.Test

import expressiontree._

import scala.collection.JavaConversions._
import collections.CollectionReifier
import optimization.Optimization
import optimization.SubquerySharing
import de.tud.cs.st.bat.resolved
import resolved.reader.Java6Framework
import resolved._
import Lifting._
import Java6Framework.ClassFile
import resolved.TypeAliases.Attributes
import resolved.TypeAliases.Methods
import java.util.zip.ZipFile
import java.util.zip.ZipEntry
import java.io.{InputStream, File}
import resolved.TypeAliases.ExceptionTable

  /* (Very incomplete) boilerplate code for making use of BAT types convenient in queries.
     This code should be generated
    */

object BATLifting {
  implicit def expToClassFileOps(t: Exp[ClassFile]) = new ClassFileOps(t)
  class ClassFileOps(t: Exp[ClassFile]) {
       def methods = liftCall('methods, (cf: ClassFile) => cf.methods, t)
  }

  implicit def expToAttributeOps(t: Exp[Method_Info]) = new Method_InfoOps(t)
  class Method_InfoOps(t: Exp[Method_Info]) {
       def attributes = liftCall('attributes, (m: Method_Info) => m.attributes, t)
       def name = liftCall('name, (m: Method_Info) => m.name, t)
  }
  object Code_attribute {
    def unapply(t: Exp[_]) : Option[(Exp[Int],Exp[Int],Exp[Seq[Instruction]],Exp[ExceptionTable], Exp[Attributes])] = {
      // or should we rather compare the actual wrapped value for nullness?
      if ((t ne null) && t.interpret().isInstanceOf[Code_attribute] /*&& (t.manifest <:< classManifest[Code_attribute])*/) {
        val codeAttrib = t.asInstanceOf[Exp[Code_attribute]]
        Some(liftCall('maxStack,      (ca: Code_attribute) => ca.maxStack, codeAttrib),
                 liftCall('maxLocals,     (ca: Code_attribute) => ca.maxLocals, codeAttrib),
                 liftCall('code,          (ca: Code_attribute) => ca.code, codeAttrib),
                 liftCall('exceptionTable,(ca: Code_attribute) => ca.exceptionTable, codeAttrib),
                 liftCall('attributes   , (ca: Code_attribute) => ca.attributes, codeAttrib)  )
      } else
        None
    }
  }
  object INSTANCEOF {
    def unapply(t: Exp[_]) : Option[Exp[ReferenceType]] = {
      if ((t ne null) && t.interpret().isInstanceOf[INSTANCEOF])
        Some(liftCall('referenceType, (io: INSTANCEOF) => io.referenceType, t.asInstanceOf[Exp[INSTANCEOF]]))
      else None
    }
  }

}
  /* end of boilerplate code */

class BasicTests  extends JUnitSuite with ShouldMatchersForJUnit {
  def getTestData = {
    val file = new File("lib/scalatest-1.6.1.jar")
    val zipfile = new ZipFile(file)
    val zipentries = zipfile.entries().filter( (file) => !(file.isDirectory()) && file.getName().endsWith(".class"))
    enumerationAsScalaIterator(zipentries)
       .map( zipfile.getInputStream(_))
       .map( (is) => Java6Framework.ClassFile( () => is))
  }




   @Test def testOpal() {
    val testdata  = getTestData.toSet


     // computing all method names that make an instance-of check in their body

     // native Scala for-comprehension
    val methods  = for (cf <- testdata;
                       m <- cf.methods;
                       Code_attribute(_,_,code,_,_) <- m.attributes;
                       INSTANCEOF(_) <- code) yield m.name
     println("begin native result")
     println(methods)
     println("end native result")

     // using reified query; INSTANCEOF is here shadowed.
    import BATLifting._

    val queryData = new CollectionReifier(testdata)
    val methods2 = for (cf <- queryData;
                         m <- cf.methods;
                         Code_attribute(_,_,code,_,_) <- m.attributes;
                         INSTANCEOF(_) <- code) yield m.name
     println("begin los result")
     val m2Int = methods2.interpret()
     println(m2Int)
     println("end los result")

     methods should equal (m2Int)
  }
}
