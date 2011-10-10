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
import tests.Benchmarking._

  /* (Very incomplete) boilerplate code for making use of BAT types convenient in queries.
     This code should be generated
    */

object BATLifting {
  implicit def expToClassFileOps(t: Exp[ClassFile]) = new ClassFileOps(t)
  class ClassFileOps(t: Exp[ClassFile]) {
       def methods = onExp(t)('methods, _.methods)
  }

  implicit def expToAttributeOps(t: Exp[Method_Info]) = new Method_InfoOps(t)
  class Method_InfoOps(t: Exp[Method_Info]) {
       def attributes = onExp(t)('attributes, _.attributes)
       def name = onExp(t)('name, _.name)
  }
  implicit def expToCode_attributeOps(t: Exp[Code_attribute]) = new Code_attributeOps(t)
  class Code_attributeOps(t: Exp[Code_attribute]) {
    def code: Exp[Seq[Instruction]] = onExp(t)('code, _.code)
  }
  object Code_attribute {
    // We need to specify Exp[Seq[Instruction]] instead of Exp[Array[Instruction]] because one cannot convert
    // Exp[Array[T]] to Exp[Seq[T]], so we must request here an implicit conversion (LowPriorityImplicits.wrapRefArray)
    // before wrapping everything within Exp.
    def unapply(t: Exp[_]) : Option[(Exp[Int], Exp[Int],
      Exp[Seq[Instruction]], //doing something special here!
      Exp[ExceptionTable],
      Exp[Attributes])] = {
      if (t ne null) {
        //Calling interpret here makes the result an exotic term - doesn't it?
        t.interpret() match {
          case ca: Code_attribute =>
            assert(ca != null) //This is satisfied because of the pattern match.
            Some((ca.maxStack, ca.maxLocals,
              ca.code: Seq[Instruction], //This is needed to allow predefined implicit conversions to trigger.
              // We can call toExp unconditionally in the generated version of this code.
              ca.exceptionTable, ca.attributes))
          case _ =>
            None
        }
        /*
        // Correct solution - but the type becomes:
        //Exp[Option[(Int, Int, Seq[Instruction], ExceptionTable, Attributes)]
        // which is not accepted by Scala for extractors - but it should, and I believe it would work, much like the
        // desugaring of for comprehensions happens before typing.
        liftCall('Code_attribute$unapply$anon1XXX, //As long as the function below is closed, we can supply a fixed Id
          (t: Any) => t match {
          case ca: Code_attribute =>
            assert(ca != null) //This is satisfied because of the pattern match.
            Some((ca.maxStack, ca.maxLocals,
              toExp(ca.code), //This is needed to allow predefined implicit conversions to trigger.
              // We can call toExp unconditionally in the generated version of this code.
              ca.exceptionTable, ca.attributes))
          case _ =>
            None
        }, t)
        */
      } else None
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

class BasicTests extends JUnitSuite with ShouldMatchersForJUnit {
  def getTestData = {
    val file = new File("lib/scalatest-1.6.1.jar")
    val zipfile = new ZipFile(file)
    val zipentries = zipfile.entries().filter( (file) => !(file.isDirectory()) && file.getName().endsWith(".class"))
    enumerationAsScalaIterator(zipentries)
       .map( zipfile.getInputStream(_))
       .map( (is) => Java6Framework.ClassFile( () => is))
  }



  val warmUpLoops = 0 //100
  val sampleLoops = 1 //5

   @Test def testOpal() {
     val testdata  = getTestData.toSet


     // computing all method names that make an instance-of check in their body

     // native Scala for-comprehension
     var methods: Set[String] = null
     benchMark("native", warmUpLoops = warmUpLoops, sampleLoops = sampleLoops) {
       methods  = for (cf <- testdata;
                       m <- cf.methods;
                       Code_attribute(_,_,code,_,_) <- m.attributes;
                       INSTANCEOF(_) <- code) yield m.name
     }
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

     //println(methods2) //goes OOM!
     var m2Int: Traversable[String] = null
     benchMark("los", warmUpLoops = warmUpLoops, sampleLoops = sampleLoops) {
       m2Int = methods2.interpret()
     }
     println("begin los result")
     println(m2Int)
     println("end los result")

     methods should equal (m2Int)
     val methods3 = queryData.flatMap( cf => cf.methods
                              .flatMap( m => m.attributes
                               .collect( x => x.ifInstanceOf[Code_attribute])
                               .flatMap( c => c.code)
                               .collect( i => i.ifInstanceOf[INSTANCEOF])
                               .map( _ => m.name)))

     var m3Int: Traversable[String] = null
     benchMark("los2", warmUpLoops = warmUpLoops, sampleLoops = sampleLoops) {
       m3Int = methods3.interpret()
     }
     println("begin los2 result")
     println(m3Int)
     println("end los2 result")
     methods should equal (m3Int)

     val methods4 = queryData.flatMap( cf => cf.methods
                              .flatMap( m => m.attributes
                               .collect(
                                   a => onExp(a)('instanceOf$Code_attribute,
                                                 x =>
                                                   if (x.isInstanceOf[Code_attribute])
                                                     Some(x.asInstanceOf[Code_attribute])
                                                   else None))
                               .flatMap( c => c.code)
                               .filter( a => onExp(a)('instanceOf$INSTANCEOF, _.isInstanceOf[INSTANCEOF]))
                               .map( _ => m.name)))

     var m4Int: Traversable[String] = null
     benchMark("los3", warmUpLoops = warmUpLoops, sampleLoops = sampleLoops) {
       m4Int = methods4.interpret()
     }
     println("begin los3 result")
     println(m4Int)
     println("end los3 result")
     methods should equal (m4Int)

     val methods5 =
       for {
         cf <- queryData
         m <- cf.methods
         a <- m.attributes
         if onExp(a)('instanceOf$Code_attribute, _.isInstanceOf[Code_attribute])
         i <- a.asInstanceOf[Exp[Code_attribute]].code //This cast works perfectly
         if onExp(i)('instanceOf$INSTANCEOF, _.isInstanceOf[INSTANCEOF])
       } yield m.name
     var m5Int: Traversable[String] = null
     benchMark("los4", warmUpLoops = warmUpLoops, sampleLoops = sampleLoops) {
       m5Int = methods5.interpret()
     }
     println("begin los4 result")
     println(m5Int)
     println("end los4 result")

     methods should equal (m5Int)
  }
}
