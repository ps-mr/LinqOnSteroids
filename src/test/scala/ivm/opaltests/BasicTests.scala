package ivm
package opaltests


import org.scalatest.junit.{ShouldMatchersForJUnit, JUnitSuite}
import org.junit.Test

import expressiontree._

import scala.collection.JavaConversions._
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
import performancetests.Benchmarking._

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
        Some(onExp(t.asInstanceOf[Exp[INSTANCEOF]])('referenceType, _.referenceType))
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



  val warmUpLoops = 1 //100
  val sampleLoops = 2 //20

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
//     println("begin native result")
//     println(methods)
//     println(methods.size())
//     println("end native result")

     // using reified query; INSTANCEOF is here shadowed.
     import BATLifting._

     val queryData = toExp(testdata)
     //The pattern-matches used are unsound.

     val methods2 = for (cf <- queryData;
                         m <- cf.methods;
                         Code_attribute(_,_,code,_,_) <- m.attributes;
                         INSTANCEOF(_) <- code) yield m.name

     //println(methods2) //goes OOM!
     var m2Int: Traversable[String] = null
     benchMark("los", warmUpLoops = warmUpLoops, sampleLoops = sampleLoops) {
       m2Int = methods2.interpret()
     }
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
     methods should equal (m3Int)

     //This is twice as slow as the other los solutions, except methods3.
     //My guess is that this is because collect applies the given function twice.
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
     methods should equal (m4Int)

     //Best performance and quite clear code.
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
     methods should equal (m5Int)

     type ID[T] = T

     val methods6 =  for (cf <- queryData;
                          m <- cf.methods;
                          ca <- m.attributes.typeFilter[Code_attribute];
                          io <- ca.code.typeFilter[INSTANCEOF]) yield m.name
     var m6Int: Traversable[String] = null
     benchMark("los5", warmUpLoops = warmUpLoops, sampleLoops = sampleLoops) {
       m6Int = methods6.interpret()
     }
     methods should equal (m6Int)


     class SND[T](_1: Method_Info, _2: T)
       extends Tuple2(_1, _2) //This clause reuses the lifted methods _1, _2: Exp[(Method_Info, T)] => ...

     //Either one of SNDExp or SNDExp2 does the job. This is something we'll have to generate!
     case class SNDExp2[T](p: Exp[(Method_Info, T)]) extends UnaryOpExp[(Method_Info, T), SND[T]](p) {
       override def copy(p: Exp[(Method_Info, T)]) = SNDExp2(p)
       override def interpret = {
         val (p1, p2) = p.interpret
         new SND(p1, p2)
       }
     }
     case class SNDExp[T](p1: Exp[Method_Info], p2: Exp[T]) extends BinaryOpExp[Method_Info, T, SND[T]](p1, p2) {
       override def copy(p1: Exp[Method_Info], p2: Exp[T]) = SNDExp(p1, p2)
       override def interpret() = new SND(p1.interpret(), p2.interpret())
     }
     // another version using type index but manual index application
     // (need to code this into optimizer - not quite obvious how to do it)

     val q = for (cf <- queryData;
                          m <- cf.methods;
                          ca <- m.attributes.typeFilter[Code_attribute];
                          i <- ca.code if !(i is null)      // the null check is not very nice...any ideas?
                          ) yield SNDExp(m, i)
     //Util.assertType[Exp[Set[SND[Instruction]]]](q) //Does not compile because there is no lifting Set[T] => Exp[Set[T]]
     Util.assertType[Exp[Traversable[SND[Instruction]]]](q) //This is just for documentation.

     val typeindex = q.groupByType(_._2)
     val evaluatedtypeindex = typeindex.interpret()
     //println(evaluatedtypeindex.map.keys)

     val methods7 = Const(evaluatedtypeindex).get[INSTANCEOF].map(_._1.name)
     var m7Int: Traversable[String] = null
     benchMark("los6", warmUpLoops = warmUpLoops, sampleLoops = sampleLoops) {
       m7Int = methods7.interpret()
     }
     methods should equal (m7Int)


  }
}
