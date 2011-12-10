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
 * This code should be generated */
object BATLifting {
  implicit def expToClassFileOps(t: Exp[ClassFile]) = new ClassFileOps(t)
  class ClassFileOps(t: Exp[ClassFile]) {
    def methods = onExp(t)('methods, _.methods)
    def fields = onExp(t)('fields, _.fields)
    def thisClass = onExp(t)('thisClass, _.thisClass)
    def superClass = onExp(t)('superClass, _.superClass)
    def interfaces = onExp(t)('interfaces, _.interfaces)
  }

  implicit def expToObjectTypeOps(t: Exp[ObjectType]) = new ObjectTypeOps(t)

  class ObjectTypeOps(t: Exp[ObjectType]) {
    def simpleName = onExp(t)('simpleName, _.simpleName)
    def packageName = onExp(t)('packageName, _.packageName)
  }

  implicit def expToAttributeOps(t: Exp[Method]) = new Method_InfoOps(t)
  class Method_InfoOps(t: Exp[Method]) {
       def attributes = onExp(t)('attributes, _.attributes)
       def name = onExp(t)('name, _.name)
  }
  implicit def expToCode_attributeOps(t: Exp[Code_attribute]) = new Code_attributeOps(t)
  class Code_attributeOps(t: Exp[Code_attribute]) {
    def code: Exp[Seq[Instruction]] = onExp(t)('code, _.code)
  }
}

/*
 * More experimental boilerplate code - this needs to evolve into something that we can generate.
 */
object BATLiftingExperimental {
  object Code_attribute {
    // We need to specify Exp[Seq[Instruction]] instead of Exp[Array[Instruction]] because one cannot convert
    // Exp[Array[T]] to Exp[Seq[T]], so we must request here an implicit conversion (LowPriorityImplicits.wrapRefArray)
    // before wrapping everything within Exp.
    def unapply(t: Exp[_]): Option[(Exp[Int], Exp[Int],
      Exp[Seq[Instruction]], //doing something special here!
      Exp[ExceptionTable],
      Exp[Attributes])] = {
      if (t ne null) {
        //Calling interpret here makes the result an exotic term - doesn't it?
        t.interpret() match {
          case ca: Code_attribute =>
            assert(ca != null) //This is satisfied because of the pattern match.
            Some((ca.maxStack, ca.maxLocals,
              ca.code, ca.exceptionTable, ca.attributes))
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
    def unapply(t: Exp[_]): Option[Exp[ReferenceType]] = {
      if ((t ne null) && t.interpret().isInstanceOf[INSTANCEOF])
        Some(onExp(t.asInstanceOf[Exp[INSTANCEOF]])('referenceType, _.referenceType))
      else None
    }
  }

}
  /* end of boilerplate code */

object OpalTestData {
  def getTestData = {
    val file = new File("lib/scalatest-1.6.1.jar")
    val zipfile = new ZipFile(file)
    val zipentries = zipfile.entries().filter( (file) => !(file.isDirectory()) && file.getName().endsWith(".class"))
    enumerationAsScalaIterator(zipentries)
       .map(zipfile.getInputStream(_))
       .map(is => Java6Framework.ClassFile(() => is))
  }
  val testdata  = getTestData.toSet
  val queryData = toExp(testdata)
  val warmUpLoops = 1 //100
  val sampleLoops = 2 //20
}

class BasicTests extends JUnitSuite with ShouldMatchersForJUnit {
  import OpalTestData._

  //A simple query, which does not use pattern matching.
  @Test def basicQuery() {
    var methodsNative: Set[Attribute] = null
    // native Scala for-comprehension
    benchMark("native simple", warmUpLoops = warmUpLoops, sampleLoops = sampleLoops) {
      methodsNative =
        for (cf <- testdata;
             m <- cf.methods;
             attrib <- m.attributes)
        yield attrib
    }

    // using reified query
    import BATLifting._

    val methodsQuery =
      for (cf <- queryData;
           m <- cf.methods;
           attrib <- m.attributes)
      yield attrib

    var methods: Traversable[Attribute] = null
    benchMark("los simple", warmUpLoops = warmUpLoops, sampleLoops = sampleLoops) {
      methods = methodsQuery.interpret()
    }

    //Result on my system (PG - Core 2 @2.66 GHz):
    //>>> Name = native simple, time = 55.415 +- 1.225
    //>>> Name = los simple, time = 59.698 +- 2.857
  }

  @Test def testOpal() {
    // computing all method names that make an instance-of check in their body

    // native Scala for-comprehension

    var methodsNative: Set[String] = null
    benchMark("native", warmUpLoops = warmUpLoops, sampleLoops = sampleLoops) {
      methodsNative  = for (cf <- testdata;
                      m <- cf.methods;
                      Code_attribute(_,_,code,_,_) <- m.attributes;
                      INSTANCEOF(_) <- code) yield m.name
    }
    //Ensure that the results are reasonable; 84 has been simply measured when the results were correct.
    //Not very pretty, but better than nothing
    methodsNative.size should be (84)

    // using reified query; INSTANCEOF is here shadowed.
    import BATLifting._
    import BATLiftingExperimental._

    //The pattern-matches used here are unsound:
    val methodsLos1 = for (cf <- queryData;
                        m <- cf.methods;
                        Code_attribute(_,_,code,_,_) <- m.attributes;
                        INSTANCEOF(_) <- code) yield m.name

    intercept[ExoticTermException] {
      println(methodsLos1) //Fails because the terms are inadequate
    }

    var m1Int: Traversable[String] = null
    benchMark("los1", warmUpLoops = warmUpLoops, sampleLoops = sampleLoops) {
      m1Int = methodsLos1.interpret()
    }
    methodsNative should equal (m1Int)

    val methodsLos2 = queryData.flatMap( cf => cf.methods
      .flatMap( m => m.attributes
      .collect( x => x.ifInstanceOf[Code_attribute])
      .flatMap( c => c.code)
      .collect( i => i.ifInstanceOf[INSTANCEOF])
      .map( _ => m.name)))

    var m2Int: Traversable[String] = null
    benchMark("los2", warmUpLoops = warmUpLoops, sampleLoops = sampleLoops) {
      m2Int = methodsLos2.interpret()
    }
    methodsNative should equal (m2Int)

    //This is twice as slow as the other los solutions, except methods3.
    //My guess is that this is because collect applies the given function twice.
    val methodsLos3 = queryData.flatMap( cf => cf.methods
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

    var m3Int: Traversable[String] = null
    benchMark("los3", warmUpLoops = warmUpLoops, sampleLoops = sampleLoops) {
      m3Int = methodsLos3.interpret()
    }
    methodsNative should equal (m3Int)

    //Best performance and quite clear code.
    val methodsLos4 =
      for {
        cf <- queryData
        m <- cf.methods
        a <- m.attributes
        if onExp(a)('instanceOf$Code_attribute, _.isInstanceOf[Code_attribute])
        i <- a.asInstanceOf[Exp[Code_attribute]].code //This cast works perfectly
        if onExp(i)('instanceOf$INSTANCEOF, _.isInstanceOf[INSTANCEOF])
      } yield m.name
    var m4Int: Traversable[String] = null
    benchMark("los4", warmUpLoops = warmUpLoops, sampleLoops = sampleLoops) {
      m4Int = methodsLos4.interpret()
    }
    methodsNative should equal (m4Int)

    type ID[T] = T

    val methodsLos5 =  for (cf <- queryData;
                         m <- cf.methods;
                         ca <- m.attributes.typeFilter[Code_attribute];
                         io <- ca.code.typeFilter[INSTANCEOF]) yield m.name
    var m5Int: Traversable[String] = null
    benchMark("los5", warmUpLoops = warmUpLoops, sampleLoops = sampleLoops) {
      m5Int = methodsLos5.interpret()
    }
    methodsNative should equal (m5Int)

    // another version using type index but manual index application
    // (need to code this into optimizer - not quite obvious how to do it)

    val q = for (cf <- queryData;
                 m <- cf.methods;
                 ca <- m.attributes.typeFilter[Code_attribute];
                 i <- ca.code if i !== null      // the null check is not very nice...any ideas?
    ) yield (m, i)
    //Util.assertType[Exp[Set[SND[Instruction]]]](q) //Does not compile because there is no lifting Set[T] => Exp[Set[T]]
    //Util.assertType[Exp[Traversable[SND[Instruction]]]](q) //This is just for documentation.

    //val typeindex = q.groupByType(_._2)
    val typeindex = q.groupByTupleType2
    val evaluatedtypeindex = typeindex.interpret()
    //println(evaluatedtypeindex.map.keys)

    //val methodsLos6 = Const(evaluatedtypeindex).get[INSTANCEOF].map(_._1.name)
    //XXX this code, instead of using typeindex: Exp[...], uses toExp(typeindex.interpret()), which is a hack. The goal
    //is to have the index evaluated at all - otherwise there would be no speed-up. However, the proper solution is to
    //wrap the query result in something similar to IncrementalResult's constructor: Exp[T] -> Exp[T] - so that the result
    //is materialized and incrementally maintained.
    val methodsLos6 = expToTypeMappingAppOps[Set, PartialApply1Of2[Tuple2, Method]#Apply](evaluatedtypeindex).get[INSTANCEOF].map(_._1.name)
    //If I omit type parameters, not type inference, but typechecking here fails after figuring the right type, even if expToTypeMappingAppOps is explicitly called.
    //As you see, it fails to unify D[_] with [B](de.tud.cs.st.bat.resolved.Method_Info, B).
    //This is documented as https://issues.scala-lang.org/browse/SI-5075
    /*
[error] /Users/pgiarrusso/Documents/Research/Sorgenti/SAE-privGit/linqonsteroids/src/test/scala/ivm/opaltests/BasicTests.scala:228: no type parameters for method expToTypeMappingAppOps: (t: ivm.expressiontree.Exp[ivm.collections.TypeMapping[C,D]])ivm.expressiontree.Lifting.TypeMappingAppOps[C,D] exist so that it can be applied to arguments (ivm.expressiontree.Exp[ivm.collections.TypeMapping[Traversable,[B](de.tud.cs.st.bat.resolved.Method_Info, B)]])
[error]  --- because ---
[error] argument expression's type is not compatible with formal parameter type;
[error]  found   : ivm.expressiontree.Exp[ivm.collections.TypeMapping[Traversable,[B](de.tud.cs.st.bat.resolved.Method_Info, B)]]
[error]  required: ivm.expressiontree.Exp[ivm.collections.TypeMapping[?0C,?0D]]
[error]      val methodsLos6 = expToTypeMappingAppOps(evaluatedtypeindex).get[INSTANCEOF].map(_._1.name)
    */
    var m7Int: Traversable[String] = null
    benchMark("los6 (with index)", warmUpLoops = warmUpLoops, sampleLoops = sampleLoops) {
      m7Int = methodsLos6.interpret()
    }
    methodsNative should equal (m7Int)
  }
}
