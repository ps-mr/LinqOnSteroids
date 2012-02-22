package ivm
package opaltests


import expressiontree._

import scala.collection.JavaConversions._
import optimization.Optimization
import optimization.SubquerySharing
import de.tud.cs.st.bat.resolved
import resolved.reader.Java6Framework
import resolved._
import Lifting._
import Java6Framework.ClassFile
import resolved.Attributes
import java.util.zip.ZipFile
import java.io.File
import performancetests.Benchmarking
import collections.{TypeMapping, IncHashSet}
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.FunSuite

/* (Very incomplete) boilerplate code for making use of BAT types convenient in queries.
 * This code should be generated */
object BATLifting {
  implicit def expToClassFileOps(t: Exp[ClassFile]) = new ClassFileOps(t)
  class ClassFileOps(t: Exp[ClassFile]) {
    def constructors = onExp(t)('constructors, _.constructors)
    def methods = onExp(t)('methods, _.methods)
    def fields = onExp(t)('fields, _.fields)
    def thisClass = onExp(t)('thisClass, _.thisClass)
    def superClass = onExp(t)('superClass, _.superClass)
    def interfaces = onExp(t)('interfaces, _.interfaces)
    def isFinal = onExp(t)('isFinal, _.isFinal)
    def isClassDeclaration = onExp(t)('isClassDeclaration, _.isClassDeclaration)
    def isInterfaceDeclaration = onExp(t)('isInterfaceDeclaration, _.isInterfaceDeclaration)
  }

  implicit def expToObjectTypeOps(t: Exp[ObjectType]) = new ObjectTypeOps(t)
  class ObjectTypeOps(t: Exp[ObjectType]) {
    def simpleName = onExp(t)('simpleName, _.simpleName)
    def packageName = onExp(t)('packageName, _.packageName)
  }

  implicit def expToClassMemberOps(t: Exp[ClassMember]) = new ClassMemberOps(t)
  class ClassMemberOps(t: Exp[ClassMember]) {
    def isPublic = onExp(t)('isPublic, _.isPublic)
    def isProtected = onExp(t)('isProtected, _.isProtected)
    def isPrivate = onExp(t)('isPrivate, _.isPrivate)
    def isStatic = onExp(t)('isStatic, _.isStatic)
  }

  implicit def expToFieldOps(t: Exp[Field]) = new FieldOps(t)
  class FieldOps(t: Exp[Field]) {
    def attributes = onExp(t)('attributes, _.attributes)
    def name = onExp(t)('name, _.name)
  }

  implicit def expToMethodOps(t: Exp[Method]) = new MethodOps(t)
  class MethodOps(t: Exp[Method]) {
    def attributes = onExp(t)('attributes, _.attributes)
    def descriptor = onExp(t)('descriptor, _.descriptor)
    def name = onExp(t)('name, _.name)
    def body = onExp(t)('body, _.body)
  }

  implicit def expToMethodDescriptorOps(t: Exp[MethodDescriptor]) = new MethodDescriptorOps(t)
  class MethodDescriptorOps(t: Exp[MethodDescriptor]) {
    def returnType = onExp(t)('returnType, _.returnType)
    def parameterTypes = onExp(t)('parameterTypes, _.parameterTypes)
  }

  implicit def expToCodeAttributeOps(t: Exp[CodeAttribute]) = new CodeAttributeOps(t)
  class CodeAttributeOps(t: Exp[CodeAttribute]) {
    def code: Exp[Seq[Instruction]] = onExp(t)('code, _.code)
    def exceptionTable = onExp(t)('exceptionTable, _.exceptionTable)
  }

  implicit def expToExceptionTableEntryOps(t: Exp[ExceptionTableEntry]) = new ExceptionTableEntryOps(t)
  class ExceptionTableEntryOps(t: Exp[ExceptionTableEntry]) {
    def catchType = onExp(t)('catchType, _.catchType)
  }
}

/*
 * More experimental boilerplate code - this needs to evolve into something that we can generate.
 */
object BATLiftingExperimental {
  object CodeAttribute {
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
          case ca: CodeAttribute =>
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
}

class BasicTests extends FunSuite with ShouldMatchers with Benchmarking {
  import OpalTestData._

  //A simple query, which does not use pattern matching.
  test("basicQuery") {
    basicQuery()
  }

  def basicQuery() {
    var methodsNative: Set[Attribute] = null
    // native Scala for-comprehension
    benchMark("native simple") {
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
    benchMark("los simple") {
      methods = methodsQuery.interpret()
    }

    //Result on my system (PG - Core 2 @2.66 GHz):
    //>>> Name = native simple, time = 55.415 +- 1.225
    //>>> Name = los simple, time = 59.698 +- 2.857
  }

  test("testBuildTypeHierarchy") {
    testBuildTypeHierarchy()
  }

  def testBuildTypeHierarchy() {
    import BATLifting._
    import collection.{Set => CSet}

    val typesQueryable = queryData
    //Pair of form (type, direct supertype)
    val superClasses: Exp[CSet[(ObjectType, ObjectType)]] = typesQueryable.flatMap(classFile =>
        (for (superClass <- classFile.interfaces) yield (superClass, classFile.thisClass)) :+ (classFile.superClass.get, classFile.thisClass))
    val superClassesMap: Exp[Map[ObjectType, CSet[ObjectType]]] = superClasses.groupBySel(_._2, _._1)
    val subClassesMap: Exp[Map[ObjectType, CSet[ObjectType]]] = superClasses.groupBySel(_._1, _._2)
    superClassesMap.interpret()
    subClassesMap.interpret()
  }

  // compute all method names that make an instance-of check in their body, using the .code member.
  test("testOpalNewStyle") {
    testOpalNewStyle()
  }

  def testOpalNewStyle() {
    var methodsNative: Set[String] = null
    benchMark("native-new") {
      methodsNative = for (cf <- testdata;
                           m <- cf.methods if m.body.isDefined;
                           INSTANCEOF(_) <- m.body.get.code) yield m.name
    }
    //Ensure that the results are reasonable; 84 has been simply measured when the results were correct.
    //Not very pretty, but better than nothing
    methodsNative.size should be (84)

    // using reified query; INSTANCEOF is here shadowed.
    import BATLifting._
    import BATLiftingExperimental._

    intercept[ExoticTermException] {
      //The pattern-matches used here are unsound:
      val methodsLos1 =
        for {
          cf <- queryData
          m <- cf.methods
          mBody <- Let(m.body)
          if mBody.isDefined
          INSTANCEOF(_) <- mBody.get.code
        } yield m.name

      println(methodsLos1) //Fails because the terms are inadequate

      var m1Int: Traversable[String] = null
      benchMark("los1-new") {
        m1Int = methodsLos1.interpret()
      }
      methodsNative should equal (m1Int)
    }

    val methodsLos2 = queryData.flatMap(cf => cf.methods
      .withFilter(m => m.body.isDefined)
      .flatMap(m => m.body.get.code
      .collect(i => i.ifInstanceOf[INSTANCEOF])
      .map(_ => m.name)))

    var m2Int: Traversable[String] = null
    benchMark("los2-new") {
      m2Int = methodsLos2.interpret()
    }
    methodsNative should equal (m2Int)
  }

  // native Scala for-comprehension
  val methodsNative: Set[String] = benchMark("native") {
    for (cf <- testdata;
         m <- cf.methods;
         CodeAttribute(_,_,code,_,_) <- m.attributes;
         INSTANCEOF(_) <- code) yield m.name
  }

  // using reified query; INSTANCEOF is here shadowed.
  import BATLifting._
  import BATLiftingExperimental._

  test("testOpal") {
    testOpal()
  }

  def testOpal() {
    // computing all method names that make an instance-of check in their body

    //Ensure that the results are reasonable; 84 has been simply measured when the results were correct.
    //Not very pretty, but better than nothing
    methodsNative.size should be (84)

    intercept[ExoticTermException] {
      //The pattern-matches used here are unsound:
      val methodsLos1 = for (cf <- queryData;
                             m <- cf.methods;
                             CodeAttribute(_,_,code,_,_) <- m.attributes;
                             INSTANCEOF(_) <- code) yield m.name

      println(methodsLos1) //Fails because the terms are inadequate

      var m1Int: Traversable[String] = null
      benchMark("los1") {
        m1Int = methodsLos1.interpret()
      }
      methodsNative should equal (m1Int)
    }

    val methodsLos2 = queryData.flatMap( cf => cf.methods
      .flatMap( m => m.attributes
      .collect( x => x.ifInstanceOf[CodeAttribute])
      .flatMap( c => c.code)
      .collect( i => i.ifInstanceOf[INSTANCEOF])
      .map( _ => m.name)))

    var m2Int: Traversable[String] = null
    benchMark("los2") {
      m2Int = methodsLos2.interpret()
    }
    methodsNative should equal (m2Int)

    //This is twice as slow as the other los solutions, except methodsLos2.
    //My guess is that this is because collect applies the given function twice.
    val methodsLos3 = queryData.flatMap( cf => cf.methods
      .flatMap( m => m.attributes
      .collect(
      a => onExp(a)('instanceOf$CodeAttribute,
        x =>
          if (x.isInstanceOf[CodeAttribute])
            Some(x.asInstanceOf[CodeAttribute])
          else None))
      .flatMap( c => c.code)
      .filter( a => onExp(a)('instanceOf$INSTANCEOF, _.isInstanceOf[INSTANCEOF]))
      .map( _ => m.name)))

    var m3Int: Traversable[String] = null
    benchMark("los3") {
      m3Int = methodsLos3.interpret()
    }
    methodsNative should equal (m3Int)

    //Best performance and quite clear code.
    val methodsLos4 =
      for {
        cf <- queryData
        m <- cf.methods
        a <- m.attributes
        if onExp(a)('instanceOf$CodeAttribute, _.isInstanceOf[CodeAttribute])
        i <- a.asInstanceOf[Exp[CodeAttribute]].code //This cast works perfectly
        if onExp(i)('instanceOf$INSTANCEOF, _.isInstanceOf[INSTANCEOF])
      } yield m.name
    var m4Int: Traversable[String] = null
    benchMark("los4") {
      m4Int = methodsLos4.interpret()
    }
    methodsNative should equal (m4Int)

    type ID[T] = T

    val methodsLos5 =
      for {
        cf <- queryData
        m <- cf.methods
        ca <- m.attributes.typeFilter[CodeAttribute]
        io <- ca.code.typeFilter[INSTANCEOF]
      } yield m.name
    val m5Int: Traversable[String] = benchMark("los5") {
      methodsLos5.interpret()
    }
    methodsNative should equal (m5Int)

    //Let's consider type indexing.
    val methodsLos5Desugared = queryData.flatMap(cf => cf.methods
      .flatMap(m => m.attributes.typeFilter[CodeAttribute]
      .flatMap(ca => ca.code.typeFilter[INSTANCEOF]
      .map(io => m.name))))
    //No benchmarking as this code is the same as methodsLos5; this just checks that we indeed got the desugaring right.
    methodsNative should equal (methodsLos5Desugared.interpret())
  }


  //////////////
  // INDEXING //
  //////////////
  type PairMethodAnd[+T] = (Method, T)

  test("testOpalWithIndexing") {
    testOpalWithIndexing()
  }

  def testOpalWithIndexing() {
    //Let coll0 be "m.attributes.typeFilter[CodeAttribute]", coll1 be "ca", FindSubcolls[coll1] be "coll1.code", RestQuery[res] be res.map(io => m.name),
    //T be "INSTANCEOF"
    //OuterQuery[S] be "queryData.flatMap(cf => cf.methods.flatMap(m => S))"
    //So that the query becomes:
    // queryData.flatMap(cf => cf.methods
    //  .flatMap(m =>
    //         coll0.flatMap(coll1 => RestQuery[FindSubcolls[coll1].typeFilter[T]]) ))"
    //Under this conditions, methodsLos5Desugared is a an instance of the pattern:
    //OuterQuery[coll0.flatMap(coll1 => RestQuery[FindSubcolls[coll1].typeFilter[T]])]
    //which we rewrite to the application of an index (if existing) of the following form:
    //OuterQuery[coll0.flatMap(coll1 => FindSubcolls[coll1].map(elem => (<free variables including coll1>, elem))]
    //(see typeIdxBase, which manually optimizes this to omit ca which is not needed).

    // another version using type index but manual index application
    // (need to code this into optimizer - not quite obvious how to do it)
    val typeIdxBase: Exp[Set[PairMethodAnd[Instruction]]] = for {
      cf <- queryData
      m <- cf.methods
      ca <- m.attributes.typeFilter[CodeAttribute]
      i <- ca.code
    } yield (m, i)
    benchMark("los6 index-base creation")(typeIdxBase.interpret())

    val typeIdxBaseSeq: Exp[Seq[PairMethodAnd[Instruction]]] = for {
      cf <- queryData.toSeq
      m <- cf.methods
      ca <- m.attributes.typeFilter[CodeAttribute]
      i <- ca.code
    } yield (m, i)
    benchMark("los6 Seq-index-base creation")(typeIdxBaseSeq.interpret())
    //Let us accept some limited overhead with explicit type annotations to create the index
    //val typeindex = q.groupByType(_._2)
    val typeIdx = typeIdxBase.groupByTupleType2
    val typeIdxSeq = typeIdxBaseSeq.groupByTupleType2
    //This is normalization-by-evaluation over terms instead of functions (and who said it is limited to functions?)
    val evaluatedtypeindex: Exp[TypeMapping[Seq, PairMethodAnd]] = //benchMark("los6 index creation"){ asExp(typeIdx.interpret()) } //XXX
      benchMark("los6 Seq-index creation"){ asExp(typeIdxSeq.interpret()) }
    //println(evaluatedtypeindex.map.keys)

    //val methodsLos6 = Const(evaluatedtypeindex).get[INSTANCEOF].map(_._1.name)
    //XXX this code, instead of using typeindex: Exp[...], uses toExp(typeindex.interpret()), which is a hack. The goal
    //is to have the index evaluated at all - otherwise there would be no speed-up. However, the proper solution is to
    //wrap the query result in something similar to IncrementalResult's constructor: Exp[T] -> Exp[T] - so that the result
    //is materialized and incrementally maintained.
    val methodsLos6 = evaluatedtypeindex.get[INSTANCEOF].map(_._1.name)
    //val methodsLos6 = expToTypeMappingAppOps[Set, SND](evaluatedtypeindex).get[INSTANCEOF].map(_._1.name)
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
    var m6Int: Traversable[String] = null
    benchMark("los6 (with index)") {
      m6Int = methodsLos6.interpret().toSet
    }
    methodsNative should equal (m6Int)

    type QueryAnd[+T] = ((ClassFile, Method, CodeAttribute), T);
    {
      //Same code as above, except that the index returns all free variables, so that the optimizer might find it.
      val typeIdxBase: Exp[Seq[QueryAnd[Instruction]]] = for {
        cf <- queryData.toSeq
        m <- cf.methods
        ca <- m.attributes.typeFilter[CodeAttribute]
        i <- ca.code
      } yield (asExp((cf, m, ca)), i)

      val typeIdx = typeIdxBase.groupByTupleType2
      // Interpreting used to take a whopping 120 seconds. Why? Since the result is a set, each class file is being hashed once
      // per each instruction. The fix was adding toSeq above. In fact, this transformation could probably be done also on the query to optimize;
      // after that, the optimizer would again find a matching subquery. The new runtime is ~ 0.3-0.4 sec.
      val evaluatedtypeindex: Exp[TypeMapping[Seq, QueryAnd]] = benchMark("los6 Seq-index (less manually optimized) creation"){ asExp(typeIdx.interpret()) }

      val methodsLos6 = evaluatedtypeindex.get[INSTANCEOF].map(_._1._2.name)

      var m6Int: Traversable[String] = null
      benchMark("los6 (with index, less manually optimized)") {
        m6Int = methodsLos6.interpret().toSet
      }
      methodsNative should equal (m6Int)
    }

    // another version using type index, to try to understand how this could be coded more easily into the optimizer.
    val idx1Base /*: Exp[Set[SND[Attribute]]]*/ = for {
      cf <- queryData.toSeq
      m <- cf.methods
      ca <- m.attributes
    } yield (m, ca)
    val typeIdx1 = idx1Base.groupByTupleType2
    //NOTE: it is crucial to mention SND here.
    val evTypeIdx1: Exp[TypeMapping[Seq, PairMethodAnd]] = benchMark("los7: creation of index typeIdx1"){ asExp(typeIdx1.interpret()) }
    //Hmm: this index inverts a 1-1 mapping, is that a good idea performance-wise?
    val idx1 = idx1Base.groupBySel(_._2, _._1)
    val evIdx1 = benchMark("los7: creation of index idx1"){ asExp(idx1.interpret()) }

    type PairCodeAttributeAnd[+T] = (CodeAttribute, T)
    /*
    val idx2Base = for {
      pair <- idx1Base
      ca <- pair._1.attributes.typeFilter[CodeAttribute]
      i <- ca.code
    } yield (ca, i)
    val idx2 = idx2Base.groupByTupleType2
    val evIdx2: Exp[TypeMapping[Set, PairCodeAttribute]] = idx2.interpret()
    */
    //val idx2BaseOpt = evIdx1.get[CodeAttribute].flatMap(ca => ca.code.map(i => (ca, i)))
    val idx2BaseOpt /*: Exp[Set[PairCodeAttribute[Attribute]]]*/ = for {
      pair <- evTypeIdx1.get[CodeAttribute]
      i <- pair._2.code
    } yield (pair._2, i)
    val typeIdx2Opt = idx2BaseOpt.groupByTupleType2
    val evTypeIdx2Opt: Exp[TypeMapping[Seq, PairCodeAttributeAnd]] = benchMark("los7: creation of index typeIdx2Opt"){ asExp(typeIdx2Opt.interpret()) }

    val methodsLos7 = evTypeIdx2Opt.get[INSTANCEOF].flatMap(x => evIdx1(x._1).map(_.name))

    var m7Int: Traversable[String] = null
    benchMark("los7 (with hierarchical indexing)") {
      m7Int = methodsLos7.interpret().toSet
    }
    methodsNative should equal (m7Int)
  }
}
