package performancetests
package opaltests

import ivm._
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
import collections.{TypeMapping, IncHashSet}
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.FunSuite


/*
 * More experimental boilerplate code - this needs to evolve into something that we can generate.
 */
object BATLiftingExperimental {
  object Code {
    // We need to specify Exp[Seq[Instruction]] instead of Exp[Array[Instruction]] because one cannot convert
    // Exp[Array[T]] to Exp[Seq[T]], so we must request here an implicit conversion (LowPriorityImplicits.wrapRefArray)
    // before wrapping everything within Exp.
    def unapply(t: Exp[_]): Option[(Exp[Int], Exp[Int],
      Exp[Seq[Instruction]], //doing something special here!
      Exp[ExceptionHandlers],
      Exp[Attributes])] = {
      if (t ne null) {
        //Calling interpret here makes the result an exotic term - doesn't it?
        t.interpret() match {
          case ca: Code =>
            assert(ca != null) //This is satisfied because of the pattern match.
            Some((ca.maxStack, ca.maxLocals,
              ca.instructions, ca.exceptionHandlers, ca.attributes))
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
              toExp(ca.instructions), //This is needed to allow predefined implicit conversions to trigger.
              // We can call toExp unconditionally in the generated version of this instructions.
              ca.exceptionHandlers, ca.attributes))
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
    // native Scala for-comprehension
    val methodsNative: Set[Attribute] = benchMark("native simple") {
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

    val methods: Traversable[Attribute] = benchMark("los simple") {
      methodsQuery.interpret()
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

  // compute all method names that make an instance-of check in their body, using the .instructions member.
  test("testOpalNewStyle") {
    testOpalNewStyle()
  }

  def testOpalNewStyle() {
    val methodsNative: Set[String] = benchMark("native-new") {
      for (cf <- testdata;
           m <- cf.methods;
           body <- m.body.toList;
           INSTANCEOF(_) <- body.instructions) yield m.name
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
          mBody <- m.body
          INSTANCEOF(_) <- mBody.instructions
        } yield m.name

      println(methodsLos1) //Fails because the terms are inadequate

      val m1Int: Traversable[String] = benchMark("los1-new") {
        methodsLos1.interpret()
      }
      methodsNative should equal (m1Int)
    }

    val methodsLos2 = queryData.flatMap(cf => cf.methods
      .withFilter(m => m.body.isDefined)
      .flatMap(m => m.body.get.instructions
      .collect(i => i.ifInstanceOf[INSTANCEOF])
      .map(_ => m.name)))

    val m2Int: Traversable[String] = benchMark("los2-new") {
      methodsLos2.interpret()
    }
    methodsNative should equal (m2Int)

    val methodsLos2_1 = queryData.flatMap(cf => cf.methods
      .flatMap(m => m.body
      .flatMap(body => body.instructions
      .collect(i => i.ifInstanceOf[INSTANCEOF])
      .map(_ => m.name))))

    benchMark("los2-new") {
      methodsLos2_1.interpret()
    } should equal (methodsNative)


    val methodsLos5 =
      for {
        cf <- queryData
        m <- cf.methods
        mBody <- m.body
        io <- mBody.instructions.typeFilter[INSTANCEOF]
      } yield m.name
    val m5Int: Traversable[String] = benchMark("los5-new") {
      methodsLos5.interpret()
    }
    methodsNative should equal (m5Int)

    val methodsLos5Seq =
      (for {
        cf <- queryData.toSeq
        m <- cf.methods
        mBody <- m.body
        io <- mBody.instructions.typeFilter[INSTANCEOF]
      } yield m.name).toSet
    val m5SeqInt: Traversable[String] = benchMark("los5-new-Seq") {
      methodsLos5Seq.interpret()
    }
    methodsNative should equal (m5SeqInt)
  }

  // native Scala for-comprehension
  val methodsNative: Set[String] = benchMark("native") {
    for (cf <- testdata;
         m <- cf.methods;
         Code(_,_,instructions,_,_) <- m.attributes;
         INSTANCEOF(_) <- instructions) yield m.name
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
                             Code(_,_,instructions,_,_) <- m.attributes;
                             INSTANCEOF(_) <- instructions) yield m.name

      println(methodsLos1) //Fails because the terms are inadequate

      val m1Int: Traversable[String] = benchMark("los1") {
        methodsLos1.interpret()
      }
      methodsNative should equal (m1Int)
    }

    val methodsLos2 = queryData.flatMap( cf => cf.methods
      .flatMap( m => m.attributes
      .collect( x => x.ifInstanceOf[Code])
      .flatMap( c => c.instructions)
      .collect( i => i.ifInstanceOf[INSTANCEOF])
      .map( _ => m.name)))

    val m2Int: Traversable[String] = benchMark("los2") {
      methodsLos2.interpret()
    }
    methodsNative should equal (m2Int)

    //This is twice as slow as the other los solutions, except methodsLos2.
    //My guess is that this is because collect applies the given function twice.
    val methodsLos3 = queryData.flatMap( cf => cf.methods
      .flatMap( m => m.attributes
      .collect(
      a => onExp(a)('instanceOf$CodeAttribute,
        x =>
          if (x.isInstanceOf[Code])
            Some(x.asInstanceOf[Code])
          else None))
      .flatMap( c => c.instructions)
      .filter( a => onExp(a)('instanceOf$INSTANCEOF, _.isInstanceOf[INSTANCEOF]))
      .map( _ => m.name)))

    val m3Int: Traversable[String] = benchMark("los3") {
      methodsLos3.interpret()
    }
    methodsNative should equal (m3Int)

    //Best performance and quite clear code.
    val methodsLos4 =
      for {
        cf <- queryData
        m <- cf.methods
        a <- m.attributes
        if onExp(a)('instanceOf$CodeAttribute, _.isInstanceOf[Code])
        i <- a.asInstanceOf[Exp[Code]].instructions //This cast works perfectly
        if onExp(i)('instanceOf$INSTANCEOF, _.isInstanceOf[INSTANCEOF])
      } yield m.name
    val m4Int: Traversable[String] = benchMark("los4") {
      methodsLos4.interpret()
    }
    methodsNative should equal (m4Int)

    type ID[T] = T

    val methodsLos5 =
      for {
        cf <- queryData
        m <- cf.methods
        ca <- m.attributes.typeFilter[Code]
        io <- ca.instructions.typeFilter[INSTANCEOF]
      } yield m.name
    val m5Int: Traversable[String] = benchMark("los5") {
      methodsLos5.interpret()
    }
    methodsNative should equal (m5Int)

    val methodsLos5Seq =
      (for {
        cf <- queryData.toSeq
        m <- cf.methods
        ca <- m.attributes.typeFilter[Code]
        io <- ca.instructions.typeFilter[INSTANCEOF]
      } yield m.name).toSet
    val m5SeqInt: Traversable[String] = benchMark("los5-Seq") {
      methodsLos5Seq.interpret()
    }
    methodsNative should equal (m5SeqInt)

    //Let's consider type indexing.
    //First let's look at the desugared query
    val methodsLos5Desugared = queryData.flatMap(cf => cf.methods
      .flatMap(m => m.attributes.typeFilter[Code]
      .flatMap(ca => ca.instructions.typeFilter[INSTANCEOF]
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
    //Let coll0 be "m.attributes.typeFilter[Code]", coll1 be "ca", FindSubcolls[coll1] be "coll1.instructions", RestQuery[res] be res.map(io => m.name),
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
      ca <- m.attributes.typeFilter[Code]
      i <- ca.instructions
    } yield (m, i)
    benchMark("los6 index-base creation")(typeIdxBase.interpret())

    val typeIdxBaseSeq: Exp[Seq[PairMethodAnd[Instruction]]] = for {
      cf <- queryData.toSeq
      m <- cf.methods
      ca <- m.attributes.typeFilter[Code]
      i <- ca.instructions
    } yield (m, i)
    benchMark("los6 Seq-index-base creation")(typeIdxBaseSeq.interpret())
    //Let us accept some limited overhead with explicit type annotations to create the index
    //val typeindex = q.groupByType(_._2)
    val typeIdx = typeIdxBase.groupByTupleType2
    val typeIdxSeq = typeIdxBaseSeq.groupByTupleType2
    //This is normalization-by-evaluation over terms instead of functions (and who said it is limited to functions?)
    val evaluatedtypeindex: Exp[TypeMapping[Seq, PairMethodAnd, Instruction]] = //benchMark("los6 index creation"){ asExp(typeIdx.interpret()) } //XXX
      benchMark("los6 Seq-index creation"){ asExp(typeIdxSeq.interpret()) }
    //println(evaluatedtypeindex.map.keys)

    //val methodsLos6 = Const(evaluatedtypeindex).get[INSTANCEOF].map(_._1.name)
    //XXX this code, instead of using typeindex: Exp[...], uses toExp(typeindex.interpret()), which is a hack. The goal
    //is to have the index evaluated at all - otherwise there would be no speed-up. However, the proper solution is to
    //wrap the query result in something similar to IncrementalResult's constructor: Exp[T] -> Exp[T] - so that the result
    //is materialized and incrementally maintained.
    val methodsLos6 = evaluatedtypeindex.get[INSTANCEOF].map(_._1.name).toSet
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
    val m6Int: Traversable[String] = benchMark("los6 (with index)") {
      methodsLos6.interpret()
    }
    methodsNative should equal (m6Int)

    type QueryAnd[+T] = ((ClassFile, Method, Code), T);
    {
      //Same code as above, except that the index returns all free variables, so that the optimizer might find it.
      val typeIdxBase: Exp[Seq[QueryAnd[Instruction]]] = for {
        cf <- queryData.toSeq
        m <- cf.methods
        ca <- m.attributes.typeFilter[Code]
        i <- ca.instructions
      } yield (asExp((cf, m, ca)), i)

      val typeIdx = typeIdxBase.groupByTupleType2
      // Interpreting used to take a whopping 120 seconds. Why? Since the result is a set, each class file is being hashed once
      // per each instruction. The fix was adding toSeq above. In fact, this transformation could probably be done also on the query to optimize;
      // after that, the optimizer would again find a matching subquery. The new runtime is ~ 0.3-0.4 sec.
      val evaluatedtypeindex: Exp[TypeMapping[Seq, QueryAnd, Instruction]] = benchMark("los6 Seq-index (less manually optimized) creation"){ asExp(typeIdx.interpret()) }

      val methodsLos6 = evaluatedtypeindex.get[INSTANCEOF].map(_._1._2.name).toSet

      val m6Int: Traversable[String] = benchMark("los6 (with index, less manually optimized)") {
        methodsLos6.interpret()
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
    val evTypeIdx1: Exp[TypeMapping[Seq, PairMethodAnd, Attribute]] = benchMark("los7: creation of index typeIdx1"){ asExp(typeIdx1.interpret()) }
    //Hmm: this index inverts a 1-1 mapping, is that a good idea performance-wise?
    val idx1 = idx1Base.groupBySel(_._2, _._1)
    val idx1BaseWithSeqs = for {
      cf <- queryData.toSeq
      m <- cf.methods
      ca <- m.attributes
    } yield Seq(m, ca)
    val idx1WithSeqs = idx1BaseWithSeqs.groupBySel(_(1), _(0))
    val evIdx1 = benchMark("los7: creation of index idx1"){ asExp(idx1.interpret()) }
    val evIdx1WithSeqs = benchMark("los7: creation of index idx1 with seqs"){ asExp(idx1WithSeqs.interpret()) }

    type PairCodeAttributeAnd[+T] = (Code, T)
    /*
    val idx2Base = for {
      pair <- idx1Base
      ca <- pair._1.attributes.typeFilter[Code]
      i <- ca.instructions
    } yield (ca, i)
    val idx2 = idx2Base.groupByTupleType2
    val evIdx2: Exp[TypeMapping[Set, PairCodeAttribute, Instruction]] = idx2.interpret()
    */
    //val idx2BaseOpt = evIdx1.get[Code].flatMap(ca => ca.instructions.map(i => (ca, i)))
    val idx2BaseOpt /*: Exp[Set[PairCodeAttribute[Attribute]]]*/ = for {
      pair <- evTypeIdx1.get[Code]
      i <- pair._2.instructions
    } yield (pair._2, i)
    val typeIdx2Opt = idx2BaseOpt.groupByTupleType2
    val evTypeIdx2Opt: Exp[TypeMapping[Seq, PairCodeAttributeAnd, Instruction]] = benchMark("los7: creation of index typeIdx2Opt"){ asExp(typeIdx2Opt.interpret()) }

    val methodsLos7 = evTypeIdx2Opt.get[INSTANCEOF].flatMap(x => evIdx1(x._1).map(_.name))

    val m7Int: Traversable[String] = benchMark("los7 (with hierarchical indexing)") {
      methodsLos7.interpret().toSet
    }
    methodsNative should equal (m7Int)
  }

  test("testOpalWithIndexing-2") {
    //Code coming from TypeTests.
    type QueryAnd[+T] = ((ClassFile, Method), T);
    val typeIdxBase: Exp[Seq[QueryAnd[Instruction]]] = for {
      cf <- queryData.toSeq
      m <- cf.methods
      body <- m.body
      i <- body.instructions
    } yield (asExp((cf, m)), i)

    val typeIdx = typeIdxBase.groupByTupleType2
    val evaluatedtypeindex: Exp[TypeMapping[Seq, QueryAnd, Instruction]] = benchMark("los6 Seq-index (less manually optimized) creation, with fixed type indexing"){ asExp(typeIdx.interpret()) }

    val methodsLos6 = evaluatedtypeindex.get[INSTANCEOF].map(_._1._2.name).toSet

    val m6Int: Traversable[String] = benchMark("los6 (with index, less manually optimized, fixed type indexing)") {
      methodsLos6.interpret()
    }
    methodsNative should equal (m6Int)
  }
}
