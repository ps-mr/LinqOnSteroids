package ivm
package opaltests

import expressiontree.{Lifting, Exp}
import Lifting._

import de.tud.cs.st.bat.resolved
import resolved.reader.Java6Framework
import resolved._
import Java6Framework.ClassFile

/**
 * User: pgiarrusso
 * Date: 1/11/2011
 */

class EvalOpalTests {
  import OpalTestData._
  import BATLifting._

  //Translate examples from paper:
  //Michael Eichberg, Sven Kloppenburg, Karl Klose and Mira Mezini,
  //"Defining and Continuous Checking of Structural Program Dependencies", ICSE '08.
  def continuousCheckingPaper() {
    def sourceElems(Types: Exp[Set[ClassFile]]) = Types union (Types flatMap (_.methods)) union (Types flatMap (_.fields))

    //Listing 3:
    //Working alternatives, to get Exp[Set[ClassFile]] as result type:
    //val Types = queryData filter (classFile => classFile.thisClass.packageName === "bat.type")
    val Types = queryData withFilter (classFile => classFile.thisClass.packageName === "bat.type") force
    //Non-working alternatives:
    /*
    val Types = (for {
      classFile <- queryData
      if classFile.thisClass.packageName === "bat.type"
    } yield classFile) force
    */
    //This desugars to:
    //val Types = queryData withFilter (classFile => classFile.thisClass.packageName === "bat.type") map identity force

    //And the problem is in the parameter for the call to map - since CanBuildFrom instances are not available very
    //precisely on TraversableView[T, Repr] (for different Repr). Maybe it'll be enough to have some for the non-view
    //version of the collection (Repr), at least surely for the type; we can then reuse
    //TraversableView/SeqView.canBuildFrom.

    val TypesEnsemble = sourceElems(Types)
    val TypesFlyweightFactoryEnsemble = sourceElems(queryData filter (x => x.thisClass.packageName === "bat.type" && x.thisClass.simpleName === "TypeFactory"))
    val tmp = queryData filter (x => x.thisClass.packageName === "bat.type" && x.thisClass.simpleName === "IType")
    val TypesFlyweightCreationEnsemble = for {
      classFile <- queryData
      cf2 <- tmp
      ancestorInterface <- classFile.interfaces
      if ancestorInterface === cf2 //we do need a contains method!
      method <- classFile.methods
    } yield method: AnyRef //Generalize type so that anything can be tested for membership

    //Listing 4:
    val uses: Exp[Set[(AnyRef, AnyRef)]] = null //Set.empty
    //Without Exp[Set].apply = Exp[Set].contains
    /*
    for {
      el <- uses
      val (s, t) = unliftPair(el)
      t1 <- TypesFlyweightCreationEnsemble
      if t === t1
      //I'm stuck here, since I can't express !(a contains b)
    } yield el
    //Use lifted Exp[Set].apply
    for {
      el <- uses
      val (s, t) = unliftPair(el)
      if TypesFlyweightCreationEnsemble(t)
      if !TypesFlyweightFactoryEnsemble(s)
    } yield el
    */
    //Rewrite to avoid using ugly unliftPair or non-working value definitions, which rely on non-working pattern
    //matching
    for {
      el <- uses
      if TypesFlyweightCreationEnsemble(el._2)
      if !TypesFlyweightFactoryEnsemble(el._1)
    } yield el
    //This query is an anti-join between sets. Can it be computed more quickly than with a single loop?
    // There's a VLDB paper about algorithms for fast set intersection - the natural lower boundary is the
    // result size, not the sum of the input sizes
    //An alternative rewriting would be this one:
    for {
      t <- TypesFlyweightCreationEnsemble
      el <- for (el <- uses if el._2 === t) yield el
      if !TypesFlyweightFactoryEnsemble(el._1)
    } yield el
  }
}
