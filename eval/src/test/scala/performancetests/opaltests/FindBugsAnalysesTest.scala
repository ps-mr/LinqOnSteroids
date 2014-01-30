package performancetests
package opaltests

import org.scalatest.{FunSuiteLike, BeforeAndAfterAll}
import org.scalatest.matchers.ShouldMatchers

class FindBugsAnalysesTest
  extends FindBugsAnalyses(FBConfig(zipFiles = List("src/test/resources/Bugs.zip"), debugBench = Benchmarking.debugBench))
          with FunSuiteLike with BeforeAndAfterAll with ShouldMatchers {
  test("BX_BOXING_IMMEDIATELY_UNBOXED_TO_PERFORM_COERCION") {
    analyzeBOXING_IMMEDIATELY_UNBOXED_TO_PERFORM_COERCION()
  }

  test("DMI_LONG_BITS_TO_DOUBLE_INVOKED_ON_INT") {
    analyzeDMI_LONG_BITS_TO_DOUBLE_INVOKED_ON_INT()
  }

  test("DP_DO_INSIDE_DO_PRIVILEGED") {
    analyzeDP_DO_INSIDE_DO_PRIVILEGED()
  }

  test("FI_USELESS") {
    analyzeFI_USELESS()
  }

  test("ITA_INEFFICIENT_TO_ARRAY") {
    analyzeITA_INEFFICIENT_TO_ARRAY()
  }


  test("MS_PKGPROTECT") {
    analyzeMS_PKGPROTECT()
  }

  test("MS_SHOULD_BE_FINAL") {
    analyzeMS_SHOULD_BE_FINAL()
  }

  test("SE_BAD_FIELD_INNER_CLASS") {
    analyzeSE_BAD_FIELD_INNER_CLASS()
  }

  test("SIC_INNER_SHOULD_BE_STATIC_ANON") {
    analyzeSIC_INNER_SHOULD_BE_STATIC_ANON()
  }

  test("SW_SWING_METHODS_INVOKED_IN_SWING_THREAD") {
    analyzeSW_SWING_METHODS_INVOKED_IN_SWING_THREAD()
  }

  test("UR_UNINIT_READ_CALLED_FROM_SUPER_CONSTRUCTOR") {
    analyzeUR_UNINIT_READ_CALLED_FROM_SUPER_CONSTRUCTOR()
  }




  test("ProtectedField") {
    analyzeProtectedFields()
  }

  test("UnusedFields") {
    analyzeUnusedFields()
  }

  test("ExplicitGC") {
    analyzeExplicitGC()
  }

  test("PublicFinalizer") {
    analyzePublicFinalizer()
  }

  test("PublicFinalizer2") {
    analyzePublicFinalizer2()
  }

  test("SerializableNoConstructor") {
    analyzeSerializableNoConstructor()
  }

  test("CatchIllegalMonitorStateException") {
    analyzeCatchIllegalMonitorStateException()
  }

  test("CovariantCompareToMethods") {
    analyzeCovariantCompareToMethods()
  }

  test("AbstractClassesThatDefinesCovariantEquals") {
    analyzeAbstractClassesThatDefinesCovariantEquals()
  }

  test("MethodsThatCallRunFinalizersOnExit") {
    analyzeMethodsThatCallRunFinalizersOnExit()
  }

  test("CloneableNoClone") {
    analyzeCloneableNoClone()
  }

  test("SuperCloneMissing") {
    analyzeCloneDoesNotCallSuperClone()
  }

  test("NotCloneable") {
    analyzeCloneButNotCloneable()
  }



  override def afterAll() {
    tearDownIndexes()
  }
}
