package ivm
package tests

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import expressiontree._
import optimization.Optimization
import Lifting._

/**
 * User: pgiarrusso
 * Date: 26/03/2012
 */

//New example, discussed with Christian.

import sampleapp._
class OopslaTutorial extends FunSuite with ShouldMatchers with TestUtil {
  //Having the import here does not work; we later import BookLiftingManual which shadows the original objects,
  //
  //import sampleapp._
  val books: Set[Book] = Set(Book("Compilers: Principles, Techniques, and Tools", "Pearson Education", Seq(Author("Alfred V.", "Aho"), Author("Monica S.", "Lam"), Author("Ravi", "Sethi"), Author("Jeffrey D.", "Ullman"))))
  val recordsOld = for {
    book <- books
    if book.publisher == "ACM"
    author <- book.authors
  } yield (book.title, author.firstName + " " + author.lastName, /*Number of coauthors*/ book.authors.size - 1)

  val processedRecordsOld = for {
    record <- recordsOld
    if record._1.startsWith("Compilers")
  } yield (record._1, record._2)

  type Result2 = (String, String, Int)

  val records = for {
    book <- books
    if book.publisher == "Pearson Education"
    author <- book.authors
//  } yield Result(book.title, author.firstName + " " + author.lastName, /*Number of coauthors*/ book.authors.size - 1)
  } yield (book.title, author.firstName + " " + author.lastName, /*Number of coauthors*/ book.authors.size - 1)

  //def titleFilter(records: Set[Result], keyword: String): Set[(String, String)] = for {
  def titleFilter(records: Set[Result2], keyword: String): Set[(String, String)] = for {
    record <- records
      if record._1.contains(keyword)
  } yield (record._1, record._2)
//    if record.title.contains(keyword)
//  } yield (record.title, record.authorName)

  val processedRecords = titleFilter(records, "Principles")

  def titleFilterHandOpt1(books: Set[Book], publisher: String, keyword: String) = for {
    book <- books
    if book.publisher == publisher
    author <- book.authors
    if book.title.contains(keyword)
  } yield (book.title, author.firstName + " " + author.lastName)
  val processedRecordsOpt1 = titleFilterHandOpt1(books, "Pearson Education", "Principles")

  def titleFilterHandOpt2(books: Set[Book], publisher: String, keyword: String) =
    for {
      book <- books
      if book.publisher == publisher
      if book.title.contains(keyword)
      author <- book.authors
    } yield (book.title, author.firstName + " " + author.lastName)
  val processedRecordsOpt2 = titleFilterHandOpt2(books, "Pearson Educationrson Education", "Principles")

  val recordsDesugared = books.withFilter(book =>
    book.publisher == "Pearson Education").flatMap(book =>
    book.authors.map(author =>
//      Result(book.title, author.firstName + " " + author.lastName, book.authors.size - 1)))
      (book.title, author.firstName + " " + author.lastName, book.authors.size - 1)))

  test("recordsDesugared should be records") {
    recordsDesugared should be (records)
    recordsOld should not be (records)
  }

  //val idxByAuthor = records.groupBy(_.authorName) //Index books by author - the index by title is a bit more boring, but not so much actually!
  val idxByAuthor = records.groupBy(_._2) //Index books by author - the index by title is a bit more boring, but not so much actually!

  import BookLifting._
  import BookLiftingManual._

  //But the correct index by title should be:
  val idxByTitle = books.groupBy(_.title)

  val idxByPublisher =
    books.asSmart groupBy (_.publisher)

  val doIndex = false //Disable this to test other optimizations, like unnesting
  if (doIndex)
    Optimization.addIndex(idxByPublisher)

  val recordsQuery = for {
   book <- books.asSmart
   if book.publisher ==# "Pearson Education"
   author <- book.authors
 } yield (book.title,
    author.firstName + " " + author.lastName,
    book.authors.size - 1)

  val recordsQuery2: Exp[Set[(String, Set[String], Int)]] = for {
    book <- books.asSmart
    if book.publisher ==# "Pearson Education"
    author <- book.authors
  } yield (book.title,
      //Sets are invariant! Hence we can't convert a set of StringConcat to a set of Exp[String] (which is due to our framework)
      //and we can't convert a Set[Exp[String]] to Set[Exp[AnyRef]] (which is independent of our framework, since Set[String] cannot be
      //converted to Set[AnyRef]).
      asExp(Set(author.firstName + " " + author.lastName)),
      book.authors.size - 1)

  test("same results") {
    println(recordsQuery)
    recordsQuery.interpret() should be (records)
    val recordsQueryOpt = Optimization.optimize(recordsQuery)
    println(recordsQueryOpt)
    recordsQueryOpt.interpret() should be (records)
  }

  def titleFilterQuery(records: Exp[Set[Result2]], keyword: String): Exp[Set[(String, String)]] = for {
    record <- records
    if record._1.contains(keyword)
  } yield (record._1, record._2)

  val processedRecordsQuery = titleFilterQuery(recordsQuery, "Principles")

  def titleFilterHandOpt2Query(books: Exp[Set[Book]], publisher: String, keyword: String): Exp[Set[(String, String)]] =
    for {
      book <- books
      if book.publisher ==# publisher && book.title.contains(keyword)
      author <- book.authors
    } yield (book.title, author.firstName + " " + author.lastName)
  val processedQueryExpectedOptimRes = titleFilterHandOpt2Query(books, "Pearson Education", "Principles")

  test("processedRecords should have the same results as the lifted version") {
    showExp(processedRecordsQuery, "processedRecordsQuery")
    processedRecordsQuery.interpret() should be (processedRecords)
    val processedRecordsQueryOpt = Optimization.optimize(processedRecordsQuery)
    showExp(processedRecordsQueryOpt, "processedRecordsQueryOpt")
    processedRecordsQueryOpt.interpret() should be (processedRecords)
    //showExp(processedQueryExpectedOptimRes, "processedQueryExpectedOptimRes")
    processedRecordsQueryOpt should be (processedQueryExpectedOptimRes)
  }

  //keyword _must_ be Exp[String].
  def titleFilterQuery2(records: Exp[Set[Result2]])(keyword: Exp[String]): Exp[Set[(String, String)]] = for {
    record <- records
    if record._1.contains(keyword)
  } yield (record._1, record._2)

  //Optimize the query before specifying the keyword to lookup.
  val processedRecordsQueryOptFun = asExp(titleFilterQuery2(recordsQuery) _).optimize
  Util.assertType[Exp[String => Set[(String, String)]]](processedRecordsQueryOptFun)
  val processedRecordsQueryOptRes = processedRecordsQueryOptFun("Principles")

  test("processedRecords should have the same results as the lifted function version") {
    processedRecordsQueryOptRes.interpret() should be (processedRecords)
    showExp(processedRecordsQueryOptRes, "processedRecordsQueryOptRes")
    processedRecordsQueryOptRes.interpret() should be (processedRecords)
    processedRecordsQueryOptRes should be (processedQueryExpectedOptimRes)
  }

  //A query like processedRecordsQuery cannot really be optimized without unnesting! After that we need inlining,
  // which we have, and only then the delta-reduction rule for tuples can kick in.
  // If instead we use Result, we need delta-reduction to work on Result; Result needs to implement ExpProduct, and the
  // selectors need to implement ExpSelection; I guess for the latter I'd need to manually alter the generated code a
  // bit (for now), but it is surely possible to recognize case classes (deployed software does it) and add ExpProduct
  // and ExpSelection for them.
  // Possible idea: after all, case classes even implement Product themselves! Might be helpful.
}


object BookLiftingManual {
  //case class Result(title: Exp[String], authorName: Exp[String], coauthors: Exp[Int])
  import sampleapp._
  case class ResultExp(title: Exp[String], authorName: Exp[String], coauthors: Exp[Int]) extends Arity3Op[Exp[String], Exp[String], Exp[Int], Result, ResultExp](title, authorName, coauthors) {
    def copy(title: Exp[String], authorName: Exp[String], coauthors: Exp[Int]) = ResultExp(title, authorName, coauthors)
    def interpret() = sampleapp.Result(title.interpret(), authorName.interpret(), coauthors.interpret())
  }
  def Result(title: Exp[String], authorName: Exp[String], coauthors: Exp[Int]) = ResultExp(title, authorName, coauthors)
}

