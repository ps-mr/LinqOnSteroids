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

import dbschema._
class OopslaTutorial extends FunSuite with ShouldMatchers with TestUtil {
  //Having the import here does not work; we later import BookLiftingManual which shadows the original objects,
  //
  //import dbschema._
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

  val records = for {
    book <- books
    if book.publisher == "Pearson Education"
    author <- book.authors
  } yield Result(book.title, author.firstName + " " + author.lastName, /*Number of coauthors*/ book.authors.size - 1)

  def titleFilter(records: Set[Result], keyword: String): Set[(String, String)] = for {
    record <- records
    if record.title.contains(keyword)
  } yield (record.title, record.authorName)

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
      Result(book.title, author.firstName + " " + author.lastName, book.authors.size - 1)))

  test("recordsDesugared should be records") {
    recordsDesugared should be (records)
    recordsOld should not be (records)
  }

  val idxByAuthor = records.groupBy(_.authorName) //Index books by author - the index by title is a bit more boring, but not so much actually!

  import dbschema.squopt._

  //But the correct index by title should be:
  val idxByTitle = books.groupBy(_.title)

  val idxByPublisher =
    books.asSmart indexBy (_.publisher)

  val doIndex = true //Disable this to test other optimizations, like unnesting
  if (doIndex)
    Optimization.addIndex(idxByPublisher)

  val recordsQuery = for {
   book <- books.asSmart
   if book.publisher ==# "Pearson Education"
   author <- book.authors
 } yield Result(book.title,
    author.firstName + " " + author.lastName,
    book.authors.size - 1)

  Util.assertType[Exp[Set[Result]]](recordsQuery)

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
    showExp(recordsQuery, "recordsQuery")
    recordsQuery.interpret() should be (records)
    val recordsQueryOpt = Optimization.optimize(recordsQuery)
    showExp(recordsQueryOpt, "recordsQueryOpt")
    recordsQueryOpt.interpret() should be (records)
    //The call to optimize is needed to get the same query as recordsQueryOpt for two reasons:
    //- we need to replace the index with the preevaluated one (not with an equal collection, but the same one)
    //- optimize also normalizes some details of the query.
    //  For instance reassociation transforms book.authors.size - 1 to (-1) + book.authors.size.
    //  This normalization allows more effective constant folding - see Optimization.buildSum and its comment.
    val indexedQuery =
      (for {
        book <- idxByPublisher("Pearson Education")
        author <- book.authors
      } yield Result(book.title, author.firstName + " " + author.lastName, book.authors.size - 1)).optimize
    showExp(indexedQuery, "indexedQuery")
    if (doIndex)
      recordsQueryOpt should be (indexedQuery)
  }

  def titleFilterQuery(records: Exp[Set[Result]], keyword: String): Exp[Set[(String, String)]] = for {
    record <- records
    if record.title.contains(keyword)
  } yield (record.title, record.authorName)

  val processedRecordsQuery = titleFilterQuery(recordsQuery, "Principles")

  //We lift the hand-optimized version to verify that the optimizer produces the same code.
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
    if (!doIndex)
      processedRecordsQueryOpt should be (processedQueryExpectedOptimRes)
  }

  //XXX do we actually need to change the type of keyword? Can't we just use Exp[String] like in the paper?
  //Optimize the query before specifying the keyword to lookup.
  //We first modify the type of titleFilterQuery, in particular the type of its `keyword` parameter.
  //Before, `keyword` had type String instead of Exp[String], because the code worked
  //either way.
  //For this example, instead, keyword must have type Exp[String], like in the paper.
  def titleFilterQuery2(records: Exp[Set[Result]], keyword: Exp[String]): Exp[Set[(String, String)]] = for {
    record <- records
    if record.title.contains(keyword)
  } yield (record.title, record.authorName)

  val processedRecordsQueryOptFun = Fun((kw: Exp[String]) => titleFilterQuery2(recordsQuery, kw)).optimize
  Util.assertType[Exp[String => Set[(String, String)]]](processedRecordsQueryOptFun)
  //XXX In the paper this is lookupFunction.
  val processedRecordsQueryOptRes = processedRecordsQueryOptFun("Principles")

  test("processedRecords should have the same results as the lifted function version") {
    processedRecordsQueryOptRes.interpret() should be (processedRecords)
    showExp(processedRecordsQueryOptRes, "processedRecordsQueryOptRes")
    processedRecordsQueryOptRes.interpret() should be (processedRecords)
    if (!doIndex)
      processedRecordsQueryOptRes should be (processedQueryExpectedOptimRes)
  }

  // To optimize processedRecordsQuery, we need query unnesting, inlining, and only then the delta-reduction rule for case classes can kick in.
  // To this end, ResultExp needs to implement ExpProduct, and the
  // selectors need to implement ExpSelection.
  // Possible idea: after all, case classes even implement Product themselves, hence one might maybe reuse the lifting of tuples;
  // probably that would work when case classes will implement ProductX.
}
