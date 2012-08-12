package ivm
package tests

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import optimization.Optimization
import expressiontree.Util
import expressiontree.{ClassTag, TypeTag}

/**
 * User: pgiarrusso
 * Date: 26/03/2012
 */

//New example, discussed with Christian.

//Import most definitions from Figure 1 - Definition of the schema and of content
import schema.{squopt => _, _}
class PaperTutorial extends FunSuite with ShouldMatchers with TestUtil {
  //Rest of Figure 1 - Definition of the schema and of content
  val books: Set[Book] = Set(Book("Compilers: Principles, Techniques, and Tools", "Pearson Education", Seq(Author("Alfred V.", "Aho"), Author("Monica S.", "Lam"), Author("Ravi", "Sethi"), Author("Jeffrey D.", "Ullman"))))

  //Figure 2 - Query on schema in Fig. 1
  val records = for {
    book <- books
    if book.publisher == "Pearson Education"
    author <- book.authors
  } yield Result(book.title, author.firstName + " " + author.lastName, /*Number of coauthors*/ book.authors.size - 1)

  //Figure 3 - Further processing of the query in Fig. 2
  def titleFilter(records: Set[Result], keyword: String): Set[(String, String)] = for {
    record <- records
    if record.title.contains(keyword)
  } yield (record.title, record.authorName)

  val processedRecords = titleFilter(records, "Principles")

  //Figure 4. Hand-optimized composition of Fig. 2 and 3
  def titleFilterHandOpt1(books: Set[Book], publisher: String, keyword: String) = for {
    book <- books
    if book.publisher == publisher
    author <- book.authors
    if book.title.contains(keyword)
  } yield (book.title, author.firstName + " " + author.lastName)
  val processedRecordsOpt1 = titleFilterHandOpt1(books, "Pearson Education", "Principles")

  //Figure 5. Hoisting the filtering step of Fig. 4
  def titleFilterHandOpt2(books: Set[Book], publisher: String, keyword: String) =
    for {
      book <- books
      if book.publisher == publisher
      if book.title.contains(keyword)
      author <- book.authors
    } yield (book.title, author.firstName + " " + author.lastName)
  val processedRecordsOpt2 = titleFilterHandOpt2(books, "Pearson Educationrson Education", "Principles")

  //Figure 8. Desugaring of code in Fig. 2.
  val recordsDesugared = books.withFilter(book =>
    book.publisher == "Pearson Education").flatMap(book =>
    book.authors.map(author =>
      Result(book.title, author.firstName + " " + author.lastName, book.authors.size - 1)))

  //Test for Fig. 8
  test("recordsDesugared should be records") {
    recordsDesugared should be (records)
  }

  val idxByAuthor = records.groupBy(_.authorName) //Index books by author - the index by title is a bit more boring, but not so much actually!

  //Figure 6. Reified query in SQUOPT
  import squopt.imports._
  import schema.squopt._

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

  //But the correct index by title should be:
  val idxByTitle = books.groupBy(_.title)

  //From Section 3.3
  val idxByPublisher =
    books.asSmart indexBy (_.publisher)

  val doIndex = true //Disable this to test other optimizations, like unnesting, instead of indexing
  if (doIndex)
    Optimization.addIndex(idxByPublisher)

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

  //Figure 7. SQUOPT version of Fig. 3
  def titleFilterQuery(records: Exp[Set[Result]], keyword: Exp[String]): Exp[Set[(String, String)]] = for {
    record <- records
    if record.title.contains(keyword)
  } yield (record.title, record.authorName)

  val processedRecordsQuery = titleFilterQuery(recordsQuery, "Principles")
  //Figure 7 end. The result is checked below.

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

  //As shown at the end of Sec. 3.2, optimize the query before specifying the keyword to lookup.
  val lookupFunction = Fun((keyword: Exp[String]) => titleFilterQuery(recordsQuery, keyword)).optimize
  Util.assertType[Exp[String => Set[(String, String)]]](lookupFunction)
  val processedRecordsQueryOptRes = lookupFunction("Principles")

  //... and test that it works:
  test("processedRecords should have the same results as the lifted function version") {
    processedRecordsQueryOptRes.interpret() should be (processedRecords)
    showExp(processedRecordsQueryOptRes, "processedRecordsQueryOptRes")
    processedRecordsQueryOptRes.interpret() should be (processedRecords)
    if (!doIndex)
      processedRecordsQueryOptRes should be (processedQueryExpectedOptimRes)
    //if doIndex, the expression tree will have another form.
  }

  val measurements = Seq(1.5, 3.0)

  //From Sec. IV
  abstract class DataProcessingStep {
    def process(values: Exp[Seq[Double]]): Exp[Seq[Double]]
  }
  class ADataProcessingStepImpl extends DataProcessingStep {
    override def process(values: Exp[Seq[Double]]) =
      for (i <- values) yield i + 1 // /values.sum
  }

  class Client(processor: DataProcessingStep) {
    def toPercentage(data: Exp[Seq[Double]]): Exp[Seq[Double]] =
      for {
        j <- processor.process(data)
      } yield j * 100
  }

  val query = new Client(new ADataProcessingStepImpl).toPercentage(measurements)

  test("foo") {
    println(query)
    val optim = query.optimize
    println(optim)
    val res = query.interpret()
    println(res)
    query.optimize.interpret() should be (res)
  }
}
