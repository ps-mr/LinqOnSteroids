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

  test("processedRecords should have the results") {
    println(processedRecordsQuery)
    processedRecordsQuery.interpret() should be (processedRecords)
    val processedRecordsQueryOpt = Optimization.optimize(processedRecordsQuery)
    println(processedRecordsQueryOpt)
    processedRecordsQueryOpt.interpret() should be (processedRecords)
  }
  //A query like processedRecordsQuery cannot really be optimized without unnesting! After that we need inlining,
  // which we have, and only then the delta-reduction rule for tuples can kick in.
  // If instead we use Result, we need delta-reduction to work on Result; Result needs to implement ExpProduct, and the
  // selectors need to implement ExpSelection; I guess for the latter I'd need to manually alter the generated code a
  // bit (for now).

  // On the other hand, I already understand unnesting.
  /*
  Force(MapOp(
    Filter(
      View(Force(
        FlatMap(
          Filter(
            View(Const(books)),
            v40 => Eq(Book_publisher1(v40),Const("Pearson Education"))),
          v41 => MapOp(
            Book_authors2(v41),
            v44 => LiftTuple3(
              Book_title0(v41),
              StringConcat(StringConcat(Author_firstName0(v44),Const(" ")),Author_lastName1(v44)),
              Plus(Size(Book_authors2(v41)),Negate(Const(1)))))))),
      v42 => Call2('StringOps$contains, Tuple3Proj1(v42), Const("Principles"))),
    v45 => LiftTuple2(Tuple3Proj1(v45),Tuple3Proj2(v45))))
    */
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

//Old example
trait SampleApp {
  case class Developer(name: String, website: String)
  case class LibraryVersion(version: String, depends: Set[Library], developers: Seq[String])

  //To introduce the need for a join, I altered this definition to refer to developers by name, instead of by a direct pointer to the developer itself. Maybe undo this.
  case class Library(name: String, versions: Set[LibraryVersion], users: Int)
}

trait SampleAppLifting extends SampleApp {
  //Code to be generated {{{
  implicit def expToDeveloperOps(t: Exp[Developer]) = new DeveloperOps(t)
  class DeveloperOps(t: Exp[Developer]) {
    def name = fmap(t)('Developer$name, _.name)
    def website = fmap(t)('Developer$website, _.website)
  }

  implicit def expToLibraryVersionOps(t: Exp[LibraryVersion]) = new LibraryVersionOps(t)
  class LibraryVersionOps(t: Exp[LibraryVersion]) {
    def version = fmap(t)('SampleApp$Library$version, _.version) //Remove class names from API! Use Manifests for that, and for arguments - but doesn't work for
    //parameterized types! Well, who cares when it's generated?
    def depends = fmap(t)('SampleApp$Library$depends, _.depends)
    def developers = fmap(t)('SampleApp$Library$developers, _.developers)
  }

  implicit def expToLibraryOps(t: Exp[Library]) = new LibraryOps(t)
  class LibraryOps(t: Exp[Library]) {
    def name = fmap(t)('Library$name, _.name)
    def versions = fmap(t)('Library$versions, _.versions)
    def users = fmap(t)('Library$users, _.users)
  }
  def Library(name: Exp[String], versions: Exp[Set[LibraryVersion]], users: Exp[Int]): Exp[Library] = fmap(name, versions, users)('Library, Library(_, _, _))
  //Code to be generated }}}
}

/*
class OopslaTutorialOld extends /*FunSuite with ShouldMatchers with TestUtil with*/ SampleApp {
  val libs: Set[Library] = Set.empty
  val idx = (for {
    lib <- libs //.asSmart
    ver <- lib.versions
  } yield (lib, ver)).groupBy {
    //libver => (libver._1, libver._2.version)
    case (lib, v@LibraryVersion(version, _, _)) => (lib, version)
  }

  for (lib <- libs) yield Library(lib.name, lib.versions, lib.users + 1)
}

class Foo extends OopslaTutorialOld with SampleAppLifting {
  val idxBase = for {
    lib <- libs.asSmart
    ver <- expToLibraryOps(lib).versions
  } yield (lib, ver)

  val idxSQuOpt = idxBase groupBy (
    libver => (libver._1, libver._2.version))

  for (lib <- libs.asSmart) yield
    Library(lib.name, lib.versions, lib.users + 1)

  //Query possibly to present
  /*
  (for {
    lib <- libs.asSmart
    ver <- lib.versions
    if ver.developers ==# Seq("Foo") /*boring*/ && ver.depends ==# Seq(lib) //We want to construct a sequence with lib, but let's not do it here.
  } yield (lib, asExp(Set(ver)))) groupBy (_._1) map (x => (x._1, /*x._2._1,*/ x._2.foldr(Set.empty)(_._2 union _._2) flatMap identity /*.flatten*/))
  */
  //But that's too cumbersome; moreover, it runs into some limitations, because folds are not yet lifted (and that's easy to fix) and
  //the yielded expression uses nested generic constructors and hence needs asExp.
}
*/
