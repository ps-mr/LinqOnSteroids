package ivm
package tests

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import expressiontree.{Exp, Lifting}
import Lifting._

/**
 * User: pgiarrusso
 * Date: 26/03/2012
 */

//New example, discussed with Christian.
object SampleLibrary {
  case class Author(firstName: String, lastName: String)
  case class Book(title: String, publisher: String, authors: Seq[Author]) //price: Decimal ?
}

object SampleLibraryLifting {
  //XXX to gen.
}

class SampleQuery extends FunSuite with ShouldMatchers with TestUtil {
  import SampleLibrary._
  val books: Set[Book] = Set.empty
  val records = for {
    book <- books
    if book.publisher == "ACM"
    author <- book.authors
  } yield (book.title, author.firstName + " " + author.lastName, /*Number of coauthors*/ book.authors.size - 1)

  val processedRecords = for {
    record <- records
    if record._1.startsWith("The")
  } yield (record._1, record._2)

  val recordsDesugared = books.withFilter(book =>
    book.publisher == "ACM").flatMap(book =>
    book.authors.map(author =>
      (book.title, author.firstName + " " + author.lastName, book.authors.size - 1)))

  test("recordsDesugared should be records") {
    recordsDesugared should be (records)
  }

  val idxByAuthor = records.groupBy(_._2) //Index books by author - the index by title is a bit more boring, but not so much actually!
  //But the correct index by title should be:
  val idxByTitle = (for {
    book <- books
  } yield (book, book.title)) groupBy (_._2)

  val idxByPublisher = (for {
    book <- books
  } yield (book, book.publisher)) groupBy (_._2)

  import SampleLibraryLifting._
  for {
    book <- books.asSmartCollection
  } yield book
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
    def name = onExp(t)('Developer$name, _.name)
    def website = onExp(t)('Developer$website, _.website)
  }

  implicit def expToLibraryVersionOps(t: Exp[LibraryVersion]) = new LibraryVersionOps(t)
  class LibraryVersionOps(t: Exp[LibraryVersion]) {
    def version = onExp(t)('SampleApp$Library$version, _.version) //Remove class names from API! Use Manifests for that, and for arguments - but doesn't work for
    //parameterized types! Well, who cares when it's generated?
    def depends = onExp(t)('SampleApp$Library$depends, _.depends)
    def developers = onExp(t)('SampleApp$Library$developers, _.developers)
  }

  implicit def expToLibraryOps(t: Exp[Library]) = new LibraryOps(t)
  class LibraryOps(t: Exp[Library]) {
    def name = onExp(t)('Library$name, _.name)
    def versions = onExp(t)('Library$versions, _.versions)
    def users = onExp(t)('Library$users, _.users)
  }
  def Library(name: Exp[String], versions: Exp[Set[LibraryVersion]], users: Exp[Int]): Exp[Library] = onExp(name, versions, users)('Library, Library(_, _, _))
  //Code to be generated }}}
}

class OopslaTutorial extends FunSuite with ShouldMatchers with TestUtil with SampleApp {
  val libs: Set[Library] = Set.empty
  val idx = (for {
    lib <- libs //.asSmartCollection
    ver <- lib.versions
  } yield (lib, ver)).groupBy {
    //libver => (libver._1, libver._2.version)
    case (lib, v@LibraryVersion(version, _, _)) => (lib, version)
  }

  for (lib <- libs) yield Library(lib.name, lib.versions, lib.users + 1)
}

class Foo extends OopslaTutorial with SampleAppLifting {
  val idxBase = for {
    lib <- libs.asSmartCollection
    ver <- expToLibraryOps(lib).versions
  } yield (lib, ver)

  val idxSQuOpt = idxBase groupBy (
    libver => (libver._1, libver._2.version))

  for (lib <- libs.asSmartCollection) yield
    Library(lib.name, lib.versions, lib.users + 1)

  //Query possibly to present
  /*
  (for {
    lib <- libs.asSmartCollection
    ver <- lib.versions
    if ver.developers ==# Seq("Foo") /*boring*/ && ver.depends ==# Seq(lib) //We want to construct a sequence with lib, but let's not do it here.
  } yield (lib, asExp(Set(ver)))) groupBy (_._1) map (x => (x._1, /*x._2._1,*/ x._2.foldr(Set.empty)(_._2 union _._2) flatMap identity /*.flatten*/))
  */
  //That's rather cumbersome. Let's ask for help.
}
