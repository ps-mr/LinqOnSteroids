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
    def version = onExp(t)('SampleApp$Library$version, _.version)
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

  val idxSQuOpt = idxBase.groupBy(
    libver => (libver._1, libver._2.version))

  for (lib <- libs.asSmartCollection) yield
    Library(lib.name, lib.versions, lib.users + 1)
}
