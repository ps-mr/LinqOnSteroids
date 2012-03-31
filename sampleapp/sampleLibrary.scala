package sampleapp
case class Author(firstName: String, lastName: String)
case class Book(title: String, publisher: String, authors: Seq[Author])
case class Result(title: String, authorName: String, coauthors: Int)
// vim: set ts=4 sw=4 et:
