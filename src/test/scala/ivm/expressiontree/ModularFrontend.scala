package ivm.expressiontree

trait ScalaIntf {
  this: Interface =>
  type Symantics <: ScalaSymantics
  trait ScalaSymantics {
    this: Symantics =>
    implicit def toIntOps(v: Rep[Int]): IntOps
    abstract class IntOps(v: Rep[Int]) {
      def +(that: Rep[Int]): Rep[Int]
    }
  }
}

trait Interface {
  type Symantics <: BaseSymantics
  trait BaseSymantics {
    type Rep[+T]
    implicit def unit[T](t: T): Rep[T]
  }
}

trait MyIntf extends Interface with ScalaIntf {
  type Symantics <: BaseSymantics with ScalaSymantics
}


// vim: set ts=4 sw=4 et:
