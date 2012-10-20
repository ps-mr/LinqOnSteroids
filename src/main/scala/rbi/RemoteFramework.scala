package rbi

import ivm.expressiontree.{Exp, LiftingTrait, LiftingLangIntf, BaseLangIntf, ClassTag, TypeTag}
import annotation.unchecked.uncheckedVariance
import java.net.InetAddress

/**
 * User: pgiarrusso
 * Date: 13/10/2012
 */

/*
//One possible interface:
trait SerializerFramework {
  type SerializationStream
  def toSerializationStream(i: InputStream): SerializationStream
  trait Serializable[T] {
    def writeOnStream(str: SerializationStream, value: T): Unit
    //Bad interface - does not represent easily the access mechanism of Java serialization, where
    //you deserialize an object and only later can figure out which object it was.
    def readFromStream(str: SerializationStream): T
  }
}
*/
trait SerializerFramework {
  type Serializator[T] <: SerializatorApi[T]
  trait SerializatorApi[T]
  type Address
}

trait JavaSerializerFramework extends SerializerFramework {
//  type Serialized[T] = Array[Byte]
  trait JavaSerializator[T] extends SerializatorApi[T] {
//    override def serialize(t: T): Array[Byte]
//    override def deserialize(ser: Array[Byte]): T
  }
  type Serializator[T] = JavaSerializator[T]
  override type Address = InetAddress
  implicit def SerializerFor[T <: java.io.Serializable] = new Serializator[T] {}
  implicit def SerializerForVal[T <: AnyVal] = new Serializator[T] {}
}

trait LanguageFramework {
  type LangIntf <: BaseLangIntf
}

trait ScalaLanguageFramework extends LanguageFramework {
  override type LangIntf = LiftingLangIntf
}

trait RemoteFramework {
  this: SerializerFramework with LanguageFramework =>
  type Host <: HostApi
  trait RepApi[+T, SelfHost] {
    def host: SelfHost
  }
  trait Liftable[T]
  implicit def isLiftable[T: ClassTag: TypeTag: Serializator]: Liftable[T] = ???
  trait HostApi {
    hostSelf: LangIntf =>
    protected val addr: Option[Address]
    type Rep[+T] >: AnywhereHost.Rep[T @uncheckedVariance]// <: RepApi[T, hostSelf.type]
    //type Rep[+T]// <: RepApi[T, hostSelf.type]
    implicit def pureTo[T: Liftable](value: T): Rep[T] = ???
  }
  implicit def toAnywhereRep[ThisHost <: Host, T: Serializator](value: ThisHost#Rep[T]): AnywhereHost.Rep[T] =
    moveTo(value, AnywhereHost)

  val AnywhereHost: Host /*{
    type Rep[+T] <: RepApi[T, AnywhereHost.type]
  }*/
  def moveTo[T: Serializator](value: Host#Rep[T], dest: Host): dest.Rep[T]
  //Scalac does not apply this conversion impicitly, for some reason. Possibly because it has a dependent method type,
  // unlike toAnywhereRep.
  implicit def move[T: Serializator](value: Host#Rep[T])(implicit dest: Host): dest.Rep[T] = moveTo(value, dest)
  //also takes a look at *TypeTag:
  //def in[U <: Universe with Singleton](otherMirror: MirrorOf[U]): U # AbsTypeTag[T]
  //Hm. I have no clue sure why they would want to use U# instead of passing u: U and writing u.AbsTypeTag[T]. But I also don't fully get mirrors.
}

trait ScalaRemoteFramework extends RemoteFramework with ScalaLanguageFramework {
  this: JavaSerializerFramework =>
  type Host <: HostApi
  trait HostApi extends LangIntf with super.HostApi
  def onHost[T](h1: Host)(body: h1.type => T): T = {
    body(h1)
  }
}

trait ScalaRemoteFrameworkTest extends ScalaRemoteFramework with JavaSerializerFramework {
  def test(h: Host)(v: h.Rep[Int]): h.Rep[Int] = {
    import h.{pure => _, _} //XXX we need to remove pure from the interface, and replace it with our pureTo. This
    // implies breaking tons of code. Rather make the type for constraints a separate typeclass
    v + 1
  }
  def test2(h1: Host, h2: Host)(v1: h1.Rep[Int], v2: h2.Rep[Int]) = {
    import h1.{pure => _, _}
    v1 + moveTo(v2, h1)
    //v1 + v2 //does not compile
  }
  def testImplicit(h1: Host, h2: Host)(v1: h1.Rep[Int], v2: h2.Rep[Int]) = {
    import h1.{pure => _, _}
    def toAnywhereRep = ??? //shadow implicit conversion just to test that it's necessary
    implicit val h1Impl: h1.type = h1 //The type annotation is necessary
    //v1 + v2 //fails to compile.
    //Wrappers for methods should incorporate the call to move, probably. That way, though, they get longer (because
    //of all the constraints they need to incorporate.
    v1 + move(v2)
  }
  def testImplicitAnywhere(h1: Host, h2: Host)(v1: h1.Rep[Int], v2: h2.Rep[Int]) = {
    import h1.{pure => _, _}
    v1 + v2 //compiles!!!
  }

  def testOnHost(h1: Host, h2: Host)(v1: h1.Rep[Int], v2: h2.Rep[Int]) = {
    import h1.{pure => _, _}
    onHost(h1){ implicit h1I =>
      v1 + move(v2)
    }
  }
}

object ScalaRemoteFrameworkImpl extends RemoteFramework with JavaSerializerFramework with ScalaLanguageFramework with ScalaRemoteFramework {
  type Host = HostApi
  class ConcreteHost(override protected val addr: Option[Address]) extends HostApi with LiftingLangIntf with LiftingTrait {
    self =>
    override type Rep[+T] = Exp[T]// with RepApi[T, self.type]
    //override implicit def pureTo[T: ClassTag: TypeTag: Serializator](value: T): Rep[T] = ???
  }
  private[this] def newHost(nHostAddr: Option[Address]): ConcreteHost = new ConcreteHost(nHostAddr)
  val AnywhereHost = newHost(None)
  override def moveTo[T: Serializator](value: Host#Rep[T], dest: Host): dest.Rep[T] = ???
}
