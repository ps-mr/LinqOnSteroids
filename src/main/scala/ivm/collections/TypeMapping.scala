package ivm.collections


// contract: map must map a ClassManifest[T] to a C[T]
class TypeMapping[C[X] <: Traversable[X], D[_]](val map: Map[ClassManifest[_],C[D[_]]]) {
   def get[T](implicit tmf: ClassManifest[T]): C[D[T]] = map(tmf).asInstanceOf[C[D[T]]]
}

