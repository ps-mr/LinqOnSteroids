package ivm.collections


// contract: map must map a ClassManifest[T] to a C[T]
class TypeMapping[C[_] <: Traversable[_]](val map: Map[ClassManifest[_],C[_]]) {
   def get[T](implicit tmf: ClassManifest[T]) : C[T] = map(tmf).asInstanceOf[C[T]]
}

