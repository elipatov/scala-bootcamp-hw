package typeclass

object TypeclassTask {
  trait Hashable[T] {
    def hash(value: T): Int
  }

  object Hashable {
    def apply[T](implicit instance: Hashable[T]): Hashable[T] = instance
  }

  implicit class HashCodeSyntax[A: Hashable](x: A) {
    def hash: Int = Hashable[A].hash(x)
  }

  implicit val stringHash: Hashable[String] = str => str.hashCode
  "abc".hash
}


