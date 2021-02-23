package typeclass

object Task1 {
  final case class Money(amount: BigDecimal)

  implicit val moneyOrdering: Ordering[Money] = (x: Money, y: Money) => x.amount.compare(y.amount)
}

object Task2 {
  trait Show[T] { // fancy toString
    def show(entity: T): String
  }

  final case class User(id: String, name: String)

  implicit val showUser: Show[User] = usr => s"ID: ${usr.id} | Name: ${usr.name}"

  object Show {
    def apply[T](implicit instance: Show[T]): Show[T] = instance
  }

  implicit class UserEx[A: Show](x: A) {
    def show: String = Show[A].show(x)
  }

  User("1", "Oleg").show
}

object Task3 {
  type Error = String
  trait Parse[T] { // invent any format you want or it can be csv string
    def parse(entity: String): Either[Error, T]
  }

  final case class User(id: String, name: String)

  implicit val parseUser: Parse[User] = str => {
    str.split(',').toList match {
      case id :: name :: _ => Right(User(id, name))
      case _ => Left(s"'$str' is not valid")
    }
  }

  object Parse {
    def apply[T](implicit instance: Parse[T]) = instance
  }

  implicit class StringExt(str: String) {
    def parse[T: Parse]: Either[Error, T] = Parse[T].parse(str)
  }

  "lalala".parse[User]
}

object Task4 {
  // TODO: design a typesafe equals so i can do a === b, but it won't compile
  //  if a and b are of different types
  // define the typeclass (think of a method signature)
  // remember `a method b` is `a.method(b)`

  trait Comparable[T] {
    def equals(x1: T, x2: T): Boolean
  }

  object Comparable {
    def apply[T](implicit instance: Comparable[T]) = instance
  }

  implicit class ComparableExt[T: Comparable](s1: T) {
    def ===(s2: T): Boolean = Comparable[T].equals(s1, s2)
  }

  implicit val compareAny: Comparable[Any] = (v1, v2) => v1.equals(v2)
  implicit val compareStr: Comparable[String] = (s1, s2) => s1.equals(s2)
  val boolStr = "asd" === "asd"

  trait User
  final case class SuperUser() extends User
  final case class RegularUser() extends User

  implicit val compareSU: Comparable[SuperUser] = (u1, u2) => u1.equals(u2)
  val boolSu = SuperUser() === SuperUser()

  implicit val compareRU: Comparable[RegularUser] = (u1, u2) => u1.equals(u2)
  val boolRu = RegularUser() === RegularUser()

  implicit val compareUser: Comparable[User] = (u1, u2) => {
    u1.equals(u2)
  }

  // Won't compile
  //val boolUser = SuperUser() === RegularUser()
}

object AdvancedHomework {
  // TODO: create a typeclass for flatMap method
  trait FlatMap[T[_]] {
    def flatMap[A, B](value: T[A], fn: A => B): T[B]
  }

  object FlatMap {
    def apply[T[_]: FlatMap](implicit instance: FlatMap[T]) = instance
  }

  implicit class FlatMapExt[T[_]: FlatMap, A](value: T[A]) {
    def flatMap[B](fn: A => B): T[B] = FlatMap[T].flatMap(value, fn)
  }

  case class Box[T](value: T)
  implicit val flatMapBox: FlatMap[Box] = new FlatMap[Box] {
    override def flatMap[A, B](obj: Box[A], fn: A => B): Box[B] = Box(fn(obj.value))
  }

  val intBox = Box(123)
  val stringBox = intBox.flatMap[String](x => x.toString)
}
