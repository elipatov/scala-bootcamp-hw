package typeclass

object QAndAExamples {

  // 1. Semigroup
  // 1.1. Implement all parts of the typeclass definition
  trait Semigroup[A] {
    def combine(x: A, y: A): A
  }

  object Semigroup {
    def apply[A](implicit instance: Semigroup[A]): Semigroup[A] = instance
    //def apply[A: Semigroup]: Semigroup[A] = implicitly
  }

  object syntax {

    implicit class SemigroupOps[A: Semigroup](x: A) {
      def combine(y: A): A = {
        Semigroup[A].combine(x, y)
      }
    }

  }

  // 1.2. Implement Semigroup for Long, String

  import syntax._

  implicit val sgLong: Semigroup[Long] = (x, y) => x + y
  implicit val strLong: Semigroup[String] = (x, y) => x + y


  // 1.3. Implement combineAll(list: List[A]) for non-empty lists
  def combineAll[A: Semigroup](ls: List[A]): A = ls.reduce[A]({ case (acc, x) => acc.combine(x) })

  // combineAll(List(1, 2, 3)) == 6

  // 1.4. Implement combineAll(list: List[A], startingElement: A) for all lists
  def combineAll[A: Semigroup](ls: List[A], startingElement: A): A = ls.foldLeft(startingElement)(_.combine(_))

  // combineAll(List(1, 2, 3), 0) == 6
  // combineAll(List(), 1) == 1

  // 2. Monoid
  // 2.1. Implement Monoid which provides `empty` value (like startingElement in previous example) and extends Semigroup
  trait Monoid[A] extends Semigroup[A] {
    def empty: A
  }

  object Monoid {
    def apply[A: Monoid]: Monoid[A] = implicitly
  }

  // 2.2. Implement Monoid for Long, String
  implicit val longMonoid: Monoid[Long] = new Monoid[Long] {
    override def empty: Long = 0

    override def combine(x: Long, y: Long): Long = x + y
  }

  implicit val stringMonoid: Monoid[String] = new Monoid[String] {
    override def empty: String = ""

    override def combine(x: String, y: String): String = x + y
  }

  // 2.3. Implement combineAll(list: List[A]) for all lists
  def combineAll[A: Monoid](list: List[A]): A = list.foldLeft(Monoid[A].empty)(_.combine(_))

  // combineAll(List(1, 2, 3)) == 6

  // 2.4. Implement Monoid for Option[A]
  implicit def monoidOption[A: Semigroup]: Monoid[Option[A]] = new Monoid[Option[A]] {
    override def empty: Option[A] = None

    override def combine(x: Option[A], y: Option[A]): Option[A] = (x, y) match {
      case (Some(xVal), Some(yVal)) => Some(xVal.combine(yVal))
      case (x, y) => x.orElse(y)
    }
  }

  monoidOption[Long].combine(Some(2), None) == Some(2)
  monoidOption[Long].combine(None, Some(2)) == Some(2)

  // combineAll(List(Some(1), None, Some(3))) == Some(4)
  // combineAll(List(None, None)) == None
  // combineAll(List()) == None

  // 2.5. Implement Monoid for Function1 (for result of the function)
  implicit def func1Monoid[A, B: Monoid]: Monoid[A => B] = new Monoid[A => B] {
    override def empty: A => B = _ => Monoid[B].empty

    override def combine(x: A => B, y: A => B): A => B = a => Monoid[B].combine(x(a), y(a))
  }

  // combineAll(List((a: String) => a.length, (a: String) => a.toInt))        === (a: String) => (a.length + a.toInt)
  // combineAll(List((a: String) => a.length, (a: String) => a.toInt))("123") === 126

  // 3. Functor
  trait Functor[F[_]] {
    def fmap[A, B](fa: F[A])(f: A => B): F[B]
  }

  implicit class FunctorOps[F[_] : Functor, A](fa: F[A]) {
    def fmap[B](f: A => B): F[B] = Functor[F].fmap(fa)(f)
  }

  object Functor {
    def apply[F[_] : Functor]: Functor[F] = implicitly[Functor[F]]
  }

  implicit val optionFunctor: Functor[Option] = new Functor[Option] {
    override def fmap[A, B](fa: Option[A])(f: A => B): Option[B] = fa.map(f)
  }

  // 4. Semigroupal
  trait Semigroupal[F[_]] {
    def product[A, B](fa: F[A], fb: F[B]): F[(A, B)]
  }

  object Semigroupal {
    def apply[F[_]](implicit instance: Semigroupal[F]): Semigroupal[F] = instance
    //def apply[F[_]: Semigroupal]: Semigroupal[F] = implicitly
  }

  implicit class SemigroupalOps[F[_] : Semigroupal, A](x: F[A]) {
    def product[B](y: F[B]): F[(A, B)] = Semigroupal[F].product(x, y)
  }

    // 4.1. Implement Semigroupal which provides `product[A, B](fa: F[A], fb: F[B]): F[(A, B)]`,
  // so in combination with Functor we'll be able to call for example `plus` on two Options (its content)

  // 4.2. Implement Semigroupal for Option
  implicit val semigrupalOption: Semigroupal[Option] = new Semigroupal[Option] {
    override def product[A, B](fa: Option[A], fb: Option[B]): Option[(A, B)] = (fa, fb) match {
      case (Some(x), Some(y)) => Some((x, y))
      case _                  => None
    }
  }

  // 4.3. Implement `mapN[R](f: (A, B) => R): F[R]` extension method for Tuple2[F[A], F[B]]
  implicit class Tuple2Ops[F[_]: Functor : Semigroupal, A, B](x: (F[A], F[B])) {
    def mapN[R](f: (A, B) => R): F[R] = x match {
      case (fa, fb) => fa.product(fb).fmap {
        case (a, b) => f(a, b)
      }
    }
  }

   (Option(1), Option(2)).mapN(_ + _) == Some(3)
   //(Option(1), None).mapN(_ + _)      == None

  // 4.4. Implement Semigroupal for Map

  // (Map(1 -> "a", 2 -> "b"), Map(2 -> "c")).mapN(_ + _) == Map(2 -> "bc")

  // 5. Applicative
  trait Applicative[F[_]] extends Semigroupal[F] with Functor[F] {
    def pure[A](x: A): F[A]
  }

  // 5.1. Implement Applicative for Option, Either
  new Applicative[Either[String, *]] {
    override def pure[A](x: A): Either[String, A] = Right(x)

    override def product[A, B](fa: Either[String, A], fb: Either[String, B]): Either[String, (A, B)] = ???

    override def fmap[A, B](fa: Either[String, A])(f: A => B): Either[String, B] = ???
  }

  // 5.2. Implement `traverse` for all Applicatives instead of Option
  def traverse[A, B](as: List[A])(f: A => Option[B]): Option[List[B]] = ???

  // traverse(List(1, 2, 3)) { i =>
  //   Option.when(i % 2 == 1)(i)
  // } == None

  // traverse(List(1, 2, 3)) { i =>
  //   Some(i + 1)
  // } == Some(List(2, 3, 4))

  // 6. Foldable
  // 6.1. Implement Foldable with `foldLeft`

  // 6.2. Implement Foldable for List
  // Note: we can use here foldLeft from standard library

  // 6.3. Implement `traverse` for all Foldables instead of List
}
