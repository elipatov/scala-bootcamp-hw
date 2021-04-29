package cats

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class CatsExercises extends AnyFreeSpec with Matchers{
  "cats" - {
    "Semigroup" in {
      Semigroup[Int].combine(1, 2) should be(3)
      Semigroup[List[Int]].combine(List(1, 2, 3), List(4, 5, 6)) should be(List(1, 2, 3, 4, 5, 6))
      Semigroup[Option[Int]].combine(Option(1), Option(2)) should be(Option(3))
      Semigroup[Option[Int]].combine(Option(1), None) should be(Option(1))
      Semigroup[Int => Int].combine(_ + 1, _ * 10).apply(6) should be(67)

      val aMap = Map("foo" -> Map("bar" -> 5))
      val anotherMap = Map("foo" -> Map("bar" -> 6))
      val combinedMap = Semigroup[Map[String, Map[String, Int]]].combine(aMap, anotherMap)
      combinedMap.get("foo") should be(Some(Map("bar" -> 11)))
    }

    "Monoid" in {
      Monoid[Map[String, Int]].combineAll(List(Map("a" -> 1, "b" -> 2), Map("a" -> 3))) should be(
        Map("a" -> 4, "b" -> 2)
      )

      Monoid[Map[String, Int]].combineAll(List()) should be(Map[String, Int]())
    }

    "Apply" in {
      val intToString = (x: Int) => x.toString
      val double = (x: Int) => x * 2

      Apply[Option].ap(Some(intToString))(Some(1)) should be(Some("1"))
      Apply[Option].ap(Some(double))(Some(1)) should be(Some(2))
      Apply[Option].ap(Some(double))(None) should be(None)
      Apply[Option].ap(None)(Some(1)) should be(None)
      Apply[Option].ap(None)(None) should be(None)

      val addArity2 = (a: Int, b: Int) => a + b
      Apply[Option].ap2(Some(addArity2))(Some(1), Some(2)) should be(Some(3))
      Apply[Option].ap2(Some(addArity2))(Some(1), None) should be(None)

      val addArity3 = (a: Int, b: Int, c: Int) => a + b + c
      Apply[Option].ap3(Some(addArity3))(Some(1), Some(2), Some(3)) should be(Some(6))

      import cats.implicits._
      val option2 = (Option(1), Option(2))
      val option3 = (option2._1, option2._2, Option.empty[Int])

      option2 mapN addArity2 should be(Some(3))
      option3 mapN addArity3 should be(None)

      option2 apWith Some(addArity2) should be(Some(3))
      option3 apWith Some(addArity3) should be(None)

      option2.tupled should be(Some((1,2)))
      option3.tupled should be(None)
    }

    "Applicative" in {
      val t = (Applicative[List] compose Applicative[Option]).pure(1)
      (Applicative[List] compose Applicative[Option]).pure(1) should be(List(Some(1)))
    }

  }
}
