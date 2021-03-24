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
  }
}
