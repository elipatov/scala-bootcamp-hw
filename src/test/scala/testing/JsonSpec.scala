package testing

import org.scalacheck.{Arbitrary, Gen, Prop}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import org.scalacheck.Test.Parameters
import cats.Applicative
import cats.implicits._
import cats.syntax.all


class JsonSpec extends AnyFlatSpec with ScalaCheckDrivenPropertyChecks with Matchers {
  import Json._

  implicit val params = Parameters.default.withMinSuccessfulTests(1000)
  val maxDepth = 7

  private sealed trait JType
  private object JType {
    case object JNull    extends JType
    case object JBoolean extends JType
    case object JNumber  extends JType
    case object JString  extends JType
    case object JArray   extends JType
    case object JObject  extends JType
  }

  def jsonGen(depth: Int): Gen[Json] = {
    typeGen.map {
        case JType.JNull    => Gen.oneOf[Json](Array(JNull))
        case JType.JBoolean => Gen.oneOf(true, false).map(JBoolean(_))
        case JType.JNumber  => Gen.double.map(JNumber(_))
        case JType.JString  => Gen.alphaNumStr.map(JString(_))
        case JType.JArray   => jArrayGen(depth + 1)
        case JType.JObject  => jObjectGen(depth + 1)
      }
      .flatMap(x => x)
  }

  private def typeGen: Gen[JType] =
    Gen.oneOf[JType](JType.JNull, JType.JBoolean, JType.JNumber, JType.JString, JType.JObject, JType.JArray)

  def jArrayGen(depth: Int): Gen[JArray] = {
    val length = if (depth < maxDepth) 3 else 0
    typeGen.map {
        case JType.JNull    => Gen.containerOf[Vector, Json](Gen.oneOf(List(JNull))).map(JArray(_))
        case JType.JBoolean => Gen.containerOf[Vector, Boolean](Gen.oneOf(true, false)).map(xs => JArray(xs.map(JBoolean(_))))
        case JType.JNumber  => Gen.containerOf[Vector, Double](Gen.double).map(xs => JArray(xs.map(JNumber(_))))
        case JType.JString  => Gen.containerOf[Vector, String](Gen.alphaNumStr).map(xs => JArray(xs.map(JString(_))))
        case JType.JObject  => Gen.containerOf[Vector, JObject](jObjectGen(depth + 1)).map(JArray(_))
        case JType.JArray   => Gen.containerOfN[Vector, JArray](length, jArrayGen(depth + 1)).map(JArray(_))
      }.flatMap(x => x)

  }

  def jObjectGen(depth: Int): Gen[JObject] = {
    implicit val applicativeGen = new Applicative[Gen] {
      override def pure[A](x: A): Gen[A] = Gen.oneOf(List(x))
      override def ap[A, B](ff: Gen[A => B])(fa: Gen[A]): Gen[B] = fa.flatMap(a => ff.map(fn => fn(a)))
    }

    Gen.sized { size =>
      val iterate = if (depth < maxDepth) 0 to size else Seq.empty
      iterate.map(_ =>
        for {
          k <- Gen.alphaNumStr
          v <- jsonGen(depth + 1)
        } yield k -> v
      ).toList.sequence.map(xs => JObject(xs.toMap))
    }
  }

  "parse" should "invert print" in {
    forAll(jsonGen(0)) { json =>
      parse(print(json)) == json
    }
  }
}
