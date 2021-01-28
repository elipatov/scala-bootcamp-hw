package basics

import Basics._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

class BasicsTest extends AnyFlatSpec {
  "gcd" should "calculate greatest common divisor" in {
    gcd(8, 12) shouldEqual 4
    gcd(7, 3) shouldEqual 1
    gcd(3, 12) shouldEqual 3
    gcd(42, 17) shouldEqual 1
    gcd(20, 5) shouldEqual 5
    gcd(3, 30) shouldEqual 3
    gcd(33, 0) shouldEqual 33
    gcd(0, 13) shouldEqual 13
    gcd(0, 0) shouldEqual 0
    gcd(-20, 4) shouldEqual 4
    gcd(20, -4) shouldEqual 4
    gcd(-4, -20) shouldEqual 4
  }

  "lcm" should "calculate least common multiple" in {
    lcm(4, 6) shouldEqual 12
    lcm(7, 3) shouldEqual 21
    lcm(3, 4) shouldEqual 12
    lcm(12, 24) shouldEqual 24
    lcm(13, 17) shouldEqual 221
    lcm(33, 6) shouldEqual 66
    lcm(4, 13) shouldEqual 52
    lcm(4, -2) shouldEqual 4
    lcm(-4, -8) shouldEqual 8
    lcm(-9, 18) shouldEqual 18
  }
}
