package testing

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers.be
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper
import testing.Calculator._

class CalculatorSpec extends AnyFreeSpec {
  "Calculator" - {

    "enters the number correctly" in {
      val calc = Calculator()

      for (i <- 0 to 9) {
        calc.press(i).screen should be (i)
      }

      calc.press(5).press(1).press(6).screen should be (516)
    }

    "does nothing" - {
      "when you just repeat pressing `=`" in {
        val calc = Calculator().press(Operation.Equal).press(Operation.Equal).press(Operation.Equal)
        calc.screen should be (0)
      }
    }

    "adds numbers correctly" in {
      val calc = Calculator()
        .press(1).press(2).press(0)
        .press(Operation.Plus)
        .press(3).press(9).press(2)
        .press(Operation.Equal)

      calc.screen should be (512)
    }


    "subtracts numbers correctly" in {
      val calc = Calculator()
        .press(9).press(8).press(6)
        .press(Operation.Minus)
        .press(3).press(9).press(2)
        .press(Operation.Equal)

      calc.screen should be (594)
    }

    "multiplies numbers correctly" in {
      val calc = Calculator()
        .press(2).press(4).press(6)
        .press(Operation.Multiply)
        .press(1).press(4)
        .press(Operation.Equal)

      calc.screen should be (3444)
    }

    "divides numbers correctly" in {
      val calc = Calculator()
        .press(2).press(4).press(2)
        .press(Operation.Divide)
        .press(2)
        .press(Operation.Equal)

      calc.screen should be (121)
    }

    "execute multiple operations correctly" in {
      val calc = Calculator()
        .press(2).press(3)
        .press(Operation.Minus)
        .press(1).press(3)
        .press(Operation.Plus)
        .press(7).press(5)
        .press(Operation.Plus)
        .press(2).press(5)
        .press(Operation.Minus)
        .press(5)
        .press(Operation.Multiply)
        .press(4)
        .press(Operation.Divide)
        .press(2)
        .press(Operation.Equal)

      calc.screen should be (210)
    }
  }

  private implicit def int2Digit(digit: Int): Digit = digit match {
    case 0 => Digit._0
    case 1 => Digit._1
    case 2 => Digit._2
    case 3 => Digit._3
    case 4 => Digit._4
    case 5 => Digit._5
    case 6 => Digit._6
    case 7 => Digit._7
    case 8 => Digit._8
    case 9 => Digit._9
  }

}
