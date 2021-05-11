package testing

import Calculator._
import testing.Calculator.Operation.Equal

/** Simple calculator with buttons.
  *
  * @param memory whatever is stored in the memory.
  * @param screen whatever you see on the screen.
  */
case class Calculator(
    private val memory: Int = 0,
    private val operation: Option[Operation] = None,
    screen: Int = 0
) {
  def press(btn: Button): Calculator =
    btn match {
      case Digit(digit)   => enterDigit(digit)
      case Equal => calculate()
      case Operation(op)  => {
        val calc = calculate()
        calc.copy(memory = calc.screen, screen = 0, operation = Some(op))
      }
    }

  private def enterDigit(digit: Int): Calculator = this.copy(screen = screen * 10 + digit)

  private def calculate(): Calculator = {
    val result = operation match {
      case Some(Operation.Plus)     => memory + screen
      case Some(Operation.Minus)    => memory - screen
      case Some(Operation.Multiply) => memory * screen
      case Some(Operation.Divide)   => memory / screen
      case None                     => screen
    }

    Calculator(memory = memory, screen = result)
  }
}

object Calculator {
  sealed trait Button
  abstract sealed class Digit(val digit: Int) extends Button
  object Digit {
    def unapply(x: Digit): Option[Int] = Some(x.digit)
    final case object _0 extends Digit(0)
    final case object _1 extends Digit(1)
    final case object _2 extends Digit(2)
    final case object _3 extends Digit(3)
    final case object _4 extends Digit(4)
    final case object _5 extends Digit(5)
    final case object _6 extends Digit(6)
    final case object _7 extends Digit(7)
    final case object _8 extends Digit(8)
    final case object _9 extends Digit(9)
  }

  sealed trait Operation extends Button
  object Operation {
    def unapply(op: Operation): Option[Operation] = Some(op)
    final case object Plus     extends Operation
    final case object Minus    extends Operation
    final case object Multiply extends Operation
    final case object Divide   extends Operation
    final case object Equal    extends Button
  }
}
