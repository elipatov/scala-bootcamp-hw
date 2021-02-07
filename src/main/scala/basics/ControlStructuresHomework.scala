package basics

import scala.io.Source
import scala.util.Try
import cats.implicits._

object ControlStructuresHomework {

  // Homework

  // Create a command line application that reads various "commands" from the
  // stdin, evaluates them, and writes output to stdout.

  // Commands are:

  //   divide 4 5
  // which should output "4 divided by 5 is 0.8"

  //   sum 5 5 6 8.5
  // which should output "the sum of 5 5 6 8.5 is 24.5"

  //   average 4 3 8.5 4
  // which should output "the average of 4 3 8.5 4 is 4.875"

  //   min 4 -3 -17
  // which should output "the minimum of 4 -3 -17 is -17"

  //   max 4 -3 -17
  // which should output "the maximum of 4 -3 -17 is 4"

  // In case of commands that cannot be parsed or calculations that cannot be performed,
  // output a single line starting with "Error: "

  sealed trait Command

  object Command {
    final case class Divide(dividend: Double, divisor: Double) extends Command
    final case class Sum(numbers: List[Double]) extends Command
    final case class Average(numbers: List[Double]) extends Command
    final case class Min(numbers: List[Double]) extends Command
    final case class Max(numbers: List[Double]) extends Command
  }

  final case class ErrorMessage(value: String)

  sealed trait Result
  final case class DivideResult(dividend: Double, divisor: Double, result: Double) extends Result
  final case class SumResult(numbers: List[Double], result: Double) extends Result
  final case class AverageResult(numbers: List[Double], result: Double) extends Result
  final case class MinResult(numbers: List[Double], result: Double) extends Result
  final case class MaxResult(numbers: List[Double], result: Double) extends Result

  def parseCommand(input: String): Either[ErrorMessage, Command] = {
    val words = input.split("\\s+").toList

    words match {
      case "divide" :: Double(dividend) :: Double(divisor) :: Nil =>
        Right(Command.Divide(dividend, divisor))
      case "sum" :: Numbers(ns: List[Double])     => Right(Command.Sum(ns))
      case "average" :: Numbers(ns: List[Double]) => Right(Command.Average(ns))
      case "min" :: Numbers(ns: List[Double])     => Right(Command.Min(ns))
      case "max" :: Numbers(ns: List[Double])     => Right(Command.Max(ns))
      case cmd :: _                               => Left(ErrorMessage(s"Unrecognized command $cmd"))
      case _                                      => Left(ErrorMessage(s"Invalid input: $input"))
    }
  }

  def calculate(cmd: Command): Either[ErrorMessage, Result] = {
    cmd match {
      case Command.Divide(dividend, divisor) =>
        Try(dividend / divisor).toEither
          .map(x => DivideResult(dividend, divisor, x))
          .leftMap(ex => ErrorMessage(ex.toString))
      case Command.Sum(ns)     => Right(SumResult(ns, ns.sum))
      case Command.Average(ns) => Right(AverageResult(ns, ns.sum / ns.length))
      case Command.Min(ns)     => Right(MinResult(ns, ns.min))
      case Command.Max(ns)     => Right(MaxResult(ns, ns.max))
    }
  }

  def renderResult(res: Result): String = {
    res match {
      case DivideResult(dividend, divisor, result) =>
        s"$dividend divided by $divisor is $result"
      case SumResult(numbers, result) =>
        s"the sum of ${numbers.mkString(" ")} is $result"
      case AverageResult(numbers, result) =>
        s"the average of ${numbers.mkString(" ")} is $result"
      case MinResult(numbers, result) =>
        s"the minimum of ${numbers.mkString(" ")} is $result"
      case MaxResult(numbers, result) =>
        s"the maximum of ${numbers.mkString(" ")} is $result"
    }
  }

  def process(input: String): String = {
    val res = for {
      cmd <- parseCommand(input)
      res <- calculate(cmd)
    } yield renderResult(res)

    res.leftMap(err => s"Error: ${err.value}").merge
  }

  // This `main` method reads lines from stdin, passes each to `process` and outputs the return value to stdout
  def main(args: Array[String]): Unit =
    Source.stdin.getLines() map process foreach println

  object Double {
    def unapply(s: String): Option[Double] = util.Try(s.toDouble).toOption
  }

  object Numbers {
    def unapply(s: List[String]): Option[List[Double]] = {
      val opts = s.map(x => util.Try(x.toDouble).toOption)
      if (opts.forall(_.nonEmpty)) Some(opts.map(o => o.get)) else None
    }
  }

}
