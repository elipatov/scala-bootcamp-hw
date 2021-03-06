package error_handling

import cats.implicits.catsSyntaxValidatedIdBinCompat0
import cats.data.ValidatedNec
import cats.syntax.all._

import java.time.YearMonth
import scala.util.Try

// Homework. Place the solution under `error_handling` package in your homework repository.
//
// 1. Model `PaymentCard` class as an ADT (protect against invalid data as much as it makes sense).
// 2. Add `ValidationError` cases (at least 5, may be more).
// 3. Implement `validate` method to construct `PaymentCard` instance from the supplied raw data.
object Homework {
  case class PaymentCard(holderName: String, number: String, expirationDate: YearMonth, securityCode: Int)


  sealed trait ValidationError

  object ValidationError {
    final case object HolderNameIsRequired extends ValidationError {
      override def toString: String = "Holder name is required"
    }

    final case object InvalidHolderName extends ValidationError {
      override def toString: String = "Holder name should contain only alphabetic characters."
    }

    final case object InvalidCardNumber extends ValidationError {
      override def toString: String = "Card number is 16 digits printed on your card. Expected format: XXXX XXXX XXXX XXXX"
    }

    final case object InvalidExpirationDate extends ValidationError {
      override def toString: String = "Enter expiration date in format mm/yyyy"
    }

    final case object ExpiredCard extends ValidationError {
      override def toString: String = "Credit card is expired"
    }

    final case object InvalidSecurityCode extends ValidationError {
      override def toString: String = "Security Code is 3 or 4 digits printed on back side of your card"
    }
  }
  object PaymentCardValidator {
    type AllErrorsOr[A] = ValidatedNec[ValidationError, A]

    def validate(
                  name: String,
                  number: String,
                  expirationDate: String,
                  securityCode: String,
                ): AllErrorsOr[PaymentCard] = {
      (
        validateHolderName(name),
        validateCardNumber(number),
        validateExpirationDate(expirationDate),
        validateSecurityCode(securityCode)).mapN(PaymentCard)
    }

    private def validateHolderName(holderName: String): AllErrorsOr[String] = {
      if (holderName.length == 0) {
        ValidationError.HolderNameIsRequired.invalidNec
      } else if (!holderName.matches("^(([A-Za-z]+ ?){1,3})$")) {
        ValidationError.InvalidHolderName.invalidNec
      } else {
        holderName.validNec
      }
    }

    private def validateCardNumber(number: String): AllErrorsOr[String] = {
      if (number.matches("^\\d{4} \\d{4} \\d{4} \\d{4}$")) {
        number.validNec
      } else {
        ValidationError.InvalidCardNumber.invalidNec
      }
    }

    private def validateExpirationDate(expDate: String): AllErrorsOr[YearMonth] = {
      val date = raw"(\d{2})/(\d{4})".r
      expDate match {
        case date(month, year) => {
          Try(YearMonth.of(year.toInt, month.toInt)).toOption
            .map(d => {
              if (d.isAfter(YearMonth.now())) d.validNec else ValidationError.ExpiredCard.invalidNec
            }).getOrElse(ValidationError.InvalidExpirationDate.invalidNec)
        }
        case _ => ValidationError.InvalidExpirationDate.invalidNec
      }
    }

    private def validateSecurityCode(code: String): AllErrorsOr[Int] = {
      if (code.matches("^\\d{3,4}$")) {
        code.toInt.validNec
      } else {
        ValidationError.InvalidSecurityCode.invalidNec
      }
    }
  }
}