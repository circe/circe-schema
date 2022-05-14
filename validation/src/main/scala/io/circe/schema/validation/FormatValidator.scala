package io.circe.schema.validation

import io.circe.schema.Format
import java.net.URI
import java.time.{LocalDate, OffsetDateTime, OffsetTime}
import java.util.regex.Pattern
import scala.util.Try

trait FormatValidator {
  def isValid(format: Format, input: String): Boolean
}

object FormatValidator {
  val default: FormatValidator = new DefaultFormatValidator

  private val emailPattern: Pattern = Pattern.compile(
    """|\A[a-z0-9!#$%&'*+/=?^_‘{|}~-]+(?:\.[a-z0-9!#$%&'*+/=?^_‘{|}~-]+)*
       |@(?:[a-z0-9](?:[a-z0-9-]*[a-z0-9])?\.)+[a-z0-9](?:[a-z0-9-]*[a-z0-9])?\z
       |""".stripMargin.replaceAll("\n", "")
  )

  private class DefaultFormatValidator extends FormatValidator {
    final def isValid(format: Format, input: String): Boolean = format match {
      case Format.DateTime => Try(OffsetDateTime.parse(input)).isSuccess
      case Format.Date     => Try(LocalDate.parse(input)).isSuccess
      case Format.Time     => Try(OffsetTime.parse(input)).isSuccess
      case Format.Uri      => Try(new URI(input)).isSuccess
      case Format.Email    => emailPattern.matcher(input).matches
      case _               => true
    }
  }
}
