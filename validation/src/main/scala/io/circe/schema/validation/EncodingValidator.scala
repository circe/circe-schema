package io.circe.schema.validation

import io.circe.schema.Encoding
import java.util.Base64
import scala.util.Try

trait EncodingValidator {
  def isValid(encoding: Encoding, input: String): Boolean
  def decode(encoding: Encoding, input: String): Option[String]
}

object EncodingValidator {
  val default: EncodingValidator = new DefaultEncodingValidator()

  private class DefaultEncodingValidator extends EncodingValidator {
    final def isValid(encoding: Encoding, input: String): Boolean = encoding match {
      case Encoding.Base64 => Try(Base64.getDecoder().decode(input)).isSuccess
      case _               => true
    }

    final def decode(encoding: Encoding, input: String): Option[String] = encoding match {
      case Encoding.Base64 => Try(Base64.getDecoder().decode(input)).toOption.map(new String(_))
      case _               => Some(input)
    }
  }
}
