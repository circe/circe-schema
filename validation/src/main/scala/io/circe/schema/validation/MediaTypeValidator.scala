package io.circe.schema.validation

import io.circe.parser

trait MediaTypeValidator {
  def isValid(mediaType: String, input: String): Boolean
}

object MediaTypeValidator {
  val default: MediaTypeValidator = new DefaultMediaTypeValidator

  private class DefaultMediaTypeValidator extends MediaTypeValidator {
    final def isValid(mediaType: String, input: String): Boolean = mediaType match {
      case "application/json" => parser.parse(input).isRight
      case _                  => true
    }
  }
}
