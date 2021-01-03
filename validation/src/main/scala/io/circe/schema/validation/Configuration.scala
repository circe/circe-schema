package io.circe.schema.validation

case class Configuration(multipleOfBigDecimal: Boolean)

object Configuration {
  val default: Configuration = Configuration(false)
}
